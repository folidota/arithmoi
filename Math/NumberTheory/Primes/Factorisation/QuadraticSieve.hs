-- |
-- Module:      Math.NumberTheory.Primes.Factorisation.QuadraticSieve
-- Copyright:   (c) 2020 Federico Bongiorno
-- Licence:     MIT
-- Maintainer:  Federico Bongiorno <federicobongiorno97@gmail.com>
--
-- <https://en.wikipedia.org/wiki/Quadratic_sieve Quadratic Sieve> algorithm
-- employing multiple polynomials and large prime variation.

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.Primes.Factorisation.QuadraticSieve
  ( QuadraticSieveConfig(..)
  , quadraticSieve
  , quadraticSieveManual
  , autoConfig
  , findSquares
  ) where

#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup
#endif
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.IntMap as I
import qualified Data.IntSet as S
import qualified Data.Vector as V
import qualified Data.Vector.Sized as SV
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as U
import qualified Data.Mod as M
import qualified Data.Mod.Word as MW
import Math.NumberTheory.Utils
import Math.NumberTheory.Roots
import Math.NumberTheory.Primes
import Math.NumberTheory.Logarithms
import Math.NumberTheory.Moduli.Sqrt
import Math.NumberTheory.Utils.FromIntegral
import Math.NumberTheory.Primes.Factorisation.LinearAlgebra
import Math.NumberTheory.Primes.Factorisation.TrialDivision
import Control.Monad
import Control.Monad.ST
import GHC.TypeNats
import Data.Proxy
import Data.Foldable
import Data.Maybe
import Data.Bits
import Data.Bifunctor
import qualified Debug.Trace

trace :: String -> a -> a
trace = if tuning then Debug.Trace.trace else const id

-- This variable can be set to @True@ when tuning the parameters. It prints
-- information about the sieving and solving processes.
tuning :: Bool
tuning = False

-- | This data type is used to configure running ot the quadratic sieve. It
-- comprises four parameters.
-- 1. @qscFactorBase@ controls the size of the factor base. More precisely the
-- factor base consists of all primes up to the given bound which are squares
-- modulo @n@ (the integer to factor).
-- 2. @qscSievingInterval@ controls the length of the sieving interval.
-- The length is twice this number plus one.
-- 3. @qscNumberOfBlocks@ controls the number of sieving blocks to run with a
-- given initialisation. Inputting @0@ results in running the standard quadratic
-- sieve. Inputting a greater number results in running the multiple polynomials
-- variant. In this case, the number of blocks employed given an initialisation
-- is @2 ^ qscNumberOfBlocks - 1@.
-- 4. @qscSmoothThreshold@ is the threshold to select smooth numbers. This is
-- needed since the sieving is approximate.
data QuadraticSieveConfig = QuadraticSieveConfig
  { qscFactorBase :: Int
  , qscSievingInterval :: Int
  , qscNumberOfBlocks :: Int
  , qscSmoothThreshold :: Int
  }

-- | Given an integer @n@ to factor, this routine produces a configuaration
-- to run the quadratic sieve with. Significantly better results may be
-- obtained by tuning the algorithm manually.
autoConfig :: Integer -> QuadraticSieveConfig
autoConfig n = QuadraticSieveConfig t m k h
  where
    h = intLog2 t + 6
    k = max 0 (l `div` 10)
    m = t
    t
      | l < 4    = integerToInt n `div` 2
      | l < 8    = integerToInt $ integerSquareRoot n
      | otherwise = max (41 - l) 1 * floor (exp (sqrt (le * log le) / 2) :: Double)
    -- number of digits of n
    l = integerLog10 n
    le = fromIntegral l * log 10

-- | Given an odd positive composite Integer @n@, @quadraticSieve@ outputs a
-- factor of @n@. The conditions are not checked.
--
-- >>> quadraticSieve 15
-- 3
quadraticSieve :: Integer -> Integer
quadraticSieve n = quadraticSieveManual n $ autoConfig n

-- | In this variant of the algorithm, a manual configuration is given as
-- additional input.
--
-- >>> quadraticSieveManual 15 $ QuadraticSieveConfig 0 0 0 0
-- Exception: Math.NumberTheory.Primes.Factorisation.QuadraticSieve: Parameters are not large enough.
quadraticSieveManual :: Integer -> QuadraticSieveConfig -> Integer
quadraticSieveManual n qsc = findFactor n $ findSquares n qsc

-- This routine attempts to infer a factorisation from a pair of numbers
-- @(x, y)@ such that @(x ^ 2 - y ^ 2) `mod` n = 0@. If it fails, it calls
-- the linear algebra routine again to provide another pair.
findFactor :: Integer -> [(Integer, Integer)] -> Integer
findFactor _ [] = error "Math.NumberTheory.Primes.Factorisation.QuadraticSieve: Parameters are not large enough."
findFactor n ((x, y) : otherSquares)
  | factor /= 1 && factor /= n   = factor
  | otherwise                    = findFactor n otherSquares
  where
    factor = gcd (x - y) n

-- | This routine outputs an infinite list of tuples @(x, y)@ such that
-- @(x ^ 2 - y ^ 2) `mod` n = 0@. A factorisation can be infered from this data
-- in at least a half of the cases. The algorithm employs multiple polynomials
-- with self-initialisation, approximate log sieving and the large prime variation.
-- The algorithm has four steps:
-- 1. Data Initialisation: a factor base and respective roots are computed.
-- 2. Data Collection: sieving is performed to find enough smooth numbers.
-- 3. Data Processing: a linear algebra routine is called to find dependencies.
-- 4. Factor Inference: a factor is inferred from the processed data.
findSquares :: Integer -> QuadraticSieveConfig -> [(Integer, Integer)]
findSquares n (QuadraticSieveConfig t m k h) = runST $ do
  let
    -- 1. In the first part of the algorithm, the data is initialised.
    factorBase = if t < 3 then error "Math.NumberTheory.Primes.Factorisation.QuadraticSieve: Parameters are not large enough."
      else filter (isResidue n) [nextPrime 2..precPrime t]
    -- @a@ is the leading coefficient of the multiple polynomials to be used in
    -- one initialisation. To maximise efficiency, its k^th root needs to be
    -- around the number computed below. This is explained in page 274 of
    -- Crandall and Pomerance's book.
    kRootOfA
      | k <= 0    = 1
      | otherwise = integerRoot (4 * k) ((2 * n) `div` (intToInteger m ^ (2 :: Int)))
    -- The factors of @a@ are chosen as closely as possible to @kRootOfA@.
    factorsOfA = trace ("Size of Factor Base: " ++ show (length factorBase)) $
      map isPrimeInt $ generatePrimes n kRootOfA k
    initialDecompositionOfA = map (,2) factorsOfA
    factorBaseWithSquareRoots = map (\p -> (p, (sqrtsModPrime n . fromJust . toPrimeIntegral) p)) factorBase

    -- 2. In the second part of the algorithm, enough smooth numbers are found.
    goSieving :: M.Map Integer (I.IntMap Int) -> [(Prime Integer, Word)] -> [(Integer, I.IntMap Int)]
    goSieving previousDiffSmoothNumbers decompositionOfA = goSelfInitSieving previousDiffSmoothNumbers $ zip valuesOfB valuesOfC
      where
        -- Initialisation of @a@ and related data.
        a = factorBack decompositionOfA
        -- The @Ratio@ traced below needs to be close to @1@ for maximal efficiency.
        valuesOfB = trace ("Ratio: " ++ show ((fromInteger (a * fromIntegral m) :: Double) / fromInteger (integerSquareRoot (2*n)))) $
          -- Only one root is picked since they produce equivalent data.
          filter (<= a `div` 2) $ sqrtsModFactorisation n decompositionOfA
        valuesOfC = map (\x -> (x * x - n) `div` a) valuesOfB

        -- Multiple polynomials are used with the same leading coefficient.
        goSelfInitSieving :: M.Map Integer (I.IntMap Int) -> [(Integer, Integer)] -> [(Integer, I.IntMap Int)]
        -- If there are no more polynomials, then choose a different @a@.
        goSelfInitSieving previousSmoothNumbers [] = goSieving previousSmoothNumbers nextDecompositionOfA
          where
            nextDecompositionOfA
              -- Change from @k = 0@ to @k = 1@.
              | null decompositionOfA = map ((, 2) . isPrimeInt) (generatePrimes n 1 1)
              -- @k@ remains the same. The smallest prime is dropped and the next suitable prime is picked.
              | otherwise             = (isPrimeInt nextFactor, 2) : L.delete lowestPrime decompositionOfA
              where
                nextFactor = head $ generatePrimes n highestPrime 1
                highestPrime = unPrime . fst . maximum $ decompositionOfA
                lowestPrime = minimum decompositionOfA

        goSelfInitSieving previousSmoothNumbers ((b, c) : otherCoeffs) = runST $ do
          let
            -- @f@ is the polynomial used during sieving.
            f x = a * x * x + 2 * b * x + c
            (sievingInterval, sievingLogInterval) = V.unzip $ generateLogInterval f m
          sievingLogIntervalM <- V.unsafeThaw sievingLogInterval
          smoothLogSieveM sievingLogIntervalM factorBaseWithSquareRoots a b c m
          sievedLogInterval <- V.unsafeFreeze sievingLogIntervalM
          let
            -- This removes duplicates. This is an issue only when trying to factor small numbers.
            newSmoothNumbers = M.fromList $ findLogSmoothNumbers factorBase m h decompositionOfA b $ V.zip sievingInterval sievedLogInterval
            smoothNumbers = previousSmoothNumbers `M.union` newSmoothNumbers
            matrixSmoothNumbers
              -- This traces the number of smooth numbers and primes found in the previous sieving block.
              | trace ("Smooth Numbers: " ++ show (length mat) ++ "\nPrimes: " ++ show numberOfConstraints) False = undefined
              -- Enough smooth numbers are found. Not too many smooth numbers are
              -- taken to ensure the dimension of the matrix is not too large.
              | numberOfConstraints < length mat = take (numberOfConstraints + 3 * (k + 2)) $ M.assocs smoothNumbers
              -- More smooth numbers are needed.
              | otherwise                        = goSelfInitSieving smoothNumbers otherCoeffs
              where
                numberOfConstraints = S.size $ foldMap convertToSet mat
                -- The tracing prints how many numbers satisfy the given threshold and how many of them are actually smoooth.
                mat = trace ("Log filtering: " ++ show (V.length (V.filter (<= h) sievedLogInterval)) ++ " -> " ++ show (M.size newSmoothNumbers)) $
                  M.elems smoothNumbers
          pure matrixSmoothNumbers

    -- Removes columns which could never be part of a solution.
    sievingData = removeRows $ goSieving mempty initialDecompositionOfA
    matrix = translate $ fmap (convertToSet . snd) sievingData

    -- 3. In the third part of the algorithm, the linear algebra routine is called
    goSolving :: Int -> Int -> [(Integer, Integer)]
    goSolving seed counter
      | counter < 5 = firstSquare `seq` secondSquare `seq` (firstSquare, secondSquare) : goSolving (seed + 1) (counter + 1)
      | otherwise   = findSquares n $ QuadraticSieveConfig (t + 50 * (k + 1)) (m + 50 * (k + 1) * (k + 1)) k h
      where
        -- 4. In the final part, the two squares are inferred.
        firstSquare = findFirstSquare n (fmap fst squaresData)
        secondSquare = findSecondSquare n (fmap snd squaresData)
        squaresData = map (sievingData !!) solution
        solution = withSomeKnown (convertToList . linearSolve seed) matrix

  -- Prints the size of the matrix.
  pure $ trace ("Size of Matrix: " ++ show (withSomeKnown intVal matrix)) $
    goSolving (integerToInt n) 0

-- This routine is used to pick primes in the factorisation of @a@.
generatePrimes :: Integer -> Integer -> Int -> [Prime Integer]
generatePrimes n midPoint len
  | len <= 0  = []
  | otherwise = lowerPrimes ++ higherPrimes
  where
    higherPrimes = take (len - length lowerPrimes) $ filter positiveResidue $ generatePrimesForwards $ midPoint + 1
    -- The length of @lowerPrimes@ may not be @len `div` 2@
    lowerPrimes = take (len `div` 2) $ filter positiveResidue $ generatePrimesBackwards midPoint
    positiveResidue p = jacobi n (unPrime p) == One

generatePrimesForwards :: Integer -> [Prime Integer]
generatePrimesForwards from = [nextPrime (max 3 from)..]

generatePrimesBackwards :: Integer -> [Prime Integer]
generatePrimesBackwards to
  -- 2 cannot be a factor of a
  | to <= 2   = []
  | otherwise = precPrime to : generatePrimesBackwards (unPrime (precPrime to) - 1)

-- Checks if the primes picked for the factorisation of @a@ are small enough.
isPrimeInt :: Prime Integer -> Prime Integer
isPrimeInt x = fromJust . toPrimeIntegral $ primeInt
  where
    primeInt = fromMaybe (error "Math.NumberTheory.Primes.Factorisation.QuadraticSieve: Parameters are not large enough.")
      (toPrimeIntegral x :: Maybe (Prime Int))

-- Cheks if a given prime is a square modulo @n@.
isResidue :: Integer -> Prime Int -> Bool
isResidue n p = (unPrime p == 2) || jacobi resN (unPrime p) == One
  where
    resN = integerToInt $ n `mod` (intToInteger . unPrime) p

-- This routine generates the sieving interval. It takes a function @f@ and
-- and an int @m@ controlling its size and returns a vector of tuples. The
-- first component @x@ is @f@ applied to @i@, as @i@ runs from @-m@ to @m@. The
-- second component stores the logarithm base two of the absolute value of @x@.
-- This is used when sieving.
generateLogInterval :: (Integer -> Integer) -> Int -> V.Vector (Integer, Int)
generateLogInterval f m = V.generate (2 * m + 1) go
  where
    go i = x `seq` v `seq` (x, v)
      where
        -- @x == 0@ happens only if n is a square number.
        -- This throws an exception to do with log.
        v = integerLog2 . abs $ x
        x = f $ intToInteger $ i - m

-- This routine takes @sievingIntervalM@, and performs log division by all the
-- primes in the @factorBase@. When, a division occurs, the logarithm of the
-- prime is subtracted from the value in the interval.
smoothLogSieveM :: MV.MVector s Int -> [(Prime Int, [Integer])] -> Integer -> Integer -> Integer -> Int -> ST s ()
smoothLogSieveM sievingIntervalM factorBaseWithSquareRoots a b c m =
  forM_ factorBaseWithSquareRoots $ \(prime, roots) -> case someNatVal (intToNatural (unPrime prime)) of
    SomeNat (Proxy :: Proxy prime) -> do
      let
        startingIndices = case MW.invertMod (fromInteger a :: MW.Mod prime) of
          -- For the primes not dividing @a@.
          Just inverseOfA -> map (\root -> (wordToInt . MW.unMod) (fromIntegral m + fromInteger (- b + root) * inverseOfA :: MW.Mod prime)) roots
          -- When a prime belongs to the factorisation of @a@.
          Nothing         -> case MW.invertMod (fromInteger (2 * b) :: MW.Mod prime) of
            Just inverseOf2B -> [(wordToInt . MW.unMod) (fromIntegral m - fromInteger c * inverseOf2B :: MW.Mod prime)]
            -- This can't mathematically happen. In this case @p@ divides @n@ and its quadratic residue would be @Zero@.
            Nothing          -> error "Math.NumberTheory.Primes.Factorisation.QuadraticSieve: Algorithm incorrect."
      forM_ startingIndices $ \startingIndex -> do
        let change y = y - (intLog2 . unPrime) prime
        forM_ [startingIndex, startingIndex + unPrime prime..(2 * m)] $ \entry ->
          MV.modify sievingIntervalM change entry

-- This routine takes the @sievedInterval@ as input, it first filters for smooth numbers
-- using the given threshold and then checks which of the filtered numbers are smooth
-- by computing their factorisation by trial division. It also adds extra smooth numbers
-- whenever there are numbers which are almost smooth except one large prime.
findLogSmoothNumbers :: [Prime Int] -> Int -> Int -> [(Prime Integer, Word)] -> Integer -> V.Vector (Integer, Int) -> [(Integer, I.IntMap Int)]
findLogSmoothNumbers factorBase m h decompositionOfA b sievedInterval = fromJust <$> filter isJust (map findSquareData processedSieve)
  where
    -- This function maybe returns tuple whose first component is the integer
    -- needed to compute @firstSquare@. Its second component is the factorisation
    -- of the sieved number needed to compute @secondSquare@.
    findSquareData datum@((ind, fac), highFactor)
      -- The number is smooth
      | highFactor <= highestPrime              = Just (complete ind, facMap)
      -- The pivot factorisation is not included in the smooth numbers.
      | Just datum == pivotFactorisation        = Nothing
      -- These are the almost smooth numbers. Note that one multiplies these by
      -- by the @pivotFactorisation@. This ensures that the @largePrime@ always
      -- shows up with even exponent and does not increase @numberOfConstraints@.
      | Just (unPrime highFactor) == largePrime = Just (complete ind * complete pivotIndex, I.unionWith (+) facMap pivotFacMap)
      -- These numbers are not smooth.
      | otherwise                               = Nothing
      where
        facMap = I.unionWith (+) intMapA $ I.fromAscList $ map (bimap integerToInt wordToInt) fac
        pivotFacMap = I.unionWith (+) intMapA $ I.fromAscList $ map (bimap integerToInt wordToInt) pivotFac
        ((pivotIndex, pivotFac), _) = fromJust pivotFactorisation

    complete i = a * intToInteger (i - m) + b
    a = factorBack decompositionOfA
    -- This is the factorisation of @a@
    intMapA = I.fromList $ map (bimap (integerToInt . unPrime) wordToInt) decompositionOfA
    pivotFactorisation = if isJust largePrime
      then L.find ((== fromJust largePrime) . unPrime . snd) processedSieve
        else Nothing
    largePrime = trace ("Large Prime Data: " ++ show largePrimeData) $
      fst <$> largePrimeData
    -- Finds the prime number that occurs the most times.
    largePrimeData = foldr greaterThan1 Nothing $ I.assocs allLargePrimes
    greaterThan1 (p, k) acc = if Just k > max (Just 1) (fmap snd acc) then Just (p, k) else acc
    allLargePrimes = findLargePrimes $ fmap snd processedSieve
    -- Creates a map whose keys are prime numbers and whose values are the times
    -- they occur in the factorisations.
    findLargePrimes :: [Prime Int] -> I.IntMap Int
    findLargePrimes [] = mempty
    findLargePrimes (highestFactor : otherFactors)
      | highestFactor > highestPrime = I.insertWith (+) (unPrime highestFactor) 1 $ findLargePrimes otherFactors
      | otherwise                    = findLargePrimes otherFactors
    -- @factorBase@ is known not to be empty.
    highestPrime = maximum factorBase
    processedSieve :: [((Int, [(Integer, Word)]), Prime Int)]
    processedSieve = V.toList $ V.imapMaybe filterAndAddHighestPrime sievedInterval
    -- Filters by the threshold and stores the remaining factor in their
    -- factorisations if this prime.
    filterAndAddHighestPrime :: Int -> (Integer, Int) -> Maybe ((Int, [(Integer, Word)]), Prime Int)
    filterAndAddHighestPrime index (value, logResidue)
      | logResidue > h       = Nothing
      | otherwise            = case maybePrime of
        Just prime -> Just ((index, fullFac), prime)
        Nothing    -> Nothing
      where
        -- The maximum in @preFac@ is the number that is left after dividing by
        -- all the primes in @factorBase@. Note that @preFac@ is empty whenever
        -- @value = 1@ hence the need to prepend @1@.
        maybePrime = isPrime =<< (toIntegralSized (maximum (1 : fmap fst preFac)) :: Maybe Int)
        -- Add negative factor.
        fullFac = if value < 0 then (-1, 1) : preFac else preFac
        preFac = trialDivisionWith (map (intToInteger . unPrime) factorBase) value

-- Removes all columns of the matrix which contain primes appearing only once.
-- These columns cannot be part of the solution.
removeRows :: [(Integer, I.IntMap Int)] -> [(Integer, I.IntMap Int)]
removeRows indexedFactorisations
  | onlyOnce == mempty = indexedFactorisations
  | otherwise          = removeRows $ filter (\(_, im) -> S.null (S.intersection (convertToSet im) onlyOnce)) indexedFactorisations
  where
    onlyOnce = appearsOnlyOnce $ map (convertToSet . snd) indexedFactorisations

-- Find all primes, which appear only once in the input list.
appearsOnlyOnce :: [S.IntSet] -> S.IntSet
appearsOnlyOnce = fst . L.foldl' go (mempty, mempty)
  where
    go (onlyOnce, atLeastOnce) x =
      ((onlyOnce S.\\ x) <> (x S.\\ atLeastOnce), atLeastOnce <> x)

convertToSet :: I.IntMap Int -> S.IntSet
convertToSet = I.keysSet . I.filter odd

-- This routine translates the list of smooth factorisations into a matrix.
-- The prime numbers (and -1) are mapped to ints based on their order (-1 -> 0,
-- 2 -> 1, 3 -> 2,...). If a prime (or -1) is missing, then their int values
-- change accordingly (-1 -> 0, 3 -> 1, 17 -> 2,...).
-- This is needed since @linearSolve@ computes powers of a matrix,
-- so the indices of columns and rows must match up.
translate :: [S.IntSet] -> SomeKnown SBMatrix
translate listOfFactorisations = translateHelper listOfFactorisations (length listOfFactorisations)
  where
    translateHelper :: [S.IntSet] -> Int -> SomeKnown SBMatrix
    translateHelper columns dim = case someNatVal (fromIntegral dim) of
      SomeNat (_ :: Proxy dim) -> let result :: SBMatrix dim = SBMatrix (fromJust (SV.fromList (map toIndices columns))) in
        SomeKnown result
          where
            indexedPrimes = U.fromList . S.toAscList $ fold columns
            toIndices :: KnownNat dim => S.IntSet -> SBVector dim
            toIndices x = SBVector $ U.fromList $ map fromIntegral primeTranslation
                  where
                    primeTranslation = binarySearch (S.toAscList x) indexedPrimes

-- When translating, it becomes necessary to see the index of a certain prime.
-- @binarySearch@ does so efficiently.
binarySearch :: (Eq a, Ord a, U.Unbox a) => [a] -> U.Vector a -> [Int]
binarySearch list v = go 0 (len - 1) list v
  where
    len = U.length v
    go :: (Eq a, Ord a, U.Unbox a) => Int -> Int -> [a] -> U.Vector a -> [Int]
    go _ _ [] _ = []
    go lowerIndex upperIndex allItems@(item : otherItems) vector = case item `compare` entry of
      LT -> go lowerIndex (currentIndex - 1) allItems vector
      EQ -> currentIndex : go (currentIndex + 1) (len - 1) otherItems vector
      GT -> go (currentIndex + 1) upperIndex allItems vector
      where
        entry = vector U.! currentIndex
        currentIndex = (upperIndex + lowerIndex) `div` 2

findFirstSquare :: Integer -> [Integer] -> Integer
findFirstSquare n squaresData = case someNatVal (integerToNatural n) of
  SomeNat (Proxy :: Proxy n) ->
    naturalToInteger . M.unMod $ foldr (\x acc -> (fromInteger x :: M.Mod n) * acc) (1 :: M.Mod n) squaresData

findSecondSquare :: Integer -> [I.IntMap Int] -> Integer
findSecondSquare n factorisations = case someNatVal (integerToNatural n) of
  SomeNat (Proxy :: Proxy n) ->
    naturalToInteger . M.unMod $
      -- Division by 2 never has remainder since, by construction, the
      -- factorisation obtained below is that of a square number.
      I.foldrWithKey (\key power acc -> (fromInteger (fromIntegral key) :: M.Mod n) ^ (power `div` 2 :: Int) * acc) (1 :: M.Mod n) $
        I.unionsWith (+) factorisations
