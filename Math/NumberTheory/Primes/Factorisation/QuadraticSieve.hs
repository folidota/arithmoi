{-# LANGUAGE CPP #-}

module Math.NumberTheory.Primes.Factorisation.QuadraticSieve
  ( quadraticSieve
  , findPairs
  ) where

#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup
#endif
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.IntMap as I
import qualified Data.IntSet as S
import qualified Math.NumberTheory.Primes.IntSet as PS
import Control.Monad
import Control.Monad.ST
import Data.Maybe
import Data.Bifunctor
import Math.NumberTheory.Roots
import Math.NumberTheory.Primes
import Math.NumberTheory.Moduli.Sqrt
import Math.NumberTheory.Utils.FromIntegral

data SignedPrimeIntSet = SignedPrimeIntSet
  { sign :: !Bool
  , primeSet :: !PS.PrimeIntSet
  } deriving (Show)

data BoolOrPrime = Bool Bool | PrimeInt (Prime Int)

insert :: Prime Int -> SignedPrimeIntSet -> SignedPrimeIntSet
insert prime (SignedPrimeIntSet s ps) = SignedPrimeIntSet s (prime `PS.insert` ps)

nonZero :: SignedPrimeIntSet -> Maybe BoolOrPrime
nonZero (SignedPrimeIntSet s ps) = case PS.minView ps of
  Just (prime, _)  -> Just (PrimeInt prime)
  Nothing          -> if s then Just (Bool True) else Nothing

member :: BoolOrPrime -> SignedPrimeIntSet -> Bool
member value (SignedPrimeIntSet s ps) = case value of
  Bool b     -> s == b
  PrimeInt p -> p `PS.member` ps

xor :: SignedPrimeIntSet -> SignedPrimeIntSet -> SignedPrimeIntSet
xor (SignedPrimeIntSet s1 ps1) (SignedPrimeIntSet s2 ps2) = SignedPrimeIntSet (s1 /= s2) ((ps1 PS.\\ PS.unPrimeIntSet ps2) <> (ps2 PS.\\ PS.unPrimeIntSet ps1))

-- Given an odd positive composite Integer n and Int parametres b and t,
-- the Quadratic Sieve attempt to decompose n into smaller factors p and q.
quadraticSieve :: Integer -> Int -> Int -> Integer
quadraticSieve n b t = findFactor n $ findPairs n b t

findFactor :: Integer -> [(Integer, Integer)] -> Integer
findFactor _ [] = error "Parametres are not large enough."
findFactor n ((x, y) : otherPairs) = if factor == 1 || factor == n then findFactor n otherPairs else factor
  where
    factor = gcd (x - y) n

findPairs :: Integer -> Int -> Int -> [(Integer, Integer)]
findPairs n b t = runST $ do
  let
    factorBase = [nextPrime 2..precPrime b]
    squareRoot = integerSquareRoot n
    sievingFunction j = j * j - n
    startingPoint = squareRoot - intToInteger t `div` 2
    sievingInterval = generateInterval sievingFunction startingPoint t
  sievingIntervalM <- V.thaw sievingInterval
  -- Perform sieving
  smoothSieveM sievingIntervalM factorBase n startingPoint
  sievingIntervalF <- V.unsafeFreeze sievingIntervalM
  -- Filter smooth numbers
  let
    indexedFactorisations = V.toList (findSmoothNumbers sievingIntervalF)
    solutionBasis = gaussianElimination indexedFactorisations
    unsignedFactorisations = map (second primeSet) indexedFactorisations

  pure $ map (\sol -> (findFirstSquare n startingPoint sol, findSecondSquare n unsignedFactorisations sol)) solutionBasis

-- Generating sieving interval. This consists of tuples whose first
-- component is x^2 - n as x runs from the square root of n for a
-- total of length t. The second component stores the factorisation
-- modulo 2 as an IntSet.
generateInterval :: (Integer -> Integer) -> Integer -> Int -> V.Vector (Integer, SignedPrimeIntSet)
generateInterval f startingPoint dim = V.map (\x -> (x, isNegative x)) vectorOfValues
  where
    vectorOfValues = V.generate dim (\i -> f (intToInteger i + startingPoint))
    isNegative j = SignedPrimeIntSet (j < 0) mempty

-- This algorithm takes the sievingInterval and the factorBase. It divides by
-- all the primes in the factor base storing the factorisations. The smooth
-- numbers correspond to tuples whose first component is 1. The second component
-- is their factorisation.
smoothSieveM :: MV.MVector s (Integer, SignedPrimeIntSet) -> [Prime Int] -> Integer -> Integer -> ST s ()
smoothSieveM sievingIntervalM factorBase n startingPoint = do
  let t = MV.length sievingIntervalM
  forM_ factorBase $ \prime -> do
    let modularSquareRoots = sqrtsModPrime n ((fromJust . toPrimeIntegral) prime)
    forM_ modularSquareRoots $ \modularSquareRoot -> do
      let startingIndex = integerToInt ((modularSquareRoot - startingPoint) `mod` (intToInteger . unPrime) prime)
      forM_ [startingIndex, startingIndex + unPrime prime..(t - 1)] $ \entry -> do
        let change (y, set) = (y `div` (intToInteger . unPrime) prime, prime `insert` set)
        MV.modify sievingIntervalM change entry

-- This function returns the smooth numbers together with their index. This
-- is in order to retrieve later the value of x and x^2 - n. The value stored
-- in the first component of the tuple is a set whose only component is
-- the index of column before sieving.
findSmoothNumbers :: V.Vector (Integer, SignedPrimeIntSet) -> V.Vector (S.IntSet, SignedPrimeIntSet)
findSmoothNumbers = V.imapMaybe selectSmooth
  where
    selectSmooth index (residue, factorisation)
      | residue == 1 = Just (S.singleton index, factorisation)
      | otherwise    = Nothing

-- This solves the linear equation. It returns a basis for the kernel
-- of the matrix as a list of IntSet.
gaussianElimination :: [(S.IntSet, SignedPrimeIntSet)] -> [S.IntSet]
gaussianElimination [] = []
gaussianElimination (p@(indices ,pivotFact) : xs) = case nonZero pivotFact of
  Just pivot -> gaussianElimination (map (\q@(_, fact) -> if pivot `member` fact then add p q else q) xs)
  Nothing    -> indices : gaussianElimination xs
  where
    add (a, u) (b, v) = ((a S.\\ b) <> (b S.\\ a), xor u v)

-- Given a solution, the value of x^2 - n is computed again. By contruction,
-- the solution IntSet consists of values which correspond to columns in the
-- original sieving interval (before sieving).
findFirstSquare ::Integer -> Integer -> S.IntSet -> Integer
findFirstSquare n startingPoint = S.foldr construct 1
  where
    construct index previous = ((intToInteger index + startingPoint) * previous) `mod` n

-- Finds the factorisations corresponding to the selected solutions and computes
-- the total number of times a given prime occurs in the selected factorisations.
-- By construction, for any given prime, this number is even. From here, a
-- square root is computed.
findSecondSquare :: Integer -> [(S.IntSet, PS.PrimeIntSet)] -> S.IntSet -> Integer
findSecondSquare n indexedFactorisations solution = I.foldrWithKey computeRoot 1 countPowers
  where
    computeRoot key power previous = (intToInteger key ^ (power `div` 2 :: Int) * previous) `mod` n
    countPowers = foldl count I.empty squares
    count = PS.foldr (\prime im -> I.insertWith (+) (unPrime prime) (1 :: Int) im)
    squares = fmap snd (filter (\(index, _) -> index `S.isSubsetOf` solution) indexedFactorisations)