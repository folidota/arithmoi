module Math.NumberTheory.Primes.Sieve.SmoothSieve
  ( smoothSieve
  ) where

import qualified Data.Vector.Unboxed as U
-- To convert between boxed and unboxed

import Math.NumberTheory.Primes
import Math.NumberTheory.ArithmeticFunctions.SieveBlock
-- Finds the number of primes less than or equal to a given number
--import Math.NumberTheory.Primes.Counting.Impl

smoothSieve :: Word -> Word -> U.Vector Word
smoothSieve n b = sieveBlockUnboxed configuration 2 n :: U.Vector Word
    where
        configuration = SieveBlockConfig 1 function (*)
        function p a = case ((unPrime p) <= b) of
            True  -> (unPrime p) ^ a
            False -> 1
