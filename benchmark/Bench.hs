module Main where

import Criterion.Main

import Math.NumberTheory.ArithmeticFunctions as A
import Math.NumberTheory.Primes.Factorisation as F

compareFunctions :: String -> (Integer -> Integer) -> (Integer -> Integer) -> Benchmark
compareFunctions name old new = bgroup name
  [ bench "old" $ nf (map old) [1..100000]
  , bench "new" $ nf (map new) [1..100000]
  ]

main = defaultMain
  [ compareFunctions "totient" F.totient A.totient
  , compareFunctions "carmichael" F.carmichael A.carmichael
  , compareFunctions "moebius" F.moebius A.moebius
  , compareFunctions "tau" F.tau A.tau
  , compareFunctions "sigma 1" (F.sigma 1) (A.sigma 1)
  , compareFunctions "sigma 2" (F.sigma 2) (A.sigma 2)
  ]
