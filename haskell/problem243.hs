module Main where

import Math.NumberTheory.Primes

problem243 = iterateUntil (\n -> resilience n < (15499/94744)) (product primeFactors) [1..28]
    where 
        primeFactors = take 9 primes
        iterateUntil p n (a:as)
            | p (n*a) = (n*a)
            | as == [] = error "no solution found"
            | otherwise = iterateUntil p n as
        resilience d = fromIntegral (totient d) / fromIntegral (d-1)

main = do
    putStrLn$show problem243
