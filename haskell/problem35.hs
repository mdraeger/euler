module Main where

import Math.NumberTheory.Primes

problem35 = length [a | a<-candidates, isCircularPrime a]
    where   
        isCircularPrime p = all (\n -> n `elem` candidates) (rotations 0 [p])
        rotations n ps 
            | n == numDigits (head ps) -1 = ps
            | otherwise = rotations (n+1) ((nextRotation$head ps):ps) 
        nextRotation n = n `div` 10 + 10^((numDigits n)-1)*(n `mod` 10)
        numDigits n = length$show n
        candidates = filter (\p -> (p > 10 && noSuspiciousDigits p) || p < 10) (takeWhile (<1000000) primes)
        noSuspiciousDigits n = not ( '0' `elem` (show n)
                                  || '2' `elem` (show n)
                                  || '4' `elem` (show n)
                                  || '5' `elem` (show n)
                                  || '6' `elem` (show n)
                                  || '8' `elem` (show n))
 
main = do
    putStrLn $ show problem35
