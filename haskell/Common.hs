module Common where

import qualified Data.List as L

-- useful types
type Point2D = (Double, Double)

-- some functions that can come in handy
primes :: [Int]
primes = 2: 3: sieve (tail primes)  [5,7 ..]
    where 
        sieve (p:ps) xs = h ++ sieve ps [x | x <- t, rem x p /= 0]
            where
                (h, ~(_:t)) = span (< p*p) xs

isPrime :: Int -> Bool
isPrime n = all (\m -> n `mod` m /= 0) [p | p <- takeWhile (upperLimit n)  primes]
    where 
        upperLimit a b = b < a

factorial n 
    | n==0      = 1
    | otherwise = product [1..n]

slope :: Point2D -> Point2D -> Double
slope (x1, y1) (x2, y2) = (y1-y2)/(x1-x2)

isPalindrome :: Show a => a -> Bool
isPalindrome n = (show n) == (reverse.show $ n)

isPandigital :: String -> Bool
isPandigital s = (L.sort s) == "123456789"

binary :: (Integral a, Show a) => a -> String
binary 0 = "0"
binary 1 = "1"
binary n = binary' n ""
    where 
        binary' n s 
            | n == 0 = s
            | otherwise = binary' (n `div` 2) (show (n `mod` 2))++s 

-- might not always deliver a solution (only reals are considered here)
quadraticZeros :: Double -> Double -> Double -> (Double, Double)
quadraticZeros a b c = ((-b + sqrt(d))/(2*a),(-b - sqrt(d))/(2*a))
    where
        d = b^2 - 4*a*c

-- isInteger :: RealFrac a => a -> Bool
isInteger r = r == (fromIntegral.round) r

-- split a string 
splitBy delimiter = foldr f [[]]
    where
        f c l@(x:xs)
            | c == delimiter = []:l
            | otherwise = (c:x):xs

