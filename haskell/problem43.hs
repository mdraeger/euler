module Main where

import Text.Printf

allDifferent :: Eq a => [a] -> Bool
allDifferent [] = True
allDifferent (x:xs) = not (x `elem` xs) && allDifferent xs

toNum :: Integral a => [a] -> a
toNum [] = 0
toNum ns = h 0 (reverse ns) 0
    where 
        h :: Integral a => Int -> [a] -> a -> a
        h _ [] result = result
        h exponent (n:ns) result = h (exponent +1) ns (result + n*10^exponent)

-- works only for numbers less than 1000, we need the leading zeros
toList :: Integral a => a -> [a]
toList num = h num 100 []
    where 
        h theNumber theDivisor theList
            | theDivisor == 0 = theList
            | otherwise = h (theNumber `mod` theDivisor) (theDivisor `div` 10) (theList ++ [theNumber `div` theDivisor])

problem43 = map toNum $ substringDivisible [13,11,7,5,3,2,1] allDivisible17
    where 
        allDivisible17 = filter allDifferent $ map toList $ filter (\n -> n `mod` 17 == 0) [10..999]
        substringDivisible [] l = l
        substringDivisible (p:ps) l = 
            substringDivisible ps [(digit: rest)|
                rest <- l,
                digit <- filter (\d -> not (d `elem` rest)) [0..9],
                (digit*100 + (head rest)*10 + (head.tail $ rest)) `mod` p == 0]

main = do
    printf "sum is: %d\n" ((sum problem43)::Integer)
