module Main where

import Common

index = zip ['A'..'Z'] [1..26]
getPosition c = case (lookup c index) of Just n -> n
                                         Nothing -> error ("This should not happen: " ++ [c])
mapWord w = sum $ map getPosition w
isTriangle n = isInteger ((sqrt (1/4 + 2*n)) - 1/2)
wordSums ws = map mapWord (splitBy ',' (filter (/= '\"')  ws))

main = do
    wordList <- readFile "words.txt"
    putStrLn $ show (length $ filter isTriangle (wordSums wordList))

