module Main where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.SP
import Text.Printf

strToList :: String -> [[Double]]
strToList s = map (\list -> map (\string -> (read string)::Double) list) (map splitByComma (lines s))

splitByComma = splitBy ','

splitBy delimiter = foldr f [[]]
    where 
        f c l@(x:xs) 
            | c == delimiter = []:l
            | otherwise      = (c:x):xs

-- this is only valid for a very special matrix as 
-- used in project Euler problem 81
fromCostMatrix :: [[Double]] -> Gr Int Double
fromCostMatrix matrix = mkGraph lNodes (fromCostMatrix' matrix 1 [])
    where
        fromCostMatrix' :: [[Double]] -> Int -> [LEdge Double] -> [LEdge Double]
        fromCostMatrix' ((y:ys):xs) n arcs = fromCostMatrix' (ys:xs) (n+1) (newArcs n y arcs)
        fromCostMatrix' ([]:xs) n arcs = fromCostMatrix' xs (n)  arcs
        fromCostMatrix' [] _ arcs = arcs
        newArcs h c arcs 
            | h <= total = ((h-1),h,c):arcs
            | h `mod` total == 1 = ((h-total),h,c):arcs
            | otherwise = ((h-total),h,c):((h-1),h,c):arcs
        total = length matrix
        lNodes = map (\n-> (n,n)) [0..total*total]


main = do
    matrix <- readFile "matrix.txt"
    let graph = fromCostMatrix $ strToList matrix
    let s = sp 0 6400 graph
    let spl = spLength 0 6400 graph
    printf "shortest path: %s\n" (show s)
    printf "path length: %f\n" spl
