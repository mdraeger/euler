module FibonacciHeap where

import Test.QuickCheck
import qualified Data.List as L

data BiTree a = Node { rank :: Int
                     , root :: a
                     , children :: [BiTree a]} deriving (Eq, Show)

data FibHeap a = E | FH { size :: Int
                        , minTree :: BiTree a
                        , trees :: [BiTree a]} deriving (Eq, Show)

singleton :: a -> FibHeap a
singleton x = FH 1 (Node 1 x []) []

isEmpty :: FibHeap a -> Bool
isEmpty E = True
isEmpty _ = False

link :: (Ord a) => BiTree a -> BiTree a -> BiTree a
link t1@(Node r x c1) t2@(Node _ y c2)
    | x < y     = Node (r+1) x (t2:c1)
    | otherwise = Node (r+1) y (t1:c2)

insert :: (Ord a) => FibHeap a -> a -> FibHeap a
insert h x = merge h (singleton x)

merge :: (Ord a) => FibHeap a -> FibHeap a -> FibHeap a
merge h E = h
merge E h = h
merge h1@(FH sz1 minTr1 ts1) h2@(FH sz2 minTr2 ts2)
    | root minTr1 < root minTr2 = FH (sz1+sz2) minTr1 (minTr2:ts2++ts1)
    | otherwise                 = FH (sz1+sz2) minTr2 (minTr1:ts1++ts2)

findMin :: (Ord a) => FibHeap a -> a
findMin = root.minTree

consolidate :: (Ord a) => [BiTree a] -> [BiTree a]
consolidate = foldl meld [] 
    where
        meld [] t = [t]
        meld (t':ts) t 
            | rank t == rank t' = meld ts (link t t')
            | rank t <  rank t' = t:t':ts
            | otherwise = t' : meld ts t

extractMin :: (Ord a) => [BiTree a] -> (BiTree a, [BiTree a])
extractMin [t]    = (t, [])
extractMin (t:ts) = if root t < root t' 
                        then (t,ts)
                        else (t', t:ts')
    where 
        (t', ts') = extractMin ts

deleteMin :: (Ord a) => FibHeap a -> FibHeap a
deleteMin (FH _ (Node _ x []) []) = E
deleteMin h@(FH sz minTr ts) = FH (sz-1) minTr' ts' 
    where
        (minTr', ts') = extractMin $ consolidate (children minTr ++ ts)

fromList :: (Ord a) => [a] -> FibHeap a
fromList = foldl insert E

heapSort :: (Ord a) => [a] -> [a]
heapSort = hsort .fromList 
    where
        hsort E = []
        hsort h = (findMin h):(hsort $ deleteMin h)

-- test
prop_sort :: [Int] -> Bool
prop_sort xs = heapSort xs == L.sort xs
