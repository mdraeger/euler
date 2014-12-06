module ElementaryNumberTheory where

import Data.Array.Unboxed

primesSA :: [Int]
primesSA = 2 : prs
    where 
        prs = 3 : sieve prs 3 []
        sieve (p:ps) x fs = [i*2 + x | (i, True) <- assocs a] 
                            ++ sieve ps (p*p) ((p,0):[(s, rem (y-q) s) | (s,y) <- fs])
            where
                q = (p*p-x) `div` 2
                a :: UArray Int Bool
                a = accumArray (\b c -> False) True (1,q-1) [(i,()) | (s,y) <- fs, 
                                                                      i <- [y+s, y+s+s .. q]]

triangles :: [Int]
triangles = t 1 1
   where
      t n s = s : t (n+1) (s + n + 1)

pentagonals :: [Int]
pentagonals = p 1 1
   where 
      p n s = s : p (n+3) (s + n + 3)

hexagonals :: [Int]
hexagonals = h 1
   where 
      h n = (n * (2 * n - 1)) : h (n+1)

isPentagonal n = elem n ps
   where 
      ps = takeWhile ( <= n ) pentagonals

fib :: [Int]
fib = f 1 1
   where 
      f a b = a : f b (a+b)
