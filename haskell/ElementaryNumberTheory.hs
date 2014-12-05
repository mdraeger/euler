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
