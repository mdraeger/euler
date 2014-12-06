module Main where

import ElementaryNumberTheory

problem45 = aux hex pent
   where
      aux (h:hs) ps = if h == head ps
                      then h
                      else aux hs (dropWhile (< head hs) ps)
      aux _ _ = 0
      pent = take 70000 $ dropWhile (< head hex)  pentagonals
      hex = take 50000 $ drop 144 hexagonals

main = print problem45
