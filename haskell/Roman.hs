module Roman (fromRoman, toRoman)  where

    import Data.Maybe (fromJust, fromMaybe)

    charToInt :: Char -> Int
    charToInt = fromJust.flip lookup numerals

    numerals = zip "IVXLCDM" [1,5,10,50,100,500,1000]

    fromRoman :: String -> Int
    fromRoman = fst
                    .foldr (\p (t,s) -> if p>= s then (t+p,p) else (t-p,p)) (0,0)
                    .map (charToInt)

    subtractivePairs = [('V','I'), ('X','I'), ('L','X'), ('C','X'), ('D','C'), ('M','C')]

    toRoman :: Int -> String
    toRoman n = (reverse . snd) (foldr toNumeral (n, "") numerals)

    toNumeral st@(rdigit, base) (n,s)
        | n >= base     = toNumeral st (n - base, rdigit:s)
        | n+k >= base   = (n-base+k, rdigit:tdigit:s)
        | otherwise     = (n,s)
        where 
            tdigit = fromMaybe '\0' (lookup rdigit subtractivePairs) 
            k      = fromMaybe 0    (lookup tdigit numerals)
