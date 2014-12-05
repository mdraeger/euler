module Main where
    import Math.NumberTheory.Primes
    import System.Environment
    import Text.Printf
                
    -- http://mathworld.wolfram.com/SumofSquaresFunction
    -- An efficient prime number algorithm is implemented in the arithmoi package.
    --
    upperLimit = 10^11
    smallestSolution = 5^3*13^2*17
    smallestFactor   = 5^3*13^2

    p4k1 = filter (\p -> p `mod` 4 == 1)  (takeWhile (< upperLimit `div` smallestFactor + 1) primes)
    
    possibleFactors = filter (\k -> all (\p -> k `mod` p > 0) (filter (<=k) p4k1)) [1..(upperLimit `div` smallestSolution + 1)]
    
    -- case number one: 105=7*5*3, that is three different prime factors 4k+1
    my_iterate1 l1@(x:xs) l2@(y:ys) l3@(z:zs) s
        | x==y                      = my_iterate1 l1 ys p4k1 s
        | x==z || y==z              = my_iterate1 l1 l2 zs s
        | x^3 > upperLimit          = s
        | x^3 * y^2 > upperLimit    = my_iterate1 xs p4k1 p4k1 s
        | (x^3*y^2*z) <= upperLimit = my_iterate1 l1 l2 zs (s + (newPartialSum x y z))
        | otherwise = my_iterate1 l1 l2 [] s
            where 
                newPartialSum a b c = sum (takeWhile (<=upperLimit) (map (* (a^3*b^2*c)) possibleFactors))
    my_iterate1 l1 (y:ys) [] s = my_iterate1 l1 ys p4k1 s 
    my_iterate1 (x:xs) [] _ s  = my_iterate1 xs p4k1 p4k1 s 
    my_iterate1 [] _ _ s       = s


    -- case number two: 105=15*7, that is two different prime factors 4k+1
    my_iterate2 l1@(x:xs) l2@(y:ys) s 
        | x==y                    = my_iterate2 l1 ys s
        | x^7 > upperLimit        = s
        | (x^7*y^3) <= upperLimit = my_iterate2 l1 ys (s + (newPartialSum x y))
        | otherwise               = my_iterate2 xs p4k1 s
            where 
                newPartialSum a b = sum (takeWhile (<= upperLimit) (map (* (a^7*b^3)) possibleFactors))
    my_iterate2 (x:xs) [] s = my_iterate2 xs p4k1 s
    my_iterate2 [] _ s      = s

    -- case number three: 105=21*5, that is two different prime factors 4k+1
    my_iterate3 l1@(x:xs) l2@(y:ys) s 
        | x==y                    = my_iterate3 l1 ys s
        | x^10 > upperLimit        = s
        | (x^10*y^2) <= upperLimit = my_iterate3 l1 ys (s + (newPartialSum x y))
        | otherwise               = my_iterate3 xs p4k1 s
            where 
                newPartialSum a b = sum (takeWhile (<= upperLimit) (map (* (a^10*b^2)) possibleFactors))
    my_iterate3 (x:xs) [] s = my_iterate3 xs p4k1 s
    my_iterate3 [] _ s      = s


    -- n <= 38.000.000 = 30.875.234.922
    -- f(84246500) = f(248431625) = 420
    -- res: 271.204.031.455.541.309
    main = do
        -- [start] <- map (\x -> (read x) :: Integer) `fmap` getArgs
        printf "%d\n" (my_iterate1 p4k1 p4k1 p4k1 0 + my_iterate2 p4k1 p4k1 0 + my_iterate3 p4k1 p4k1 0)
