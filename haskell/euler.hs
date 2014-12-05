module Euler where
    import qualified Data.List as L
    import qualified Math.NumberTheory.Primes as P
    import Math.NumberTheory.Primes.Factorisation
    import System.IO.Unsafe

    import Common
    import Roman

    -- actual solutions

    problem26 :: Int
    problem26 = generatesLongestCycle
        where 
            generatesLongestCycle = fst (foldl longer (0,0) generatorCyclePairs)
            generatorCyclePairs = map (\d -> (d,cycleLength d)) [2..1000] 
            longer (a1, a2) (b1, b2) = if (a2 < b2) then (b1, b2) else (a1, a2)
            cycleLength :: Int -> Int
            cycleLength d = lastModule [] 1 d 
                where
                    lastModule :: [Int] -> Int -> Int -> Int
                    lastModule ds a b = if ( a `mod` b == 0 ) then 0
                        else if nextModule >0 && nextModule `elem` ds then findLast nextModule ds 
                            else lastModule (ds++[nextModule]) ((10*a `mod` b)) b
                        where nextModule = 10*a `mod` b
            findLast x xs = if x == last xs then 1 else 1+(findLast x (init xs))

    problem27 :: Int
    problem27 = x*y
        where
            (l, x, y) = foldl larger (0,0,0) triples
            larger (v, v1, v2) (z, z1, z2) = if (v < z) then (z,z1,z2) else (v,v1,v2)
            triples = map (\(v,z)->(lengthPrimeList v z, v, z)) params
            params = [(v,z)|v<-a, z<-b, -(1600+40*v)<z]
            a = [-999 ..  999]
            b = takeWhile (\p -> p < 1000) primes
            gen v z = [w*w + v*w + z | w <- [0..]]
            lengthPrimeList v z = length$takeWhile isPrime (gen v z)

    problem29 = length$distinct [a^b | a<-[2..100], b<-[2..100]]
        where
            distinct [] = []
            distinct (x:xs) = if (x `elem` xs) then distinct xs else x:(distinct xs)

    problem30 = sum fifthPowers - 1
        where 
            fifthPowers = [asNum [a,b,c,d,e,f] |a<-x,b<-x,c<-x,d<-x,e<-x,f<-x, (sum$map (^5) [a,b,c,d,e,f]) == (asNum [a,b,c,d,e,f])]
            asNum [] =0
            asNum xs = (last xs) + asNum (map (*10) (init xs))
            x=[0..9]

    problem31 = length [(a,b,c,d,e,f,g)| a<-[0,1], 
                                         b<-[0..(2-2*a)], 
                                         c<-[0..(4-4*a-2*b)], 
                                         d<-[0..(10-10*a-5*b-2*c)], 
                                         e<-[0..(20-20*a-10*b-5*c-2*d)],
                                         f<-[0..(40-40*a-20*b-10*c-4*d-2*e)], 
                                         g<-[0..(100-100*a-50*b-25*c-10*d-5*e-2*f)], 
                                         200*a+100*b+50*c+20*d+10*e+5*f+2*g <= 200]

    problem32 = sum . unique $ [a*b | a<-[4..49], b<-[a+1..1987], arePandigital (a,b,a*b)]
        where
            arePandigital (x,y,z) = sort (show x ++ show y ++ show z) == "123456789"
            sort [] = []
            sort (x:xs) = sort (filter (<= x) xs) ++ [x] ++ sort (filter (> x) xs)
            unique [] = []
            unique (x:xs)
                | x `elem` xs = unique xs
                | otherwise = x: unique xs

    problem33 = denominator `div` (gcd numerator denominator)
       where 
           pairs = [(a,b) | a<-[10..98], b<-[a+1..99], isCurious a b]
           numerator = product (map (\(n,_) -> n) pairs) 
           denominator = product (map (\(_,d) -> d) pairs)
           isCurious x y = (reducedFraction x y) == fromIntegral x / fromIntegral y
           reducedFraction :: Fractional t => Int -> Int -> t
           reducedFraction x y
               | x `mod` 10 == y `mod` 10 && x `mod` 10 > 0 = fromIntegral (x `div` 10) / fromIntegral (y `div` 10)
               | x `mod` 10 == y `div` 10 = fromIntegral (x `div` 10) / fromIntegral (y `mod` 10)
               | x `div` 10 == y `mod` 10 = fromIntegral (x `mod` 10) / fromIntegral (y `div` 10)
               | x `div` 10 == y `div` 10 = fromIntegral (x `mod` 10) / fromIntegral (y `mod` 10)
               | otherwise = 0
            
    problem34 = sum (filter numIsSumOfDigitFactorials [10..9999999])
        where
            numIsSumOfDigitFactorials num = sum (map (\n -> precalcFactorials !! n) (listOfDigits num)) == num
                where
                    precalcFactorials = map factorial [0..9] 
                    listOfDigits n 
                        | n < 10    = n:[]
                        | otherwise = (n `mod` 10):(listOfDigits (n `div` 10))

    problem36 = sum [a | a <- [1,3..999999], isPalindrome a, isPalindrome $ binary a] 

    problem37 = sum [p | p <- properPrimes, truncatableLeft p, truncatableRight p]
        where 
            properPrimes = filter (\p -> p>10 && noSuspiciousDigits p) (take 1000000 P.primes)
                where 
                    noSuspiciousDigits n = not ( '0' `elem` (show n)
                                              || '2' `elem` (tail.show $ n)
                                              || '4' `elem` (show n)
                                              || '5' `elem` (tail.show $ n)
                                              || '6' `elem` (show n)
                                              || '8' `elem` (show n))
            truncatableRight n 
                | n < 10 = P.isPrime n
                | otherwise = (P.isPrime n) && (truncatableRight (n `div` 10))
            truncatableLeft n 
                | n < 10 = P.isPrime n
                | otherwise = (P.isPrime n) && (truncatableLeft (n `mod` (10^(length (show n) -1))))

    problem38 = maximum $ filter (isPandigital) (map (\n -> conc (mapping n) "") [1..9999])
        where
            mapping n = [show (n*factor) | factor <- [1..9]]
            conc :: [String] -> String -> String
            conc [] xs = xs
            conc (a:as) xs
                | (length (a ++ xs) > 9) = xs
                | otherwise = conc as (xs ++ a)
    
    problem39 = maximum $ map (\n -> (length.solutions $ n, n)) [100,102..1000]
        where 
            solutions n = [[a,b,c] |
                           a <- [1..n `div` 3],
                           let b = n*(n-2*a) `div` (2*(n-a)),
                           let c = n - a - b,
                           b > a,
                           b <= (2*n) `div` 3,
                           a*a + b*b == c*c]

    problem41 = maximum $ filter (P.isPrime) candidates
        where 
            candidates = [(read n)::Integer | l <- [1..9]
                                            , n <- L.permutations (take l "123456789")]

    problem45 = head $ dropWhile (\n -> not.p.fromIntegral $ n) pentagon
        where
            p n = isInteger (t n) && isInteger (h n)
            t n = sqrt (1/4 + 2*n) - 1/2
            h n = sqrt (1/16 + n/2) + 1/4
            pentagon = [b*(3*b-1) `div` 2 | b <- [166,167..]]

    problem48 = sum ([a^a `mod` 10^10 | a <- [1..1000]]) `mod` 10^10

    problem50 = snd$last$snd (last (filter (\(_, xs) -> length xs >0) primeSums))
        where
            ps = takeWhile (<1000000) primes
            consSumPrimes l = filter (\p -> snd p `elem` ps) (map (\n -> (n, sum (take l (drop n ps)))) [0..(1000000 `div` l)])
            primeSums = map (\n -> (n, consSumPrimes n)) [500 .. 600]

    problem53 = length [(n,r)| n<-[22,23..100], r<-[1..n], choose n r > 1000000]
        where
            choose n r = foldl (\z i -> (z*(n-i+1)) `div` i) 1 [1..r]

    problem55 = length (filter isLychrel [1..10000])
        where   
            isLychrel :: Integer -> Bool
            isLychrel n = isLychrel' n 0 
                where 
                    isLychrel' :: Integer -> Int -> Bool
                    isLychrel' n count
                        | count >= 50 = True
                        | isPalindrome (nextNum n) = False
                        | otherwise = isLychrel' (nextNum n) (count+1)
                    nextNum n = n + (read (reverse.show $ n) :: Integer) 

    problem89 = sum differenceList 
        where 
            differenceList = map (\s -> (length s) - (length (toRoman.fromRoman $ s))) listOfRomanNumbers
            listOfRomanNumbers = lines (unsafePerformIO.readFile $ "roman.txt" ) -- I love the danger !!!

    problem97 = (28433*2^7830457 + 1) `mod` 10^10

    problem144 = length (takeWhile (\((x,y),_)-> ((-0.01) > x || x > 0.01 || y < 0)) pointSequence)
        where
            pointSequence :: [(Point2D, Double)]
            pointSequence = iterate newReflectionPoint ((1.4,-9.6), slope (0.0, 10.1) (1.4, -9.6))
                where
                    slopeNormal (x,y) = 1.0/4.0*y/x
                    slopeReflection m1 m2 = (m1 - h)/(1+m1*h)
                        where
                            h = (m2-m1)/(1+m1*m2)
                    newReflectionPoint (p@(x, y), currentSlope) = ((newX, newX*m + n), m)
                        where
                            m = slopeReflection (slopeNormal p) currentSlope
                            n = y -  m*x
                            (x1, x2) = quadraticZeros (4+ m^2) (2* m*n) (n^2-100) 
                            newX = if (abs (x-x1) > abs (x-x2)) then x1 else x2

