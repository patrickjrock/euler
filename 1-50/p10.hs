checkFactors :: Integer -> Integer -> Bool
checkFactors n f 
    | f > (floor $ sqrt $ fromIntegral $ n) = True
    | mod n f == 0 = False
    | otherwise = checkFactors n (f+2)

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime 3 = True
isPrime 5 = True
isPrime n = if mod n 2 == 0 || mod n 3 == 0
            then False 
            else checkFactors n 5

sumPrimes 0 s = s
sumPrimes n s = if isPrime n 
                then sumPrimes (n-1) (s+n)
                else sumPrimes (n-1) s

main = do print $ sumPrimes 2000000 0
