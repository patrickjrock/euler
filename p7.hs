sieve :: [Int] -> Int -> Bool
sieve [] x = True 
sieve ps x = if mod x (head ps) == 0 then False else sieve (tail ps) x

nextPrime' ps n = if sieve ps n then n else nextPrime' ps (n+1)
nextPrime :: [Int] -> [Int]
nextPrime ps = ps ++ [nextPrime' ps (last ps)]

nthPrime n = last $ iterate nextPrime [2] !! (n-1)

main = do print $ nthPrime 10001
