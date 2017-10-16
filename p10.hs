sieve :: [Int] -> Int -> Bool
sieve [] x = True 
sieve ps x = if mod x (head ps) == 0 then False else sieve (tail ps) x

nextPrime' ps n = if sieve ps n then n else nextPrime' ps (n+1)
nextPrime :: [Int] -> [Int]
nextPrime ps = ps ++ [nextPrime' ps (last ps)]

getPrimes' limit ps = if p < limit
                      then getPrimes' limit (ps ++ [p])
                      else ps
                      where p = last $ nextPrime ps 
getPrimes limit = getPrimes' limit [2]

main = do print $ sum $ getPrimes 2000000
