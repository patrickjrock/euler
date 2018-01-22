import Math.NumberTheory.Primes

-- number of composites x is a factor of 
g n x = (f n (div n x))


-- takes a max n and factor to count composites x
f n x = 
  if x*x <= n
  then (length $ takeWhile (<=m) primes) - 1
  else length $ takeWhile (<=m) primes
  where m = (div n x)




ps n = takeWhile (<=(div n 2)) primes

composites n = (div temp 2) + (length ps'')
  where temp = (sum $ map (f n) ps')
        ps' = ps n
        ps'' = takeWhile (\x-> x*x<n) primes


-- counts of x is 2(n-1) + 1 = 2n - 1
-- if x is less than root n
