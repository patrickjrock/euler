import Math.NumberTheory.Primes

-- Let s(n) be the smallest number m such 
--  that n divides m!.

-- count factors from list of prime factoization

pfs = map factorise [1..]

countUp 0 _ = 0
countUp n (x:xs) = 1 + (countUp (n-x) xs)

s n = maximum $ minfac <$> facs 
  where 
  facs = factorise n

bigS n = sum $ s <$> [2..n]

countPs' p fs = filter (\(a,b)->a==p) fs
countPs p fs =
  if (countPs' p fs) == []
  then 0
  else snd $ head $ countPs' p fs

partialFacs p = scanl (+) 0 (map (countPs p) pfs)

minfac (p,pow) = length $ takeWhile (< pow) $ partialFacs p --(countUp pow) $ (checkFac p pfs)


