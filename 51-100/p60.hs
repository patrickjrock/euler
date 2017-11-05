import Data.Char
import Data.Numbers.Primes
import Data.List

digits :: Int -> [Int]
digits x = map digitToInt $ show x 

undigits :: [Int] -> Int
undigits = read . concatMap show

count a xs = length $ filter (==a) xs

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']

--------------------------------------

candidate :: [Int] -> Bool
candidate ps = (unique) && (and $ map isPrime cats)
  where cats = [catn p1 p2 | p1 <- ps, p2 <- ps, p1 /= p2]
        catn x y = read (show x ++ show y) :: Int
        unique = (nub ps) == ps

helper f p = filter isPrime ps
  where ds = digits p
        ps = map undigits $ (tail . init) $ f ds

prefixPrimes :: Int -> [Int]
prefixPrimes = helper inits
        
suffixPrimes :: Int -> [Int]
suffixPrimes = helper tails 

pfs n = nub $ sort $ concatMap prefixPrimes $ take n primes
sfs n = nub $ sort $ concatMap suffixPrimes $ take n primes

-- prime numbers that appear as prefixes and suffixes of primes
candidates n = intersect (pfs n) (sfs n)

-- given two primes test if they are a concatPair
-- this is a two prime family
isPair :: Int -> Int -> Bool
isPair p1 p2 = candidate [p1,p2] 

countFamilys :: Int -> [Int] -> Int -> Int
countFamilys 2 ps p = 1
countFamilys n ps p = length $ filter id (map candidate $ map (p:) combs)
  where combs = combinations (n-1) ps'
        ps' = filter (\x -> countFamilys (n-1) ps x /=0) ps

countPairs :: [Int] -> Int -> Int
countPairs ps p = (length $ filter (isPair p) ps) 

cPairs n = map (countPairs cs) cs
  where cs = candidates n

cTrips n = map ((countFamilys 3) cs) cs
  where cs = candidates n

---------------------------
-- Trying to find quad familys
-- applicative style is too slow

isQuad :: Int -> Int -> Int -> Int -> Bool
isQuad p1 p2 p3 p4 = candidate [p1,p2,p3,p4]

countQuads :: [Int] -> Int -> Int
countQuads ps p = length $ filter id ((isQuad p) <$> ps <*> ps <*> ps)

cQuads n = map (countQuads cs) cs
  where cs = filter (\x-> countTrips (candidates n) x /= 0) (candidates n)
        countTrips = countFamilys 3


----------------------------
countFM _ _ [] = []
countFM n i ps 
  | length ps == i = []
  | otherwise = if c == 0 then countFM n i (delete p ps) 
                          else c : countFM n (i+1) ps
  where c = (countFamilys n ps p)
        p = ps !! i

countFM' n m = countFM n 0 (candidates m)
