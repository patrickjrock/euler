import Data.Char
import Data.Numbers.Primes
import Data.List

digits :: Int -> [Int]
digits x = map digitToInt $ show x 

undigits :: [Int] -> Int
undigits = read . concatMap show

count a xs = length $ filter (==a) xs

--------------------------------------

candidate :: [Int] -> Bool
candidate ps = and $ map isPrime cats
  where cats = [catn p1 p2 | p1 <- ps, p2 <- ps, p1 /= p2]
        catn x y = read (show x ++ show y) :: Int

helper f p = filter isPrime ps
  where ds = digits p
        ps = map undigits $ (tail . init) $ f ds

prefixPrimes :: Int -> [Int]
prefixPrimes = helper inits
        
suffixPrimes :: Int -> [Int]
suffixPrimes = helper tails 

pfs n = nub $ sort $ concatMap prefixPrimes $ take n primes
sfs n = nub $ sort $ concatMap suffixPrimes $ take n primes


-- given two primes test if they are a concatPair
isPair :: Int -> Int -> Bool
isPair p1 p2 = candidate [p1,p2] 

-- prime numbers that appear as prefixes and suffixes of primes
candidates n = intersect (pfs n) (sfs n)

countPairs :: [Int] -> Int -> Int
countPairs ps p = (length $ filter (isPair p) ps) 

cPairs n = map (countPairs cs) cs
  where cs = candidates n
