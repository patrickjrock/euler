import Data.Char
import Data.Numbers.Primes
import Data.List

digits :: Int -> [Int]
digits x = map digitToInt $ show x 

undigits :: [Int] -> Int
undigits = read . concatMap show

count a xs = length $ filter (==a) xs

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1] 
                                  , x <- combinations (n-1) (drop (i+1) xs) ]

candidate :: [Int] -> Bool
candidate ps = and $ map isPrime cats
  where cats = [catn p1 p2 | p1 <- ps, p2 <- ps, p1 /= p2]
        catn x y = read (show x ++ show y) :: Int

helper f p = filter isPrime ps
  where ds = digits p
        ps = map undigits $ (tail . init) $ f ds

prefixPrimes :: Int -> [Int]
prefixPrimes p = filter isPrime ps
  where ds = digits p
        ps = map undigits $ (tail . init) $ inits ds
        
suffixPrimes :: Int -> [Int]
suffixPrimes p =  filter isPrime ps
  where ds = digits p
        ps = map undigits $ (tail . init) $ tails ds

pfs n = nub $ sort $ concatMap prefixPrimes $ take n primes
sfs n = nub $ sort $ concatMap suffixPrimes $ take n primes

-- prime numbers that appear as prefixes and suffixes of primes
candidates n = intersect (pfs n) (sfs n)
