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

pfs n = nub $ sort $ concatMap prefixPrimes $ takeWhile (<n) primes
sfs n = nub $ sort $ concatMap suffixPrimes $ takeWhile (<n) primes

-- prime numbers that appear as prefixes and suffixes of primes
candidates n = intersect (pfs n) (sfs n)

-- given two primes test if they are a concatPair
-- this is a two prime family
isPair :: Int -> Int -> Bool
isPair p1 p2 = candidate [p1,p2] 

pairs :: Int -> [Int]
pairs p = filter (isPair p) $ takeWhile (<10000) primes

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

fams 2 m = filter candidate $ combinations 2 $ candidates m
fams n m = filter candidate $ combs
  where cs = nub $ concat $ fs
        combs = (++) <$> (map (:[]) cs) <*> fs
        fs = fams (n-1) m

expandFamily :: Int -> [Int] -> [[Int]]
expandFamily n f = filter candidate $ map (:f) ps
  where ps = foldr1 intersect $ map (take n) $ map pairs f
        fsize = length f

main = do 
  n <- getLine
  m <- getLine
  print $ fams (read n) (read m)
