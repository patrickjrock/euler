import Data.Numbers.Primes
import Data.Char
import Data.List

digits x = map digitToInt $ show x 

joinds :: [Int] -> Int
joinds = read . concatMap show

pandigitals :: [[Int]]
pandigitals = permutations [0..9]

subdivide :: [a] -> [[a]]
subdivide = subdivide' . tail
  where subdivide' (x:xs) 
          | length (x:xs) == 3 = [x:xs]
          | otherwise = (take 3 (x:xs)) : (subdivide' xs)

candidate s = and $ zipWith ($)  (div' <$> substrings) primes
  where substrings = joinds <$> (subdivide s)
        div' a b = (rem a b) == 0
