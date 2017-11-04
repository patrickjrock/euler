import Data.Numbers.Primes
import Data.Char
import Data.List

digits :: Int -> [Int]
digits x = map digitToInt $ show x 

undigits :: [Int] -> Int
undigits = read . concatMap show

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace x y (a:as) 
  | a == x = y : (replace x y as)
  | otherwise = a : (replace x y as)

nubDigits = nub . digits

familys :: Int -> [[Int]]
familys p = map family rs
  where rs = [replace x | x <- nubDigits p]
        family r = filter isPrime $ map undigits $ filter (\n->head n /= 0) [r i p' | i <- [0..9]]
        p' = digits p

candidate f = elem 8 $ map length f 
