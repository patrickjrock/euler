import Data.Char
import Data.List

digitize x = map digitToInt $ show x 

pandigital n x = (null ([1..n] \\ x)) && (length x == n)

pd = pandigital 9

product' :: Int -> [Int] -> [Int]
product' x ns = concat $ map digitize $ map (*x) ns


candidates m n = zipWith ($) (map product' [1..m]) (repeat [1..n])

p38 m n = sort $ filter pd $ concatMap (candidates m) $ [1..n]
