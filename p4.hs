import Data.Char
import Data.List

digitize :: Int -> [Int]
digitize x = map digitToInt $ show x 

palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == (reverse xs)

numdrome :: Int -> Bool
numdrome = palindrome . digitize

list = [x |a <- [1..999], b <- [1..999], let x = a*b]

largestDrome = last $ sort $ filter numdrome list
