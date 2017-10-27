module Utils where

import Data.Char
import Data.List

digits :: Integer -> [Integer]
digits x = map (toInteger . digitToInt) $ show x 

palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == (reverse xs)

numdrome :: Integer -> Bool
numdrome = palindrome . digits

undigits :: [Integer] -> Integer
undigits = read . concatMap show


