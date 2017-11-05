module Utils where

import Data.Char
import Data.List
import Control.Monad

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])
  
digits :: Integral a => a -> [a]
digits x = map digitToInt $ show x 

palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == (reverse xs)

numdrome :: Integer -> Bool
numdrome = palindrome . digits

undigits :: Integral a => [a] -> a
undigits = read . concatMap show

