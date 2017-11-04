module Utils where

import Data.Char
import Data.List
import Control.Monad

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])
  
--digits :: Integer -> [Integer]
digits x = map digitToInt $ show x 

palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == (reverse xs)

numdrome :: Integer -> Bool
numdrome = palindrome . digits

--undigits :: [Integer] -> Integer
undigits = read . concatMap show

