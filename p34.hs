import Data.Char

digitize x = map digitToInt $ show x 

facts = 1 : zipWith (*) [1..] facts 
fact n = facts !! n

sumFact n = n == (sum $ map fact (digitize n))
sumFacts = filter sumFact [1..]
