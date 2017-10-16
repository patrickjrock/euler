import Data.List

isDivis :: Int -> Bool
isDivis n = and $ fmap test [1..20]
  where test x = mod n x == 0

nextNum (num, val) = (num+1, isDivis $ num+1)

--255255 is obtained by multiplying all the prime numbers less than 20
smallest' (num, True) = num
smallest' (num, False) = smallest' (num+255255, isDivis $ num+255255)

smallest = smallest' (255255, False)

main = do print $ smallest
