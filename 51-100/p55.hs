import Data.Char
import Data.List
import Utils

--lychrel :: Integer -> Bool
lychrel n = not $ or $ map numdrome $ take 50 iters
  where iters = tail $ iterate revadd n
        revadd a = a + (ra a)
        ra = undigits . reverse . digits
