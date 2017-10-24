import Data.Char
import qualified Data.Set as Set
import Data.List
import Control.Monad

digitize x = map digitToInt $ show x 
unDigitize x = sum $ reverse $ zipWith (*) (reverse x) [10^n | n <- [0..]]

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

pandigital n x = (null ([1..n] \\ x)) && (length x == n)
noRepeat x = nub d == d
  where d = digitize x


setToNums s = map unDigitize $ perms
  where perms = concatMap permutations $ init $ powerset s 

ns = filter ((<5) . length. digitize) $ setToNums [1..9]
getBs a = setToNums $ map toInteger ([1..9] \\ (digitize a))

l = concat $ zipWith (stitch) ns (map getBs ns)
  where stitch n bs = map ((,) n) bs

candidate (a,b) = pandigital 9 (da++db++dc)
  where da = digitize a
        db = digitize b
        dc = digitize (a*b)
