import Data.Numbers.Primes
import Data.Char
import Data.List

digits x = map digitToInt $ show x 

joinds :: [Int] -> Int
joinds = read . concatMap show

perm a b = elem da pb
  where da = digits a
        pb = permutations $ digits b
        

ps = filter (\n->n>1000) $ takeWhile (<10000) primes

cs = [(a,b,c) | a <- ps, b <- ps, c <- ps,
                a-b == b-c,
                a /= b,
                b /= c,
                (perm a b) && (perm b c)]
