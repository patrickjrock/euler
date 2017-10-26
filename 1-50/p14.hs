import Data.List
import Data.Ord

nextCollatz :: Integer -> Integer
nextCollatz n
  | even n = div n 2
  | otherwise = 3*n + 1 

collatz :: Integer -> [Integer]
collatz 1 = []
collatz n = m : (collatz m)
  where m = nextCollatz n

longestCollatz n = maximumBy (comparing (\(a,b) -> b)) seqs
  where seqs = zip [1..n] $ map length $ map collatz [1..n]

main = do print $ longestCollatz 1000000
