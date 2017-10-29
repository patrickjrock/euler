import Utils
import Data.Function.Memoize

candidates = [(a,b,c,d,e,f,g,h,i,j) | 
               a <- [0..9],
               b <- [0..9],
               c <- [0..9],
               d <- [0..9],
               e <- [0..9],
               f <- [0..9],
               g <- [0..9],
               h <- [0..9],
               i <- [0..9],
               j <- [0..9],
               a+b+c+d+e+f+g+h+i+j == 9]

chain n = iterate f n 
  where f x = sum $ map (^2) $ digits $ x

chain89' n 
  | c == 89 = True
  | c == 1 = False
  | otherwise = chain89 (head cs)
  where c:cs = chain n

cs = map chain89' [0..] 
chain89 n = cs !! (fromInteger n) 

chainSum 1 = 1
chainSum n = if chain89 n then 1 + chainSum (n-1) else chainSum (n-1)

main = do 
  temp_n <- getLine
  let n = read temp_n :: Integer
  print $ foldl (+) 0 $ map ((\b-> if b then 1 else 0) . chain89) [1..n]
