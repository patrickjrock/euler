import Data.List

ps =  toInteger <$> (\n->div (n*(3*n-1)) 2) <$> [1..] 
pentagonal n = ps !! (n-1)
pentagonals = map pentagonal [1..]

ispent n = isp n pentagonals
  where isp n (p:ps) 
          | n == p = True
          | n < p = False
          | otherwise = isp n ps -- n < p


pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

cs n = pairs [1..n]

candidate1 (a,b) = ispent (pa+pb)
  where pa = pentagonal a
        pb = pentagonal b 

candidate2 (a,b) = ispent $ abs (pa-pb)
  where pa = pentagonal a
        pb = pentagonal b 



p44 n = filter candidate2 $ filter candidate1 $ cs n 

main = do
  l <- getLine
  let n = read l :: Int
  print $ p44 n
