import Data.List

isInt x = x == fromInteger (round x)

ps =  toInteger <$> (\n->div (n*(3*n-1)) 2) <$> [1..] 
pentagonal n = ps !! (n-1)
pentagonals = map pentagonal [1..]

isPent n = isInt $ inv $ fromInteger n
  where inv x = (1/6)*(1 + sqrt (24*x + 1))



pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

cs n = pairs [1..n]

candidate1 (a,b) = isPent (pa+pb)
  where pa = pentagonal a
        pb = pentagonal b 

candidate2 (a,b) = isPent $ abs (pa-pb)
  where pa = pentagonal a
        pb = pentagonal b 



p44 n = filter candidate2 $ filter candidate1 $ cs n 

main = do
  l <- getLine
  let n = read l :: Int
  print $ p44 n
