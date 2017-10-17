import Data.List.Split
import Data.List

toInt n = read n :: Int

parse :: String -> [[Int]]
parse s = map (map toInt) rows  
  where rows = map (splitOn " ")  (take 20 $ splitOn "\n" s)
  
readm m (x,y) 
    | x > 19 || y > 19 = 1
    | x < 0 || y < 0 = 1
    | otherwise = m !! x !! y

-- Takes a matrix m, a location (x,y), and a rule for finding 
-- the next position
getAdjacent f m (x,y) = map (readm m) $
  [(x,y), f (x,y), f $ f (x,y), f $ f $ f (x,y)]

generator (a,b) = getAdjacent (\(x,y) -> (x+a, y+b))

allAdjacents m (x,y) = 
  [generator (1,1) m (x,y), 
   generator (1,0) m (x,y), 
   generator (0,1) m (x,y),
   generator (1, -1) m (x,y),
   generator (-1, 1) m (x,y)]

candidates m = foldr (++) [] adjs
  where adjs = map (allAdjacents m) [(x,y) | x <- [0..19], y <- [0..19]]


main = do
  s <- readFile "p11.data"
  print $ last $ sort $ map product $ candidates (parse s)
