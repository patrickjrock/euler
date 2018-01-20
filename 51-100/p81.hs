import Data.List.Split
import Data.List
import Data.Array

toInt n = read n :: Int

parse s = map (map toInt) rows 
  where rows = map (splitOn ",") $ splitOn "\n" (init s)

neighbors (x,y)  
  | x /= 0 && y /= 0 = [(x-1,y), (x,y-1)]
  | x == 0 && y == 0 = []
  | x /= 0 = [(x-1,0)]
  | otherwise = [(0,y-1)]

get m (x,y) = (m !! x) !! y

shortestPath :: [[Int]] -> (Int,Int) -> Int
shortestPath m (a,b) = sp (a,b)
  where sp (0,0) = get m (0,0) 
        sp loc = (get m loc) + (minPath loc)
        minPath loc = minimum $ map (\x->arr ! x) (neighbors loc)
        bounds = ((0,0),(79,79))
        arr = listArray bounds [sp x | x <- range bounds] 


main = do 
  s <- readFile "p081_matrix.txt"
  print $ shortestPath (parse s) (79,79)
