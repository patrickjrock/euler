import Data.List.Split
import Data.List

toInt n = read n :: Int

parse s = map (map toInt) rows 
  where rows = map (splitOn " ") $ splitOn "\n" (init s)

ancestors (r,c) 
  | c == 0 = [(r-1, c)]
  | c == r = [(r-1, c-1)]
  | otherwise = [(r-1,c), (r-1, c-1)]
getv l (r,c) = (l !! r) !! c 



triangular :: Int -> Int
triangular n = div (n*(n+1)) 2
triangulars :: [Int]
triangulars = map triangular [1..]


maxPath l (0,0) = getv l (0,0)
maxPath l (a,b) = maxs !! i
  where maxs = map (maxPath' l) [(a,b) | a <- [0..99], b <- [0..a]]
        i = (triangulars !! (a-1)) + b
        maxPath' l (0,0) = getv l (0,0)
        maxPath' l (a,b) = v + (maximum paths)
        ancs = ancestors (a,b)
        v = getv l (a,b)
        paths = map (maxPath l) ancs


main = do
  s <- readFile "p067_triangle.txt"
  let l = parse s
  print $ maximum $ map (maxPath l) [(a,b) | a<-[99], b<-[0..99]]

