import Data.List.Split
import Data.List

toInt n = read n :: Int

parse s = map (map toInt) rows 
  where rows = map (splitOn " ") $ splitOn "\n" (init s)

paths 0 = [[0], [1]]

paths n = concatMap extend $ paths (n-1)
  where extend path = [0:path, 1:path]

convertPath [x] = [x]
convertPath (x:xs) = x : (convertPath $ map (+1) xs)

route t p  = (head $ head t) : path
  where path = zipWith ($) (map (flip (!!)) p) (tail t)

allPaths s = map (route (parse s)) (paths 13)



main = do
  s <- readFile "p18.data"
  print $ sort $ map sum $ allPaths s
