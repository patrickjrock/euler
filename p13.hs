import Data.List.Split


toInt n = read n :: Integer

parse :: String -> [Integer]
parse s = map toInt rows 
  where rows = init $ splitOn "\n" s

main = do 
  s <- readFile "p13.data"
  print $ sum $ parse s
