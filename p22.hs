import Data.List.Split
import Data.Char
import Data.List

parse s = map (init . tail) names
  where names  = splitOn "," s

value c = (ord c) - 64
score s = sum $ map value s

main = do 
  s <- readFile "p022_names.txt"
  print $ sum $ zipWith (*) [1..] (map score $ sort $ parse s) 
