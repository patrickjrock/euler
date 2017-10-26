import Data.List
import Data.List.Split
import Data.Char


parse s = map (filter isAlpha) words
  where words = splitOn "," s

value c = (ord c) - 64
score s = sum $ map value s

triangulars = 1 : zipWith (+) [2,3..] triangulars
triangular n = elem n (takeWhile (<=n) triangulars)

triangleWord w = triangular $ score w

main = do 
  s <- readFile "p042_words.txt"
  print $ length $ filter triangleWord $ parse s 
