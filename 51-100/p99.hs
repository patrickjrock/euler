import Data.List.Split
import Data.List
import Data.Function

compareExp (a,x) (b,y) 
  | x*(log a) > y*(log b) = GT
  | x*(log a) < y*(log b) = LT
  | otherwise = EQ

parse f = map tupf $ map (map (\x->read x :: Integer)) strings
  where strings = map (splitOn ",") $ splitOn "\n" f
        tupf [a,b] = (fromIntegral a,fromIntegral b)
main = do 
  f <- readFile "p099_base_exp.txt"
  print $ sortBy (compareExp `on` snd) $ zip [1..] (parse f)
