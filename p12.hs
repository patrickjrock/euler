import Data.List

factors n = pairs ++ (map (div n) pairs) 
  where r = floor $ sqrt $ fromIntegral n
        pairs = filter (\x -> mod n x == 0) [1..r]


triangulars = 1 : zipWith (+) [2,3..] triangulars

tri500s = filter (\(a,b) -> b > 500) $ ts
  where ts = zip triangulars $ map length $ map factors triangulars

main = do print $ head $ tri500s
