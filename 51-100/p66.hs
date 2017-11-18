import Data.Ord
import Data.List

isInt x = x == fromInteger (round x)
isSquare = isInt . sqrt . fromInteger 

isSolution :: Integer -> Integer -> Bool
isSolution d x  
  | isInt k = isSquare (round k) 
  | otherwise = False
  where x' = fromIntegral x
        d' = fromIntegral d
        k = (x'*x'-1)/d' :: Double

cs = filter (not . isSquare) [2..] 
minimize f = if x == [] then -1 else head x
  where x = filter f [2..200000]

-- gives the index of the maximum element
indexMax :: Ord a => [a] -> (a,Int)
indexMax l = maximumBy (comparing fst) (zip l [1..])

maxSet s = maximumBy (comparing snd) $ zip s (minimize <$> isSolution <$> s)

p66 n = indexMax $ minimize <$> isSolution <$> takeWhile (<=n) cs
