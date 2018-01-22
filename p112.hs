import Data.Digits
import Data.List
import Data.List.Ordered

type Ratio = (Integer,Integer) 

numberf f n = isSortedBy f ds
  where ds = digits 10 n

increasing :: Integer -> Bool
increasing = numberf (<=) 
 
decreasing :: Integer -> Bool
decreasing = numberf (>=) 

strictDec = numberf (>)

strictInc = numberf (<)

bouncy :: Integer -> Bool
bouncy n = (not . increasing) n && (not . decreasing) n

brs = map bouncyRatio' [1..] 

bouncyRatio' 1 = (0,1)
bouncyRatio' 1585000 = bouncyRatio 1585000
bouncyRatio' n =
  if bouncy n 
  then (a+1,b+1)
  else (a,b+1)
  where (a,b) = brs !! ((fromIntegral n)-2) 


bouncys n = takeWhile (<=n) bs 
  where bs = filter bouncy [1..n]

bouncyRatio n = (bs,  (fromIntegral n))
  where bs = fromIntegral $ length $ filter bouncy [1..n]

evalf (a,b) = (fromIntegral a) / (fromIntegral b)

binarySearch :: Ord b => (a->b) -> [a] -> b -> a
binarySearch _ [x] _ = x
binarySearch f l targ
  | midVal == targ = mid l
  | midVal > targ = binarySearch f (take midIndex l) targ
  | otherwise = binarySearch f (drop (midIndex+1) l) targ
  where mid list = list !! midIndex
        midIndex = div (length l) 2
        midVal = f (mid l)


