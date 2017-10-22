import Data.Maybe
import Data.List 
import Control.Applicative
import Data.Function

type Numerator = Int
type Denominator = Int
data Fraction = Fraction Numerator Denominator

type Base = Maybe Int
type Cycle = Maybe Int
data Decimal = Decimal Base Cycle 

--division :: Fraction -> Decimal
--division (Fraction n d) 
--  | mod n d == 0 = Decimal (Just (div n d)) Nothing


remain n d = (mod n d) * 10
--division' :: Int -> Int -> [Int]
division n d 
  | mod n d == 0 = [(div n d, (n, d))]
  | otherwise = ((div n d), (n,d)) : (division (remain n d) d)

-- finds cycles of length n 
findCycle n xs = findCycle' xs 0 n

findCycle' [] _ n = Nothing
findCycle' (x:xs) p n
  | i /= Nothing = liftA2 (,) (Just p) (fmap (+(1+p)) i)
  | otherwise = findCycle' xs (p+1) n
  where i = elemIndex x $ take n xs

cycleLength :: Maybe (Int, Int) -> Int
cycleLength Nothing = 0 
cycleLength (Just (a,b)) = b-a

cycles = map (findCycle 1000) $ map (division 1) [1..]
lengths = map cycleLength $ take 1000 cycles

main = do print $ sortBy (compare `on` snd) $ zip [1..] lengths 

-- division algorithm has two steps 
-- 1. given a/b compute div a b and append to c
-- 2. multiply mod a b by 10 
