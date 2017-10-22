import Data.Maybe
import Data.List 
import Control.Applicative

type Numerator = Int
type Denominator = Int
data Fraction = Fraction Numerator Denominator

type Base = Maybe Int
type Cycle = Maybe Int
data Decimal = Decimal Base Cycle 

--division :: Fraction -> Decimal
--division (Fraction n d) 
--  | mod n d == 0 = Decimal (Just (div n d)) Nothing

division :: Int -> Int -> ([Int], Bool)
division n d 
  | mod n d == 0 = ([div n d], False)
  | mod n d == 1 && n /= 1 = ([d], True)
  | mod n d == div n 10 = ([d], True)
  | otherwise = ((div n d) : (fst rec), snd rec)
  where rec = (division ((mod n d) * 10 ) d) 

remain n d = (mod n d) * 10
--division' :: Int -> Int -> [Int]
division' n d 
  | mod n d == 0 = [(div n d, (n, d))]
  | otherwise = ((div n d), (n,d)) : (division' (remain n d) d)

-- finds cycles of length n 
findCycle xs n = findCycle' xs 0 n

findCycle' [] _ n = Nothing
findCycle' (x:xs) p n
  | i /= Nothing = liftA2 (,) (Just p) (fmap (+(1+p)) i)
  | otherwise = findCycle' xs (p+1)
  where i = elemIndex x $ take n xs

-- division algorithm has two steps 
-- 1. given a/b compute div a b and append to c
-- 2. multiply mod a b by 10 
