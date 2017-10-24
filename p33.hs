import Data.Char
import Data.Function

div' = (/) `on` fromIntegral

digitize x = map digitToInt $ show x 
unDigitize x = sum $ reverse $ zipWith (*) (reverse x) [10^n | n <- [0..]]

trivial :: (Int, Int) -> Bool
trivial (a,b) = (mod a 10 == 0) && (mod b 10 == 0) 

safeTail [] = []
safeTail a = tail a

dropFirst :: Eq a => a -> [a] -> [a]
dropFirst x xs = a ++ safeTail b
  where (a,b) = break (==x) xs

simplify' :: ([Int],[Int]) -> ([Int],[Int])
simplify' ([], b) = ([], b)
simplify' (a,b) = if elem (head a) b 
                 then (tail a, dropFirst (head a) b) 
                 else ((head a) : sa, sb)
  where (sa, sb) = simplify' ((tail a), b)  

simplify (a,b) = (unDigitize sa, unDigitize sb)
  where (sa, sb) = simplify' (digitize a, digitize b)

curious :: (Int, Int) -> Bool
curious (a,b) = different && sameDiv && nonTrivial
  where (sa, sb) = simplify (a,b)
        different = (sa,sb) /= (a,b)
        sameDiv = (a `div'` b)  == (sa `div'` sb) 
        nonTrivial = not $ trivial (a,b)
