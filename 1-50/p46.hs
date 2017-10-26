import Data.Numbers.Primes

isInt x = x == fromInteger (round x)
square = isInt . sqrt
twiceSquare n = square $ (x/2)
  where x = fromIntegral n 

candidate :: Int -> Bool
candidate n = or $ map twiceSquare ds
  where ds = map (subt n) (takeWhile (<=n) primes)
        subt n p = n-p

main = do print $ head $ dropWhile candidate [3,5..]
