data Frac = Frac Int Int deriving (Show)

denom :: Frac -> Int
denom (Frac x y) = y

numer :: Frac -> Int
numer (Frac x y) = x

divf :: Frac -> Float
divf (Frac x y) = fromIntegral x / fromIntegral y

closeLeft :: Int -> Frac -> Frac
closeLeft n (Frac x y) = Frac (n*x-1) (n*y-2)


main = do print $ closeLeft 142857 (Frac 3 7)
