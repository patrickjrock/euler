import Data.Digits

data Gauss = Gauss Integer Integer 

instance Show Gauss where
  show (Gauss 1 b) 
    | b < 0 = "i" ++ show b
    | otherwise = "i+" ++ show b
  show (Gauss a b) 
    | b < 0 = show a ++ "i" ++ show b
    | otherwise = show a ++ "i+" ++ show b

instance Num Gauss where
  (+) (Gauss a b) (Gauss c d) = Gauss (a+c) (b+d)
  (-) (Gauss a b) (Gauss c d) = Gauss (a-c) (b-d)
  (*) (Gauss a b) (Gauss c d) = Gauss (a*d+b*c) (b*d-a*c)
  abs (Gauss a b) = Gauss 0 (a*a + b*b)
  signum (Gauss 0 0) = 0
  signum _ = 1
  fromInteger x = (Gauss 0 x)

-------
radix = Gauss 1 (-1)
basis = map (\x->radix^x) [0..]
-------

--binToGauss :: Integer -> Gauss
binToGauss x =  sum $ snd <$> filter (\(a,b)->a) (zip ds basis)
  where ds = reverse $ map (\x->x/=0) (digits 2 x)


csvGauss (Gauss a b) = show a ++ " " ++ show b ++ "\n"

format :: [Gauss] -> String
format l = "im re\n" ++ (concatMap csvGauss l)
