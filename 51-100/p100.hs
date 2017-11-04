ratio (a:b:cs) = ((fromIntegral b) / (fromIntegral a)) : (ratio (b:cs))

candidate :: (Integer, Integer) -> Bool
candidate (r,b) = 2*b*(b-1) == t*(t-1)
  where t = r+b

-- given a total return the number of blues 
getB :: Integer -> Integer
getB t' = ceiling $ t / (sqrt 2)
  where t = fromIntegral t'

getTotal :: Maybe (Integer, Integer) -> Integer
getTotal (Just (a,b)) = a+b
getTotal _ = 0

makeArr :: Integer -> Maybe (Integer, Integer)
makeArr t 
  | candidate (r,b) = Just (r,b)
  | otherwise = Nothing
  where b = getB t
        r = t-b

nextArr (r,b) = head $ filter (/= Nothing) $ map makeArr [i..]
  where t = r+b
        i = floor $ (fromIntegral t)*5.82842

iterateM f x = x : iterateM f (x >>= f) 

