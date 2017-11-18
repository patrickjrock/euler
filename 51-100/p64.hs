

isInt x = x == fromIntegral (round x)

isSquare :: Int -> Bool
isSquare = isInt . sqrt . fromIntegral

cs = filter (not . isSquare) [2..] 

sqrt' = floor . sqrt . fromIntegral

cfrac :: Int -> [(Int, (Int,Int,Int))]
cfrac x = fixM expand [(a0, (x,a,b))]
  where a0 = sqrt' x
        a = 1
        b = (-a0)

period = (+(-1)) . length . cfrac

--expand :: Number -> (Int,  Number)
expand (x,a,b) = [(an, (x,a',b'))]
  where v = x - b*b
        b' = last l
        x' = floor $ sqrt $ fromIntegral x
        a' = round $ fromIntegral v / fromIntegral a
        l = takeWhile (>=(-1*x')) $ iterate (+ (-1*a')) (-1*b)
        an = (length l) - 1

-- adhoc monad bullshit
bind xs exp 
  | elem (head r) xs = xs
  | otherwise = xs ++ r
  where r = exp s
        s = snd $ last xs

fixM f x = if y == x then y else fixM f y
  where y = x `bind` f

--contf :: Int -> ContinuedFraction
--contf x = (a0, 
--  where a0 = floor $ sqrt $ fromIntegegral x
