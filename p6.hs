square :: Integer -> Integer
square x = x*x

sumSquare n = sum $ fmap square [1..n]
squareSum n = square $ sum [1..n]

difference n = (sumSquare n) - (squareSum n)
