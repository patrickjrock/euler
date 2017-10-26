diagonals 0 = 1
diagonals n = ((div (n-1) 4)*2 + 2) + diagonals (n-1)
spiragonals n = map diagonals [0..(n*2 -2)]
main = do $ print $ sum $ spiragonals 1001
