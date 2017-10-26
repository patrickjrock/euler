fib :: [Integer] -> [Integer]
fib (a:b:cs) = ((a+b):a:b:cs)

applyIfLess :: [Integer] -> [Integer]
applyIfLess (a:bs) = if a > 4000000 then bs else applyIfLess $ fib $ (a:bs)

test :: Integer -> Bool
test x = mod x 2 == 0

main :: IO()
main = do print $ sum $ filter test (applyIfLess [2,1])  
