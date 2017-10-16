list = filter (\x -> x `mod` 5 == 0 || x `mod` 3 == 0 ) [1..999]
main = do print $ sum list
