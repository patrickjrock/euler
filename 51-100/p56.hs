import Utils

maxSum :: Integer
maxSum = maximum $ digitalSum <$> l
  where digitalSum = sum . digits 
        l = [a^b | a <- [1..100], b <- [1..100]]

main = do print $ maxSum
