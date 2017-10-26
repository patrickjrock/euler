import Data.Char

digitize x = map digitToInt $ show x 

c10 = concatMap digitize [1..]
d n = c10 !! (n-1)
main = do print $ product $ map d [1,10,100,1000,10000,100000,1000000]
