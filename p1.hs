import System.CPUTime

list = [x | x <- [1..999], (mod x 5==0 || mod x 3==0)]
main = do time $ print $ sum list 

