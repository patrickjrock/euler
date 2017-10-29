import Data.Numbers.Primes
import Utils 
import Data.Maybe

removeD :: [Maybe a] -> [[Maybe a]]
removeD [x] = [[Nothing]]
removeD ((Just x):xs) = (Nothing:xs) : (map ([Just x]++) $ removeD xs)
removeD (Nothing:xs) = (map ([Nothing]++) $ removeD xs)

