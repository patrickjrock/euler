import Data.List
import Data.Char

-- checks permutations
(<~>) :: Integer -> Integer -> Bool
l <~> m = (sort $ toDigits l) == (sort $ toDigits m)

splitPerms :: [Integer] -> [[Integer]]
splitPerms [] = []
splitPerms (x:xs) = as : splitPerms bs
  where (as,bs) = partition ((<~>) x) (x:xs)

isInt x = x == fromInteger (round x)
isCube x = (round ((fromIntegral x) ** (1/3)))^3 == x

cubes = map (^3) [1..]
cubes' n = takeWhile (f n) $ dropWhile (not . (f n)) cubes
  where f n = (\x -> (length $ toDigits x) == n)

toDigits :: Integer -> [Integer]
toDigits = map (fromIntegral . digitToInt) . show

fromDigits :: [Integer] -> Integer
fromDigits x = read $ concatMap show x :: Integer

candidate n = 3 == (length $ nub $ filter isCube perms)
  where perms = map fromDigits $ filter zero $ permutations $ toDigits (n^3)
        zero (x:xs) = if x == 0 then False else True

main = do print $ filter (\x-> length x == 5) $ splitPerms $ cubes' 12
