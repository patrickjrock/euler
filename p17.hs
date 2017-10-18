import Data.Char

digitize x = map digitToInt $ show x 

fix [x] = [x]
fix [x, y] = [10*x, y]
fix [x,y,z] = [100*x, 10*y, z]

wordify :: Int -> String
wordify 0 = ""
wordify 1 = "one"
wordify 2 = "two"
wordify 3 = "three"
wordify 4 = "four"
wordify 5 = "five"
wordify 6 = "six"
wordify 7 = "seven"
wordify 8 = "eight"
wordify 9 = "nine"
wordify 10 = "ten"
wordify 11 = "eleven"
wordify 12 = "twelve"
wordify 13 = "thirteen"
wordify 14 = "fourteen"
wordify 15 = "fifteen"
wordify 16 = "sixteen"
wordify 17 = "seventeen"
wordify 18 = "eighteen"
wordify 19 = "nineteen"
wordify 20 = "twenty"
wordify 30 = "thirty"
wordify 40 = "forty"
wordify 50 = "fifty"
wordify 60 = "sixty"
wordify 70 = "seventy"
wordify 80 = "eighty"
wordify 90 = "ninety"
wordify 1000 = "onethousand"

wordify n 
  | mod n 100 == 0 = (wordify (div n 100)) ++ "hundredand"
  | otherwise = foldr (++) [] (map wordify digits)
  where digits = (fix $ digitize n)
