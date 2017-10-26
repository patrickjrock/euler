import Data.Char

isTeen x = x > 10 && x < 20
digitize x = map digitToInt $ show x 
separate x 
  | isTeen $ mod x 100 = (100 * (head digits)) : [mod x 100]
  | otherwise = reverse $ zipWith (*) [1, 10, 100] (reverse digits)
  where digits = digitize x
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
  | n < 100 = concatMap wordify digits
  | mod n 100 == 0 = wordify (div n 100) ++ "hundred"
  | otherwise = concatMap wordify $ digits
  where digits = separate n 

main = do print $ (9*99*3) + (length $ concatMap wordify [1..1000])
