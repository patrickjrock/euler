import Data.Char
import Data.Bits
import Data.List.Split
import Data.List

decrypt :: [Int] -> String -> String
decrypt cipher key = map (\c->toEnum c :: Char) $ zipWith ($) xorfs cipher
  where xorfs = map xor $ cycle (map ord key)


keys = [map (\c->toEnum c :: Char) [a,b,c] |
        a <- [97..122],
        b <- [97..122],
        c <- [97..122]]

isEnglish :: String -> Bool
isEnglish = isInfixOf "Gospel"

main = do
  temp_cipher <- readFile "p059_cipher.txt"
  let cipher =  map (\c->read c :: Int) $ splitOn "," temp_cipher
  print $ sum $ map ord $ head $ filter isEnglish $ map (decrypt cipher) keys
