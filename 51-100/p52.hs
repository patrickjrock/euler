import Data.Char
import Data.Set

digitize x = Prelude.map digitToInt $ show x 

sameDigits a b = (fromList da) == (fromList db)
  where da = digitize a
        db = digitize b 

candidate x = sameDigits x (2*x) &&
              sameDigits x (3*x) &&
              sameDigits x (4*x) &&
              sameDigits x (5*x) &&
              sameDigits x (6*x)

main = do print $ head $ Prelude.filter candidate [1..]
