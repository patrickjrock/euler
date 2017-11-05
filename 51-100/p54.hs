import Data.List
import Data.Char
import Data.List.Split

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Eq, Show)
type Value = Int
data Card = Card Suit Value deriving Show
type Hand = [Card] 
data Rank = HighCard | OnePair | TwoPair | ThreeKind | Straight |
            Flush | FullHouse | FourKind | StraightFlush |
            RoyalFlush deriving (Show, Eq, Ord)


compareH (h1,h2) 
  | rank h1 > rank h2 = True
  | rank h2 > rank h1 = False
  | rank h1 == rank h2 = highC h1 h2
  where highC h1 h2 = (sort $ vs h1) > (sort $ vs h2)
        vs h = map value h
        
twoPairHand = [Card Hearts 1, Card Diamonds 1, Card Hearts 2, Card Diamonds 2,
               Card Clubs 10]

onePairHand = [Card Hearts 3, Card Diamonds 1, Card Hearts 2, Card Diamonds 2,
               Card Clubs 10]

suit :: Card -> Suit
suit (Card s v) = s

value :: Card -> Value
value (Card s v) = v

sameSuit :: Card -> Card -> Bool
sameSuit a b = (suit a) == (suit b)

flush :: Hand -> Bool
flush h = (length $ nub $ map suit h) == 1

straight h = isInfixOf (map value h) [1..14]

royalFlush :: Hand -> Bool
royalFlush h = royal && same 
  where royal = map value h == [10,11,12,13,14] 
        same = flush h

straightFlush h = (flush h) && (flush h)

count a xs = length $ filter (==a) xs

pairs h = filter isPair (nub vs)
  where vs = map value h
        isPair a = 2 == ((flip count) vs $ a)

onePair h = 4 == (length $ nub $ map value h)
twoPair h = 4 == (length $ filter (==2) $ zipWith ($) (map count vs) (take 5 $ repeat vs))
  where vs = map value h

nKind n h = n == (length $ filter (==n) $ counts)
  where counts = zipWith ($) (map count vs) (take 5 $ repeat vs)
        vs = map value h

threeKind = nKind 3
fourKind = nKind 4

fullHouse h = (threeKind h) && (onePair h)

rank :: Hand -> Rank
rank h 
  | royalFlush h = RoyalFlush
  | straightFlush h = StraightFlush
  | fourKind h = FourKind
  | fullHouse h = FullHouse
  | flush h = Flush
  | straight h = Straight
  | threeKind h = ThreeKind
  | twoPair h = TwoPair
  | onePair h = OnePair 
  | otherwise = HighCard 

readV 'T' = 10
readV 'J' = 11
readV 'Q' = 12
readV 'K' = 13
readV 'A' = 14
readV v = digitToInt v

readCard :: String -> Card
readCard [v,'H'] = Card Hearts (readV v)
readCard [v,'D'] = Card Diamonds (readV v)
readCard [v,'S'] = Card Spades (readV v)
readCard [v,'C'] = Card Clubs (readV v)

readHand :: String -> (Hand,Hand)
readHand s = (map readCard (take 5 cs), map readCard (drop 5 cs))
  where cs = splitOn " " s

main = do 
  f <- readFile "p054_poker.txt"
  let lines = splitOn "\n" f
  print $ length $ filter compareH $ map readHand (init lines)
