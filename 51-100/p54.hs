import Data.List

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Eq, Show)
type Value = Int
data Card = Card Suit Value deriving Show
type Hand = [Card] 
data Rank = HighCard Int | OnePair | TwoPair | ThreeKind | Straight |
            Flush | FullHouse | FourKind | StraightFlush |
            RoyalFlush deriving Show

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

onePair h = 4 == (length $ nub $ map value h)
twoPair h = 4 == (length $ filter (==2) $ zipWith ($) (map count vs) (take 5 $ repeat vs))
  where vs = map value h

nKind n h = n == (length $ filter (==n) $ counts)
  where counts = zipWith ($) (map count vs) (take 5 $ repeat vs)
        vs = map value h

threeKind = nKind 3
fourKind = nKind 4

fullHouse h = (threeKind h) && (onePair h)

count :: Eq a => a -> [a] -> Int
count _ [] = 0
count x (y:ys) 
  | x == y = 1 + (count x ys)
  | otherwise = count x ys

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
  | otherwise = HighCard (maximum $ map value h)
