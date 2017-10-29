import Data.List

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Eq, Show)
type Value = Int
data Card = Card Suit Value deriving Show
type Hand = [Card] 
data Rank = HighCard | OnePair | TwoPair | ThreeKind | Straight |
            Flush | FullHouse | FourKind | StraightFlush | RoyalFlush

suit :: Card -> Suit
suit (Card s v) = s

value :: Card -> Value
value (Card s v) = v

sameSuit :: Card -> Card -> Bool
sameSuit a b = (suit a) == (suit b)

flush :: Hand -> Bool
flush h = (length $ nub $ map suit h) == 1

straight h = isInfixOf (map value h) [1..])

royalFlush :: Hand -> Bool
royalFlush h = royal && same 
  where royal = map value h == [10,11,12,13,14] 
        same = flush h

straightFlush h = (flush h) && (flush h)

onePair h = 4 == (length $ nub $ map value h)

