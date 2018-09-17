import Data.List
import Data.Char
import Data.List.Split

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Eq, Show)
type Value = Int
data Card = Card Suit Value deriving (Show, Eq)
type Hand = [Card] 
data Rank = HighCard Value | OnePair Value | TwoPair Value | ThreeKind Value | 
            Straight Value | Flush Value | FullHouse Value Value | FourKind Value |
            StraightFlush Value | RoyalFlush  deriving (Show, Eq, Ord)

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

highCard :: Hand -> Hand -> Bool
highCard h1 h2 = (highCard' h1) < (highCard' h2)
  where highCard' hand = maximum $ map value hand

-- counts the number of times each card's value occurs
count :: Hand -> [(Card, Int)]
count h = zip h counts
  where count' a xs = length $ filter (==a) xs
        counts = [f vals | f <- (map count' vals)]
        vals = map value h

--onePair :: Hand -> Hand -> Bool
--onePair h1 h2 = 
--  where onePair' 
        
