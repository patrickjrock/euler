import Data.Maybe

type Day = Int
data Month = January | February | March | April | May | June | July | August |
  September | October | November | December deriving (Show, Enum, Eq)
type Year = Int

data Date = Date Day Month Year deriving (Show, Eq)

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday |
  Sunday deriving (Show, Enum, Eq)

dateToDay :: Date -> Maybe Weekday
dateToDay (Date 1 January 1900) = Just Monday
dateToDay _ = Nothing 

daysInMonth :: Month -> Int
daysInMonth February = 28
daysInMonth September = 30
daysInMonth April = 30
daysInMonth June = 30
daysInMonth November = 30
daysInMonth _ = 31

leapYear :: Year -> Bool
leapYear y 
  | mod y 100 == 0 = mod y 400 == 0
  | otherwise = mod y 4 == 0 

nextDay :: (Date, Weekday) -> (Date, Weekday)
nextDay (d, Sunday) = (nextDate d, Monday)
nextDay (d, w) = (nextDate d, toEnum (1+(fromEnum w)))

nextDate :: Date -> Date
nextDate (Date 28 February y) = if (leapYear y) then (Date 29 February y) else
  (Date 1 March y)
nextDate (Date 29 February y) = Date 1 March y
nextDate (Date 31 December y) = Date 1 January (y+1)

nextDate (Date d m y)  
  | d == (daysInMonth m) = Date 1 (toEnum (1 + (fromEnum m))) y 
  | otherwise = Date (d+1) m y

days = iterate nextDay ((Date 1 January 1901), Tuesday)

lastDay (d,w) = d /= Date 31 December 2000
century = takeWhile lastDay days

firstSunday ((Date d m y), w) = (d == 1) && (w == Sunday)

main = do print $ length $ filter firstSunday century
