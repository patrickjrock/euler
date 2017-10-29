module Fraction where

data Number = Fraction Number Number | Sum Number Number | Whole Integer 

nmap f (Fraction a b) = Fraction (f a) (f b)
nmap f (Sum a b) = Sum (f a) (f b)
nmap f (Whole a) = Whole a

instance Show Number where 
  show (Whole a) = show a
  show (Fraction a b) = "(" ++ show a ++ "/" ++ show b ++ ")"
  show (Sum a b) =  "(" ++ show a ++ "+" ++ show b ++ ")"

multF :: Number -> Number -> Number
multF (Fraction a b) (Fraction c d) = Fraction (multF a c) (multF b d)
multF (Whole a) (Whole b) = Whole (a*b)
multF (Fraction a b) (Whole c) = Fraction (multF a (Whole c)) b
multF (Whole a) (Fraction b c) = Fraction (multF (Whole a) b) c

addF :: Number -> Number -> Number
addF (Fraction a b) (Fraction c d) = Fraction n d 
  where n = addF (multF a d)  (multF c b)  
        d = multF b d
addF (Whole a) (Whole b) = Whole (a+b)
addF (Whole a) (Fraction b c) = Fraction n c
  where n = addF (multF (Whole a) c) b
addF (Fraction a b) (Whole c) = addF (Whole c) (Fraction a b)

simplify a = simplify' $ nmap simplify' a  

simplify' :: Number -> Number
simplify' (Whole a) = Whole a 
simplify' (Sum a b) = addF a b
simplify' (Fraction (Whole a) (Whole b)) 
  | rem a b == 0 = Whole (div a b)
  | otherwise = Fraction (Whole a) (Whole b)

simplify' (Fraction a (Fraction b c)) = simplify' (multF a (Fraction c b))
simplify' (Fraction a (Whole b)) = 
  simplify' (multF a (Fraction (Whole 1) (Whole b)))

simplify' (Fraction a b) =  simplify' (Fraction (simplify a) (simplify b))


