import Fraction
import Utils

type ContinuedFraction = (Integer, [Integer])

wholeFraction :: Number -> Bool
wholeFraction (Fraction (Whole a) (Whole b)) = True
wholeFraction _ = False

inverse :: Integer -> Number
inverse x = Fraction (Whole 1) (Whole x)

half = Fraction (Whole 1) (Whole 2)

numerator (Whole a) = a
numerator (Fraction (Whole a) (Whole b)) = a 
numerator f = numerator $ simplify f

expand :: Integer -> Number -> Number
expand x (Fraction (Whole a) (Whole b)) =
  Fraction (Whole a) (Sum (Whole b) (inverse x))
expand x (Sum a b) = Sum a (expand x b)
expand x (Fraction a b) = Fraction a (expand x b)

nthConvergent :: ContinuedFraction -> Integer -> Number
nthConvergent (a,_) 0 = Whole a
nthConvergent (a,bs) n = Sum (Whole a) fs 
  where expfs = map expand (tail bs) 
        fs = foldr ($) (inverse $ head bs) (reverse $ take n' expfs) 
        n' = fromIntegral (n-1)

interpose l [] = []
interpose l (x:xs) = [x] ++ l ++ (interpose l xs)

root2 = (1, repeat 2)
e = (2, 1:(interpose [1,1] [2,4..]))

