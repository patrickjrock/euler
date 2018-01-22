-- prime generating integers
import Math.NumberTheory.Primes
import Math.NumberTheory.GCD
import Math.NumberTheory.ArithmeticFunctions

primeGen n = and $ isPrime <$> f <$> divisorsList n
  where f x = x + (div n x)
