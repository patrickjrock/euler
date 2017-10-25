import Data.List

triangular n = n*(n+1)/2
pentagonal n = n*(3*n-1)/2
hexagonal n = n*(2*n-1)

triangulars = map triangular [1..]
pentagonals= map pentagonal [1..]
hexagonals = map hexagonal [1..]

tripent n = elem t $ takeWhile (<=t) $ drop (div n 2) pentagonals
  where t = triangulars !! n 

penthex n = elem p $ takeWhile (<=p) $ drop (5*(div n 6)) hexagonals
  where p = pentagonals !! n 


