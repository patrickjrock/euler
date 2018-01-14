coins = [1..99]

changeN 0 x = 1
changeN n x  = sum $ map (changeN (n-1))  [x-a*coin | a <- [0..lim] ]
  where coin = coins !! n 
        lim = div x coin

change = changeN 98
