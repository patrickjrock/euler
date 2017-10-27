import Math.Combinatorics.Binomial

p53 = length $ filter (>1000000) candidates
  where candidates = concatMap chelp [1..100]
        chelp n = map (choose n) [1..n]
