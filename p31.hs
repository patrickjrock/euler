coinSum (a,b,c,d,e,f,g) = a + 2*b + 5*c + 10*d + 20*e + 50*f + 100*g == 200

candidates = [(a,b,c,d,e,g,f) | a <- [0..200],
                                b <- [0..100],
                                c <- [0..40],
                                d <- [0..20],
                                e <- [0..10],
                                f <- [0..4],
                                g <- [0..2]]

p31 = filter coinSum candidates

changex :: Int -> Int -> [[Int]] 
changex x n 
  | n-x < 0 = [[]]
  | n-x == 0 = [[n]]
  | otherwise = map  (++ [x]) (change (n-x))

coins = [1,2,5,10,20,50,100,200] 

change :: Int -> [[Int]]
change n = concatMap ($n) changefs
  where changefs = map changex $ filter (>=n) coins
