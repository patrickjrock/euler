isTriple (a,b,c) = if a*a + b*b == c*c then True else False 

euclid (m,n,k) = (m*m - n*n, 2*m*n, m*m+n*n)

triples n = map euclid [(m,n,k) | m <- [1..n],
                                  n <- [1..n],
                                  k <- [1..n]]

main = do print $ head $ filter (\(a,b,c) -> a+b+c == 1000) (triples 20)

