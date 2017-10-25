import Data.List

euclid (m,n,k) =  (abs $ (m*m - n*n)*k, 2*m*n*k, (m*m+n*n)*k)

triples l = map euclid [(m,n,k) | m <- [1..l],
                                                    n <- [1..l],
                                                    k <- [1..5],
                                                    m /= n]


eqTriples (a,b,c) (d,e,f) = (a==e && b==d) || (a==d && b==e)

perimiter p (a,b,c) = a+b+c == p

rightTriangles n p = nubBy eqTriples $ filter (perimiter p) (triples n)
