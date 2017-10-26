import Data.List
import Data.Ord

euclid (m,n,k) =  (abs $ (m*m - n*n)*k, 2*m*n*k, (m*m+n*n)*k)

triples = map euclid [(m,n,k) | m <- [1..23],
                                  n <- [1..499],
                                  k <- [1..100],
                                  m /= n]


eqTriples (a,b,c) (d,e,f) = (a==e && b==d) || (a==d && b==e)

perimiter p (a,b,c) = a+b+c == p

rightTriangles p = nubBy eqTriples $ filter (perimiter p) triples
candidates = zip [1..1000] $ length <$> rightTriangles <$> [1..1000]


main = do print $ sortBy (comparing snd) $ candidates  
