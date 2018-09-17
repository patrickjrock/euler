import Data.Complex as C
import Graphics.Image
import Control.Monad
import Numeric

diverge :: RealFloat a => Complex a -> Int -> Complex a -> Int 
diverge x escape c = diverge' 0 (orbit c)
  where diverge' n (o:os) 
          | n == escape = n
          | C.magnitude o >= 2 = n
          | otherwise = diverge' (n+1) os
        orbit c = iterate (f c) 0
        f c z = z**x + c

mandelbrot :: RealFloat a => Complex a -> Int -> (Int,Int) -> Int -> Double
mandelbrot x escape (i,j) max = (fromIntegral p) / (fromIntegral escape)
  where p = diverge x escape $ (transform j) :+ (transform i)
        transform x = ((4* (fromIntegral x))/(fromIntegral max)) - 2

points :: [Complex Double]
points = (/100) <$> fromIntegral <$> [1..1000]

main = do 
  let grad_gray x = makeImageR VU (1000, 1000) (\(i, j) -> PixelY $ (mandelbrot x 50 (i,j) 1000)) / 1 
  let write x = writeImage ("images/brot_" ++ (Numeric.showFFloat (Just 2) (C.realPart x) "") ++ ".png") (grad_gray x)
  write 1000

