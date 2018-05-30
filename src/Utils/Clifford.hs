module Utils.Clifford
  where

clifford :: (Num a, Floating a) => a -> a -> a -> (a,a,a)
clifford beta theta phi =
  (  cos(theta+beta) * cosphi / d
   , sin(theta+beta) * cosphi / d
   , cos beta * sinphi / d  )
  where
    cosphi = cos phi
    sinphi = sin phi
    d = 1 - sin beta * sinphi
