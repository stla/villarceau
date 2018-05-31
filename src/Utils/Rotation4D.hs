module Utils.Rotation4D
  where

rotate4D :: (Num a, Floating a) => a -> a -> a -> (a,a,a,a) -> (a,a,a,a)
rotate4D theta phi alpha (p,q,r,s) =
  [ a*p - b*q - c*r - d*s
  , a*q + b*p + c*s - d*r
  , a*r - b*s + c*p + d*q
  , a*s + b*r - c*q + d*p ]
  where
    a = cos alpha
    b = sintheta * cos phi * sinalpha
    sintheta = sin theta
    sinalpha = sin alpha
    c = sintheta * sin phi * sinalpha
    d = cos theta * sinalpha
