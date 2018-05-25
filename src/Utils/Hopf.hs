module Utils.Hopf
  where

hopfinverse :: (Num a, Floating a) => a -> a -> a -> a -> (a,a,a,a)
hopfinverse q0 q1 q2 t =
  let h = (1 / sqrt (2 * (1+q2))) in
  (h* (cos t * q0 + sin t * q1),
   h * sin t * (1+q2),
   h * cos t * (1+q2),
   h * (sin t * q0 - cos t * q1))

-- hopfinverse' :: [GLfloat] -> GLfloat -> Vertex3 GLfloat
-- hopfinverse' q t = toVx3 (hopfinverse q t)
--    where
--      toVx3 x = Vertex3 (x!!0) (x!!1) (x!!2)

stereoProj :: (Num a, Fractional a) => (a,a,a,a) -> (a,a,a)
stereoProj (x0,x1,x2,x3) =
  let h = 1/(1-x3) in
  (h * x0, h * x1, h * x2)

stereoCircHinv :: (Num a, Floating a) => a -> a -> a -> (a, a, a)
stereoCircHinv theta phi xi =
  stereoProj (hopfinverse q0 q1 q2 xi)
  where
    q0 = cos theta * cos phi
    q1 = sin theta * cos phi
    q2 = sin phi

stereoCircHinv' :: (Num a, Floating a) => a -> a -> a -> (a, a, a)
stereoCircHinv' theta phi xi =
  stereoProj (hopfinverse q0 q1 q2 xi)
  where
    q0 = cos theta * sin phi
    q1 = sin theta * sin phi
    q2 = cos phi

stereoCircHinv'' :: (Num a, Floating a) => a -> a -> a -> a -> (a, a, a)
stereoCircHinv'' q0 q1 q2 xi =
  stereoProj (hopfinverse q0 q1 q2 xi)
