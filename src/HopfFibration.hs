module HopfFibration
  where
import           Control.Concurrent                (threadDelay)
import           Control.Monad                     (forM_, when)
import qualified Data.ByteString                   as B
import           Data.IORef
import           Data.Tuple.Extra                  (fst3, snd3, thd3)
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Linear                            (V3 (..))
import           System.Directory                  (doesDirectoryExist)
import           System.IO.Unsafe
import           Text.Printf
import           Utils.TransformationMatrix

type Point = (GLfloat,GLfloat,GLfloat)

ppmExists :: Bool
{-# NOINLINE ppmExists #-}
ppmExists = unsafePerformIO $ doesDirectoryExist "./ppm"

pointToV3 :: Point -> V3 GLfloat
pointToV3 (x,y,z) = V3 x y z

parametrization :: GLfloat -> GLfloat -> GLfloat -> Point
parametrization beta theta0 phi =
  (
    cos(theta0+beta) * cos phi / a,
    sin(theta0+beta) * cos phi / a,
    cos beta * sin phi / a
  )
  where
  a = 1 - sin beta * sin phi

myTriplets :: GLfloat -> GLfloat -> [(Point, Point, Point)]
myTriplets phi start = map triplet theta_
  where
    theta_ = [start + fromIntegral i * pi / 10.0 | i <- [0 .. 9]]
    triplet theta =
      (
        parametrization 0 theta phi,
        parametrization 2 theta phi,
        parametrization 4 theta phi
      )

myTriplets1 :: [(Point, Point, Point)]
myTriplets1 = myTriplets 2.4 (pi/2)

myTriplets2 :: [(Point, Point, Point)]
myTriplets2 = myTriplets 2.2 0

myTriplets3 :: [(Point, Point, Point)]
myTriplets3 = myTriplets 2.1 pi

data Context = Context
    {
      contextRot1 :: IORef GLfloat
    , contextRot2 :: IORef GLfloat
    , contextRot3 :: IORef GLfloat
    , contextZoom :: IORef Double
    }

white,black,blue,green,red :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1
blue       = Color4    0    0    1    1
green      = Color4    0    1    0    1
red        = Color4    1    0    0    1

tmatsAndRadii1 :: [([GLfloat], GLdouble)]
tmatsAndRadii1 = map (\triplet -> transformationMatrix
                                  (pointToV3 $ fst3 triplet)
                                  (pointToV3 $ snd3 triplet)
                                  (pointToV3 $ thd3 triplet)) myTriplets1

tmatsAndRadii2 :: [([GLfloat], GLdouble)]
tmatsAndRadii2 = map (\triplet -> transformationMatrix
                                  (pointToV3 $ fst3 triplet)
                                  (pointToV3 $ snd3 triplet)
                                  (pointToV3 $ thd3 triplet)) myTriplets2

tmatsAndRadii3 :: [([GLfloat], GLdouble)]
tmatsAndRadii3 = map (\triplet -> transformationMatrix
                                  (pointToV3 $ fst3 triplet)
                                  (pointToV3 $ snd3 triplet)
                                  (pointToV3 $ thd3 triplet)) myTriplets3

display :: Context -> IORef GLfloat -> DisplayCallback
display context alpha = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  zoom <- get (contextZoom context)
  alpha' <- get alpha
  (_, size) <- get viewport
  loadIdentity
  resize zoom size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  rotate alpha' $ Vector3 1 1 1
  forM_ tmatsAndRadii1 $ \tmatAndRadius ->
    preservingMatrix $ do
      m <- newMatrix RowMajor (fst tmatAndRadius) :: IO (GLmatrix GLfloat)
      multMatrix m
      materialDiffuse Front $= blue
      renderObject Solid $ Torus 0.1 (snd tmatAndRadius) 30 30
  forM_ tmatsAndRadii2 $ \tmatAndRadius ->
    preservingMatrix $ do
      m <- newMatrix RowMajor (fst tmatAndRadius) :: IO (GLmatrix GLfloat)
      multMatrix m
      materialDiffuse Front $= green
      renderObject Solid $ Torus 0.1 (snd tmatAndRadius) 30 30
  forM_ tmatsAndRadii3 $ \tmatAndRadius ->
    preservingMatrix $ do
      m <- newMatrix RowMajor (fst tmatAndRadius) :: IO (GLmatrix GLfloat)
      multMatrix m
      materialDiffuse Front $= red
      renderObject Solid $ Torus 0.1 (snd tmatAndRadius) 30 30
  swapBuffers

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-15+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef GLdouble -- zoom
         -> IORef Bool -- animation
         -> IORef Int -- thread delay (animation rate)
         -> IORef Bool -- save
         -> KeyboardCallback
keyboard rot1 rot2 rot3 zoom anim delay save c _ = do
  case c of
    'a' -> anim $~! not
    'o' -> delay $~! (+10000)
    'p' -> delay $~! (\d -> if d==0 then 0 else d-10000)
    's' -> save $~! not
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+2)
    'm' -> zoom $~! (+1)
    'l' -> zoom $~! subtract 1
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing

idle :: IORef Bool -> IORef Int -> IORef Bool -> IORef Int -> IORef GLfloat
     -> IdleCallback
idle anim delay save snapshots alpha = do
    a <- get anim
    snapshot <- get snapshots
    s <- get save
    when a $ do
      d <- get delay
      when (s && ppmExists && snapshot < 360) $ do
        let ppm = printf "ppm/fibration%04d.ppm" snapshot
        (>>=) capturePPM (B.writeFile ppm)
        print snapshot
        snapshots $~! (+1)
      alpha $~! (+1)
      _ <- threadDelay d
      postRedisplay Nothing
    return ()

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Hopf fibration"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= black
--  materialAmbient Front $= black
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
--  ambient (Light 0) $= black
--  diffuse (Light 0) $= white
--  specular (Light 0) $= black
  depthFunc $= Just Less
  shadeModel $= Smooth
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  alpha <- newIORef 0.0
  anim <- newIORef False
  delay <- newIORef 0
  save <- newIORef False
  snapshots <- newIORef 0
  displayCallback $= display Context { contextRot1 = rot1,
                                       contextRot2 = rot2,
                                       contextRot3 = rot3,
                                       contextZoom = zoom }
                             alpha
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 zoom anim delay save)
  idleCallback $= Just (idle anim delay save snapshots alpha)
  putStrLn "*** Hopf fibration ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Animation: a\n\
        \    Animation speed: o, p\n\
        \    Save animation: s\n\
        \"
  mainLoop
