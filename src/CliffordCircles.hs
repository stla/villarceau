module CliffordCircles
  where
import           Control.Monad                     (forM_, when)
import qualified Data.ByteString                   as B
import           Data.IORef
import           Data.Tuple.Extra                  (fst3, snd3, thd3)
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Linear                            (V3 (..))
import           Text.Printf
import           Utils.Clifford
import           Utils.TransformationMatrix

type Point = (GLfloat,GLfloat,GLfloat)

pointToV3 :: Point -> V3 GLfloat
pointToV3 (x,y,z) = V3 x y z

tripletOnCircle :: GLfloat -> GLfloat -> (Point, Point, Point)
tripletOnCircle phi theta =
  (clifford 0 theta phi, clifford 2 theta phi, clifford 4 theta phi)

myTriplets :: GLfloat -> [(Point, Point, Point)]
myTriplets phi = map (tripletOnCircle phi) theta_
  where
    n = 30
    theta_ = [2 * pi * frac i n | i <- [0 .. n-1]]
    frac :: Int -> Int -> GLfloat
    frac p q = realToFrac p / realToFrac q

data Context = Context
    {
      contextRot1 :: IORef GLfloat
    , contextRot2 :: IORef GLfloat
    , contextRot3 :: IORef GLfloat
    , contextZoom :: IORef Double
    }

white,black,blue :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1
blue       = Color4    0    0    1    1

tmatsAndRadii :: GLfloat -> [([GLfloat], GLdouble)]
tmatsAndRadii phi = map (\triplet -> transformationMatrix
                                     (pointToV3 $ fst3 triplet)
                                     (pointToV3 $ snd3 triplet)
                                     (pointToV3 $ thd3 triplet))
                        (myTriplets phi)

display :: Context -> IORef GLfloat -> DisplayCallback
display context alpha = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  zoom <- get (contextZoom context)
  alpha' <- get alpha
  let phi = atan alpha'
  (_, size) <- get viewport
  loadIdentity
  resize zoom size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  forM_ (tmatsAndRadii phi) $ \tmatAndRadius ->
    preservingMatrix $ do
      m <- newMatrix RowMajor (fst tmatAndRadius) :: IO (GLmatrix GLfloat)
      multMatrix m
      materialDiffuse Front $= blue
      renderObject Solid $ Torus 0.1 (snd tmatAndRadius) 30 30
  swapBuffers

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-10+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef GLdouble -- zoom
         -> IORef Bool -- animation
         -> KeyboardCallback
keyboard rot1 rot2 rot3 zoom anim c _ = do
  case c of
    'a' -> writeIORef anim True
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

idle :: IORef Bool -> IORef Int -> IORef GLfloat -> IdleCallback
idle anim snapshots alpha = do
    a <- get anim
    s <- get snapshots
    when a $ do
      when (s < 100) $ do
        let ppm = printf "ppm/clifford%04d.ppm" s
        (>>=) capturePPM (B.writeFile ppm)
      snapshots $~! (+ 1)
      alpha $~! (+ 1/10)
      postRedisplay Nothing
    return ()

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Clifford circles"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient Front $= black
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= black
  depthFunc $= Just Less
  shadeModel $= Smooth
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  alpha <- newIORef 0.1
  anim <- newIORef False
  snapshots <- newIORef 0
  displayCallback $= display Context { contextRot1 = rot1,
                                       contextRot2 = rot2,
                                       contextRot3 = rot3,
                                       contextZoom = zoom }
                             alpha
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 zoom anim)
  idleCallback $= Just (idle anim snapshots alpha)
  putStrLn "*** Clifford circles ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Animation: a\n\
        \    Zoom: l, m\n\
        \"
  mainLoop
