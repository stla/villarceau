module HopfTorus1
  where
import           Control.Monad                (forM_)
import           Data.IORef
import           Data.Tuple.Extra             (fst3, snd3, thd3)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Linear                       (V3 (..))
import           Utils.Hopf
import           Utils.TransformationMatrix

type Point = (GLfloat,GLfloat,GLfloat)

pointToV3 :: Point -> V3 GLfloat
pointToV3 (x,y,z) = V3 x y z

tripletOnCircle :: GLfloat -> (Point, Point, Point)
tripletOnCircle theta =
  (stereoCircHinv theta 0 0, stereoCircHinv theta 0 2, stereoCircHinv theta 0 4)

myTriplets :: [(Point, Point, Point)]
myTriplets = map tripletOnCircle theta_
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

tmatsAndRadii :: [([GLfloat], GLdouble)]
tmatsAndRadii = map (\triplet -> transformationMatrix
                                 (pointToV3 $ fst3 triplet)
                                 (pointToV3 $ snd3 triplet)
                                 (pointToV3 $ thd3 triplet)) myTriplets

display :: Context -> DisplayCallback
display context = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  zoom <- get (contextZoom context)
  (_, size) <- get viewport
  loadIdentity
  resize zoom size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  forM_ tmatsAndRadii $ \tmatAndRadius ->
    preservingMatrix $ do
      m <- newMatrix RowMajor (fst tmatAndRadius) :: IO (GLmatrix GLfloat)
      multMatrix m
      materialDiffuse FrontAndBack $= blue
      renderObject Solid $ Torus 0.1 (snd tmatAndRadius) 30 30
  swapBuffers

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-20+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef GLdouble -- zoom
         -> KeyboardCallback
keyboard rot1 rot2 rot3 zoom c _ = do
  case c of
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

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Torus passing by three points"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient FrontAndBack $= black
  lighting $= Enabled
  lightModelTwoSide $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= black
  depthFunc $= Just Less
  shadeModel $= Smooth
  blend $= Enabled    -- allow transparency
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  displayCallback $= display Context { contextRot1 = rot1,
                                       contextRot2 = rot2,
                                       contextRot3 = rot3,
                                       contextZoom = zoom }
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 zoom)
  idleCallback $= Nothing
  putStrLn "*** Torus passing by three points ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \"
  mainLoop
