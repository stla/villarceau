module Lib
  where
import           Data.Foldable                           (toList)
import           Graphics.Rendering.OpenGL.GL
import           Linear                            hiding (lookAt, perspective,
                                                    rotate)
import           Data.IORef
import           Graphics.UI.GLUT
import qualified Data.List as L


plane3pts :: Num a => V3 a -> V3 a -> V3 a -> (V3 a, a)
plane3pts p1 p2 p3 = (V3 xcoef ycoef zcoef, offset)
  where
    V3 p1x p1y p1z = p1
    V3 p2x p2y p2z = p2
    V3 p3x p3y p3z = p3
    xcoef = (p1y-p2y)*(p2z-p3z)-(p1z-p2z)*(p2y-p3y)
    ycoef = (p1z-p2z)*(p2x-p3x)-(p1x-p2x)*(p2z-p3z)
    zcoef = (p1x-p2x)*(p2y-p3y)-(p1y-p2y)*(p2x-p3x)
    offset = p1x*xcoef + p1y*ycoef + p1z*zcoef

plane1ptnormal :: Num a => V3 a -> V3 a -> (V3 a, a)
plane1ptnormal p normal = (normal, p `dot` normal)

circleCenterRadius :: (Num a, Fractional a, Floating a) => V3 a -> V3 a -> V3 a
                   -> ((V3 a, a), V3 a)
circleCenterRadius p1 p2 p3 = ((center, radius), coefs1)
  where
    p12 = (p1 ^+^ p2) ^/ 2
    p23 = (p2 ^+^ p3) ^/ 2
    v12 = p2 ^-^ p1
    v23 = p3 ^-^ p2
    (coefs1, offset1) = plane3pts p1 p2 p3
    (coefs2, offset2) = plane1ptnormal p12 v12
    (coefs3, offset3) = plane1ptnormal p23 v23
    a = V3 coefs1 coefs2 coefs3
    b = V3 offset1 offset2 offset3
    center = inv33 a !* b
    op1 = p1 ^-^ center
    radius = norm op1

transformationMatrix :: (Real a, Floating a) => V3 a -> V3 a -> V3 a
                     -> ([a], GLdouble)
transformationMatrix p1 p2 p3 =
  -- (concatMap toList (toList (mkTransformationMat m center)), radius')
  (concat $ L.transpose $ map toList (toList m), radius')
  where
    ((center, radius), plane) = circleCenterRadius p1 p2 p3
    radius' = realToFrac radius
    V3 a b c = plane
    measure = norm plane
    a' = a / measure
    b' = b / measure
    c' = c / measure
    n = V3 a' b' c'
--    s = sqrt (a'*a'+c'*c')
--    a'' = a'/s
--    c'' = c'/s
--    u = V3 (-c'') 0 a''
    -- s = sqrt (b'*b'+c'*c')
    -- b'' = b'/s
    -- c'' = c'/s
    -- u = V3 0 c'' (-b'')
    s = sqrt (a'*a'+b'*b')
    a'' = a'/s
    b'' = b'/s
    u = V3 (-b'') a'' 0

    v = cross n u
    m = V4 (V4 b'' (-a'') 0 0) (V4 a' b' c' 0) (V4 v1 v2 v3 0) (V4 0 0 0 1)
    -- rotMat = V3 (V3 1 0 0) (V3 0 0 1) (V3 0 1 0) -- exchange y z
    -- rotMat = V3 (V3 0 0 1) (V3 0 1 0) (V3 1 0 0) -- exchange x z
    -- rotMat = V3 (V3 0 1 0) (V3 1 0 0) (V3 0 0 1) -- exchange x y
    -- m' = m !*! rotMat
      where
      V3 v1 v2 v3 = v

data Context = Context
    {
      contextRot1 :: IORef GLfloat
    , contextRot2 :: IORef GLfloat
    , contextRot3 :: IORef GLfloat
    , contextZoom :: IORef Double
    }

white,black,red,green,blue :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1
red        = Color4    1    0    0    1
green      = Color4    0    1    0    0.5
blue       = Color4    0    0    1    0.5

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [(-1,0,1),(2,1,2),(2,0,3)] -- issue if in plane z=0

myPointsV3 :: [V3 GLfloat]
myPointsV3 = map toV3 myPoints
  where
    toV3 (x,y,z) = V3 x y z



tmatAndRadius :: ([GLfloat], GLdouble)
tmatAndRadius = transformationMatrix
                (myPointsV3!!0) (myPointsV3!!1) (myPointsV3!!2)

transfo :: IO ()
transfo = do
  m <- newMatrix RowMajor (fst tmatAndRadius) :: IO (GLmatrix GLfloat)
  mm <- getMatrixComponents RowMajor m
  print mm
  multMatrix m

radius :: GLdouble
radius = snd tmatAndRadius

display :: Context -> DisplayCallback
display context = do
  putStrLn "radius:"
  print radius
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
  preservingMatrix $ do
    transfo
    materialDiffuse FrontAndBack $= blue
    renderObject Solid $ Torus 0.1 radius 30 30
  renderPrimitive Triangles $ do
    materialDiffuse FrontAndBack $= red
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  -- preservingMatrix $ do
  --   materialDiffuse FrontAndBack $= green
  --   renderObject Solid $ Torus 0.1 2 30 30
  swapBuffers

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-25+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h


keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef GLdouble -- zoom
         -> IORef Bool     -- animation
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



main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Villarceau Circles"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient FrontAndBack $= black
--  materialShininess FrontAndBack $= 95
--  materialSpecular FrontAndBack $= white
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
  anim <- newIORef False
  snapshots <- newIORef 0
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom }
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 zoom anim)
  idleCallback $= Nothing -- Just (idle anim a n m quadsRef snapshots)
  putStrLn "*** Villarceau Circles ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Animation: a \n\
        \"
  mainLoop
