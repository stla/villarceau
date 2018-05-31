module LinkedHopfTori
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
import           Utils.Hopf
import           Utils.Rotation4D
import           Utils.TransformationMatrix

type Point = (GLfloat,GLfloat,GLfloat)

pointToV3 :: Point -> V3 GLfloat
pointToV3 (x,y,z) = V3 x y z

tripletOnCircle :: GLfloat -> GLfloat -> (Point, Point, Point)
tripletOnCircle phi phi0 theta =
  ( stereoProj (r (hopfinverse' theta phi 0))
  , stereoProj (r (hopfinverse' theta phi 2))
  , stereoProj (r (hopfinverse' theta phi 4)))
  where
    r = rotate4D (pi/2) phi0 1

myTriplets :: GLfloat -> GLfloat -> [(Point, Point, Point)]
myTriplets phi phi0 = map (tripletOnCircle phi phi0) theta_
  where
    n = 100
    theta_ = [2 * pi * frac i n | i <- [0 .. n-1]]
    frac :: Int -> Int -> GLfloat
    frac p q = realToFrac p / realToFrac q
