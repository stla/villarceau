module Utils.Colour
  where
import           Data.Colour.Palette.RandomColor (randomCIELab)
import           Data.Colour.SRGB.Linear         (channelBlue, channelGreen,
                                                  channelRed, toRGB)
import           Graphics.Rendering.OpenGL.GL    (Color4 (..), GLfloat)

pickColor :: IO (Color4 GLfloat)
pickColor = do
  kolor <- randomCIELab
  let rgb = toRGB kolor
      r = realToFrac $ channelRed rgb
      g = realToFrac $ channelGreen rgb
      b = realToFrac $ channelBlue rgb
  return $ Color4 r g b 1
