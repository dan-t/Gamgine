{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Gamgine.Gfx where
import Graphics.Rendering.OpenGL.Raw
import Control.Monad (forM_)
import Data.Either
import Foreign.Marshal.Utils
import Foreign.Storable
import Data.Array.Storable
import System.IO
import Gamgine.Image.PNG
import qualified Gamgine.Math.Box as B
import Gamgine.Math.Vect
import Gamgine.Math.BoxTree as BT
import Gamgine.Control ((?))
#include "Gamgine/Utils.cpp"

type XYZ      = (Double, Double, Double)
type RGB      = (Double, Double, Double)
type RGBA     = (Double, Double, Double, Double)
type TexCoord = (Double, Double)

floatToFloat :: (RealFloat a, RealFloat b) => a -> b
floatToFloat = (uncurry encodeFloat) . decodeFloat


class Tuple4d a where
   t4d_first  :: a -> Double
   t4d_second :: a -> Double
   t4d_third  :: a -> Double
   t4d_forth  :: a -> Double

instance Tuple4d (Double, Double, Double, Double) where
   t4d_first  (f, _, _, _) = f
   t4d_second (_, s, _, _) = s
   t4d_third  (_, _, t, _) = t
   t4d_forth  (_, _, _, f) = f

instance Tuple4d (Vec4 Double) where
   t4d_first  (f:._)           = f
   t4d_second (_:.s:._)        = s
   t4d_third  (_:._:.t:._)     = t
   t4d_forth  (_:._:._:.f:.()) = f


class Tuple3d a where
   t3d_first  :: a -> Double
   t3d_second :: a -> Double
   t3d_third  :: a -> Double

instance Tuple3d (Double, Double, Double) where
   t3d_first  (f, _, _) = f
   t3d_second (_, s, _) = s
   t3d_third  (_, _, t) = t

instance Tuple3d (Vec3 Double) where
   t3d_first  (f:._)        = f
   t3d_second (_:.s:._)     = s
   t3d_third  (_:._:.t:.()) = t


class Tuple2d a where
   t2d_first  :: a -> Double
   t2d_second :: a -> Double

instance Tuple2d (Double, Double) where
   t2d_first  (f, _) = f
   t2d_second (_, s) = s

instance Tuple2d (Vec2 Double) where
   t2d_first  (f:._)     = f
   t2d_second (_:.s:.()) = s


(<<) :: Tuple2d a => (GLfloat -> GLfloat -> IO ()) -> a -> IO ()
f << a = f (floatToFloat $ t2d_first a)
           (floatToFloat $ t2d_second a)
infixl 5 <<


(<<<) :: Tuple3d a => (GLfloat -> GLfloat -> GLfloat -> IO ()) -> a -> IO ()
f <<< a = f (floatToFloat $ t3d_first a)
            (floatToFloat $ t3d_second a)
            (floatToFloat $ t3d_third a)
infixl 5 <<<


(<<<<) :: Tuple4d a => (GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> a -> IO ()
f <<<< a = f (floatToFloat $ t4d_first a)
             (floatToFloat $ t4d_second a)
             (floatToFloat $ t4d_third a)
             (floatToFloat $ t4d_forth a)
infixl 5 <<<<


quad :: (Double,Double) -> (Double,Double) -> [(Double,Double)]
quad (minx, miny) (maxx, maxy) =
   [(minx,miny), (maxx,miny), (maxx, maxy), (minx, maxy)]

quadTexCoords :: Double -> Double -> [(Double,Double)]
quadTexCoords maxx maxy = [(0,maxy), (maxx,maxy), (maxx,0), (0,0)]

draw :: Tuple3d a => GLenum -> [a] -> IO ()
draw primType vertices = withPrimitive primType $ mapM_ (glVertex3f <<<) vertices

drawBox :: B.Box -> IO ()
drawBox box = do
   drawQuad (B.minPt box) (B.maxPt box)

drawQuad :: Tuple3d a => a -> a -> IO ()
drawQuad min max = do
   draw gl_QUADS [(minX, minY, 0 :: Double), (maxX, minY, 0 :: Double),
                  (maxX, maxY, 0 :: Double), (minX, maxY, 0 :: Double)]
   where
      minX = t3d_first min
      minY = t3d_second min
      maxX = t3d_first max
      maxY = t3d_second max


drawBoxTree :: BT.BoxTree a -> IO ()
drawBoxTree tree = do
   go tree
   where
      go (Node box ts) = drawBox box >> mapM_ (\t -> go t) ts
      go (Leaf box _)  = drawBox box

drawPoint :: Tuple3d a => a -> RGB -> IO ()
drawPoint pos color = do
   glPointSize 10
   glBegin gl_POINTS
   glVertex3f <<< pos
   glEnd

withPrimitive :: GLenum -> IO () -> IO ()
withPrimitive primType act = do
   glBegin primType
   act
   glEnd

withPushedMatrix :: IO a -> IO a
withPushedMatrix act = do
   glPushMatrix
   a <- act
   glPopMatrix
   return a

withPolyMode :: GLenum -> IO () -> IO ()
withPolyMode mode act = do
   glPolygonMode gl_FRONT_AND_BACK mode
   act
   glPolygonMode gl_FRONT_AND_BACK gl_FILL

withEnabled :: GLenum -> IO () -> IO ()
withEnabled mode act = do
   glEnable mode
   act
   glDisable mode

withBlend :: GLenum -> GLenum -> IO () -> IO ()
withBlend srcFactor dstFactor act = do
   glBlendFunc srcFactor dstFactor
   withEnabled gl_BLEND act

withTexture2d :: GLuint -> IO () -> IO ()
withTexture2d id act = do
   glBindTexture gl_TEXTURE_2D id
   withEnabled gl_TEXTURE_2D act

makeTexture2d :: FilePath -> GLenum -> IO GLuint
makeTexture2d file wrapMode = do
   res <- loadPNGFile file
   either (\str -> ERROR str)
          (\img -> genTex img)
	  res
   where
      genTex img = do
	 let (width, height) = dimensions img
             imgData         = imageData img
	     format          = hasAlphaChannel img ? gl_RGBA $ gl_RGB
	 id <- with 0 (\buf -> glGenTextures 1 buf >> peek buf)
	 glBindTexture gl_TEXTURE_2D id
	 glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S (fromIntegral wrapMode)
	 glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T (fromIntegral wrapMode)
	 glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER (fromIntegral gl_NEAREST)
	 glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER (fromIntegral gl_NEAREST)
	 withStorableArray imgData (\array ->
	    glTexImage2D gl_TEXTURE_2D 0 (fromIntegral format) (fromIntegral width)
	                 (fromIntegral height) 0 (fromIntegral format) gl_UNSIGNED_BYTE array)
	 return id


renderTexturedQuad :: (Double,Double) -> GLuint -> IO ()
renderTexturedQuad size texture =
   withTexture2d texture $
      withBlend gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA $
         withPrimitive gl_QUADS $ do
            let coords   = quadTexCoords 1 1
                vertices = quad (0,0) size
            glColor3f <<< ((1, 1, 1) :: RGB)
            forM_ (zip coords vertices) (\(c,v) -> do
               glTexCoord2f << c
               glVertex2f << v)
