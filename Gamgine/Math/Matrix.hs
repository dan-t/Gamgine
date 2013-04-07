
module Gamgine.Math.Matrix (
   module Data.Vec,
   Matrix,
   Window,
   Frustum(..),
   mkOrtho,
   mkScale,
   mkTranslate,
   mkWindowMatrix,
   mkWorldToWinMatrix,
   mkWinToWorldMatrix,
   winToWorld,
   inverseOrIdentity) where

import Data.Vec
import Gamgine.Math.Vect

-- row major matrix
type Matrix = Mat44 Double

type Width  = Int
type Height = Int
type Window = (Width, Height)

type X        = Int
type Y        = Int
type WinCoord = (X, Y)

data Frustum = Frustum {
   left   :: Double,
   right  :: Double,
   bottom :: Double,
   top    :: Double,
   near   :: Double,
   far    :: Double
   } deriving (Show, Eq)


mkOrtho :: Frustum -> Matrix
mkOrtho Frustum {left = l, right = r, bottom = b, top = t, near = n, far = f} =
   matFromList [2 / rml,       0,       0, -(rpl / rml),
                0      , 2 / tmb,       0, -(tpb / tmb),
                0      ,       0, 2 / fmn, -(fpn / fmn),
                0      ,       0,       0,            1]
   where
      rml = r - l
      rpl = r + l
      tmb = t - b
      tpb = t + b
      fmn = f - n
      fpn = f + n


mkScale :: Vect -> Matrix
mkScale v = scale (snoc v 1) identity 


mkTranslate :: Vect -> Matrix
mkTranslate v = translate v identity


mkWindowMatrix :: Window -> Matrix
mkWindowMatrix (width, height) = toGLFW dHeight `multmm` unitCubeToWin dWidth dHeight
   where
      unitCubeToWin :: Double -> Double -> Matrix
      unitCubeToWin width height = 
	 mkScale (v3 (width*0.5) (height*0.5) 0.5) `multmm` mkTranslate (v3 1 1 1)

      toGLFW :: Double -> Matrix
      toGLFW height = mkTranslate (v3 0 height 0) `multmm` mkScale (v3 1 (-1) 1) 

      dWidth  = fromIntegral width
      dHeight = fromIntegral height


mkWorldToWinMatrix :: Window -> Frustum -> Matrix
mkWorldToWinMatrix win frust = mkWindowMatrix win `multmm` mkOrtho frust


mkWinToWorldMatrix :: Window -> Frustum -> Matrix
mkWinToWorldMatrix win frust = inverseOrIdentity $ mkWorldToWinMatrix win frust


inverseOrIdentity :: Matrix -> Matrix
inverseOrIdentity m =
   case invert m of
	Nothing -> identity
	Just m  -> m


winToWorld :: Matrix -> WinCoord -> Vect
winToWorld winToWorldMatrix (x, y) = fromVect4 $ winToWorldMatrix `multmv` posVec
   where
      posVec = v4 (fromIntegral x) (fromIntegral y) 0 1
