
module Gamgine.Engine where
import Graphics.UI.GLFW (getTime)
import Control.Monad.State (MonadIO, liftIO)


mkUpdateLoop :: (MonadIO m) => Int -> Int -> m a -> (Double -> m (Double, Double))
mkUpdateLoop ticksPerSecond maxFrameSkip update = \nextFrame -> loop nextFrame 0
   where
      loop nextFrame skippedFrames = do
	 time <- liftIO getTime
	 if time > nextFrame && skippedFrames < maxFrameSkip
	    then do
	       update
	       loop (nextFrame + skipTicks) (skippedFrames + 1)
	    else do
	       let interpol = interpolation time nextFrame skipTicks
	       return (nextFrame, interpol)

      interpolation time nextFrame skipTicks =
	 (time - skipTicks - nextFrame) / skipTicks

      skipTicks = 1 / (fromIntegral ticksPerSecond :: Double)
