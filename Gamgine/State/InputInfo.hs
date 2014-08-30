
module Gamgine.State.InputInfo where
import qualified Graphics.UI.GLFW as GLFW
import Gamgine.Control ((?))
import qualified Gamgine.Math.Vect as V
import Control.Applicative ((<$>), (<*>))

data Modifier = Ctrl | Alt | Shift deriving (Eq, Ord)

-- | mouse position in world coordinates
type MousePos = V.Vect

-- | if the key/mouse button was pressed or released
data InputState = Pressed | Released deriving (Eq, Ord)

pressedModifiers :: GLFW.Window -> IO [Modifier]
pressedModifiers win = do
   ctrlPressed  <- isCtrlPressed win
   shiftPressed <- isShiftPressed win
   altPressed   <- isAltPressed win
   return $ [Ctrl | ctrlPressed] ++ [Shift | shiftPressed] ++ [Alt | altPressed]

isCtrlPressed, isAltPressed, isShiftPressed :: GLFW.Window -> IO Bool
isCtrlPressed  win = (||) <$> isKeyPressed win GLFW.Key'LeftControl <*> isKeyPressed win GLFW.Key'RightControl
isAltPressed   win = (||) <$> isKeyPressed win GLFW.Key'LeftAlt     <*> isKeyPressed win GLFW.Key'RightAlt
isShiftPressed win = (||) <$> isKeyPressed win GLFW.Key'LeftShift   <*> isKeyPressed win GLFW.Key'RightShift

isKeyPressed :: GLFW.Window -> GLFW.Key -> IO Bool
isKeyPressed win key = (== GLFW.KeyState'Pressed) <$> GLFW.getKey win key
