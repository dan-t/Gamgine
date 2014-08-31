
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

isModifierPressed :: Modifier -> GLFW.ModifierKeys -> Bool
isModifierPressed Ctrl  = GLFW.modifierKeysControl
isModifierPressed Alt   = GLFW.modifierKeysAlt
isModifierPressed Shift = GLFW.modifierKeysShift
