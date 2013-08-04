
module Gamgine.Lens.State where
#include "Gamgine/Utils.cpp"
IMPORT_LENS_AS_LE

import Control.Applicative ((<$>))
import qualified Control.Monad.State as ST

-- | apply the getter lens on the value of the state 
getL :: Monad m => LE.Lens a b -> ST.StateT a m b
getL lens = ST.gets $ LE.getL lens
  

-- | apply the setter of the lens on the value of the state
setL :: Monad m => LE.Lens a b -> b -> ST.StateT a m ()
setL lens value = ST.modify $ LE.setL lens value

-- | modify the value of the state with a lens
modL :: Monad m => LE.Lens a b -> (b -> b) -> ST.StateT a m ()
modL lens f =  ST.modify $ LE.modL lens f
