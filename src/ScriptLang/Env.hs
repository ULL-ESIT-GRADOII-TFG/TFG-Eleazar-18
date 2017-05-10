module ScriptLang.Env where

import           Control.Monad.State
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Text           (Text)
import           Lens.Simple


-- | Defines the program scope into levels, it should be generate with functions, loop, conditional...
data Env a = Env
  { _implicit        :: !(Maybe a)
  , _scope           :: !(Map Text a)
  , _upperScope      :: !(Maybe (Env a))
  , _typeDefinitions :: ![a]
  -- ^ Object definitions avialable
  }

initialEnv :: Env a
initialEnv = Env
  { _implicit = Nothing
  , _scope = M.empty
  , _upperScope = Nothing
  , _typeDefinitions = []
  }
