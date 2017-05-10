module ScriptLang.EDSL where

import           Control.Monad.State.Strict

import           ScriptLang.Env
import           ScriptLang.Primitives

-- | Enviroment where code is evaluate
type EDSL = StateT (Env Primitives) IO

intoScope :: EDSL a -> EDSL a
intoScope prog = do
  env <- get
  put (Env PNone M.empty (Just env) [])
  val <- prog
  mEnv <- use upperScope
  case mEnv of
    Just e  -> put e >> return val
    Nothing -> error "Something unexpected undoing a scope"

addVar :: Text -> Primitive -> EDSL ()
addVar name val = scope %= M.insert name val

-- |
mapPrimitive :: Primitive -> (Primitive -> StWorld Primitive) -> StWorld ()
mapPrimitive prim fun = case prim of
  PList list    -> mapM_ fun list
  PDict dic     -> mapM_ fun dic
  PObject _meta -> undefined
  PClass _meta  -> undefined
  _             -> void $ fun prim


search :: [T.Text] -> [Primitive] -> StWorld Primitive
search = undefined
