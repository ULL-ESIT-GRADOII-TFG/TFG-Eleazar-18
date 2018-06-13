module Compiler.Identifier
  (getNewID)
  where

import           Data.IORef
import           System.IO.Unsafe


counterID :: IORef Word
{-# NOINLINE counterID #-}
counterID = unsafePerformIO $ newIORef (0 :: Word)

-- | Generates a new ID
getNewID :: Word
{-# NOINLINE getNewID #-}
getNewID = unsafePerformIO $ do
  val <- readIORef counterID
  writeIORef counterID (val + 1)
  return val
