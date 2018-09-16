module Compiler.Identifier
  (getNewID, counterID)
  where

import           Data.IORef
import           System.IO.Unsafe


counterID :: IORef Int
{-# NOINLINE counterID #-}
counterID = unsafePerformIO $ newIORef (0 :: Int)

-- | Generates a new ID
getNewID :: IO Int
getNewID = do
  val <- readIORef counterID
  writeIORef counterID (val + 1)
  return val
