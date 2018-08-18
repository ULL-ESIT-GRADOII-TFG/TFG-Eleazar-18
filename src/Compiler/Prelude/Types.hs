{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Compiler.Prelude.Types where

-- import           Control.Monad.Except
-- import           Control.Monad.Trans.Free

-- import           Compiler.Error
-- import           Compiler.Object
-- import           Compiler.Types



-- -- | Tranform haskell curryfied functions into single list argument function
-- class MemoryManagement mm => Normalize a mm where
--   {-# MINIMAL normalize' #-}
--   normalize :: a -> [RawObj mm] -> Prog mm mm (RawObj mm)
--   normalize a objs = normalize' a 0 (length objs) objs
--   -- | Same to normalize but take account of numbers of args expected and given
--   normalize' :: a -> Int -> Int -> [RawObj mm] -> Prog mm mm (RawObj mm)

-- instance (MemoryManagement mm, Prog mm mm (RawObj mm) ~ a, GetInfo (Prog mm mm), MonadError (ErrorInfo WorldError) (Prog mm mm)) => Normalize a mm  where
--   normalize' f expected given ls =
--     case ls of
--       [] -> f
--       _  -> throw $ NumArgsMissmatch expected given

-- instance {-# OVERLAPPING #-} (MemoryManagement mm, RawObj mm ~ Object mm, GetInfo (Prog mm mm), MonadError (ErrorInfo WorldError) (Prog mm mm)) => Normalize (Object mm) mm where
--   normalize' f expected given ls =
--     case ls of
--       [] -> return f
--       _  -> throw $ NumArgsMissmatch expected given

-- instance {-# OVERLAPPING #-} (MemoryManagement mm, ToObject a mm, RawObj mm ~ Object mm, GetInfo (Prog mm mm), MonadError (ErrorInfo WorldError) (Prog mm mm), MonadIO (Prog mm mm), MonadTrans (Prog mm)) => Normalize (IO a) mm where
--   normalize' f expected given ls =
--     case ls of
--       [] -> do
--         val <- liftIO f
--         lift $ toObject val
--       _  -> throw $ NumArgsMissmatch expected given

-- instance {-# OVERLAPPING #-} (ToObject a mm, MemoryManagement mm, ToObject a mm, RawObj mm ~ Object mm, GetInfo (Prog mm mm), MonadError (ErrorInfo WorldError) (Prog mm mm), MonadTrans (Prog mm)) => Normalize (mm a) mm where
--   normalize' f expected given ls =
--     case ls of
--       [] -> lift $ f >>= toObject
--       _  -> throw $ NumArgsMissmatch expected given

-- instance {-# OVERLAPPING #-} (Normalize r mm, FromObject a mm, MemoryManagement mm, RawObj mm ~ Object mm, GetInfo (Prog mm mm), MonadError (ErrorInfo WorldError) (Prog mm mm), MonadTrans (Prog mm)) => Normalize (a -> r) mm where
--   normalize' fun _ given []     =
--       throw $ NumArgsMissmatch (count fun 0) given
--   normalize' fun expected given (a:xs) = do
--     obj <- lift $ fromObject a
--     normalize' (fun obj) (expected + 1) given xs

-- -- | Used to count params avoiding evaluate function
-- class CountParams a where
--   count :: a -> Int -> Int

-- instance CountParams a where
--   count _ n = n

-- instance {-# OVERLAPPING #-} (CountParams r) => CountParams (a -> b -> r) where
--   count fun n = count (fun undefined) (n + 1)

-- instance {-# OVERLAPPING #-} (CountParams r) => CountParams (a -> r) where
--   count _ n = n + 1
