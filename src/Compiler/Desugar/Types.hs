{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Compiler.Desugar.Types where


-- | Apply a transformation into AST, it can varies internal info type of ast.
-- This transformation be able to carry out in monad typed
class Monad m => Desugar ast a m ast' b | a -> b, ast -> ast' where
  transform :: ast a -> m (ast' b)
