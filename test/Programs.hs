{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Programs where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State.Strict
import qualified Data.ByteString               as BS
import           Data.Default
import qualified Data.Vector                   as V
import           Development.IncludeFile
import           Test.Hspec

import           Compiler.Ast
import           Compiler.Error
import           Compiler.Instruction
import           Compiler.Interpreter.Evaluate
import           Compiler.Prelude
import           Compiler.Scope
import           Compiler.Token.Types
import           Compiler.Types
import           Compiler.World

import           Parser

compileDirectory :: FilePath -> [Bool]
compileDirectory = undefined

$(includeFileInSource "./test/programs/01-basics.sflow" "basics01")
$(includeFileInSource "./test/programs/02-structures.sflow" "structures02")
$(includeFileInSource "./test/programs/03-classes.sflow" "classes03")


getResult :: MonadIO m => BS.ByteString -> m (Either (ErrorInfo WorldError) (Maybe Object))
getResult prog =
  liftIO $ runExceptT (evalStateT (compileFile prog) (def :: World Object))

compileFile :: BS.ByteString -> StWorld (Maybe Object)
compileFile rawText = do
  let ast = getAST rawText
  case ast of
    Right (Code stmts) -> do
      mapM_ (uncurry newVarWithName) baseBasicFunctions -- Load Prelude
      astScoped <- liftScope $ computeStatements stmts
      instrs <- return . runIdentity $ transform (ExprSeq astScoped)
      passing <- runProgram (instrs :: ProgInstr Passing)
      case passing of
        ByRef addr -> do
          obj <- unwrap <$> getVar addr
          return (Just obj)
        ByVal val -> return $ Just val

    _ -> return Nothing




programTest :: SpecWith ()
programTest =
  describe "Full pass programs" $ do
    it "Basics" $ do
      prog <- getResult basics01
      prog `shouldBe` Right (Just ONone)

    it "Structures" $ do
      prog <- getResult structures02

      prog `shouldBe` Right (Just (ONum 7))

    it "Classes" $ do
      prog <- getResult classes03

      prog `shouldBe` Right (Just ONone)
