module Memory where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Default
import qualified Data.IntMap                as IM
import           Data.IORef
import           Test.Hspec


import           Compiler.Error
import           Compiler.Identifier
import           Compiler.Instruction
import           Compiler.Types

getMemory :: MonadIO m => ProgInstr Address -> m (Either (ErrorInfo WorldError) (World Object))
getMemory prog =
  liftIO $ runExceptT (execStateT (runProgram prog) (def :: World Object))

--
info :: Info
info = tokToInfo def

memoryTest :: SpecWith ()
memoryTest =
  describe "Memory management" $ do
    -- Reset Counter
    before_ (writeIORef counterID 0) $ do

      it "Simple CreateVar, DropVar" $ do
        memory <- getMemory $ do
          addr <- createVar info ONone
          dropVar info (simple 0)
          noop

        memory `shouldBe` (Right (def :: World Object))

      it "Simple CreateVar, CollectVar and LocalGC" $ do
        memory <- getMemory $ do
          addr <- createVar info ONone
          _ <- collectAddress addr
          applyLocalGC
          noop

        memory `shouldBe` (Right (def :: World Object))

      it "References RC counter - Sum" $ do
        memory <- getMemory $ do
          addr <- createVar info ONone
          addr1 <- getRef info (simple addr)
          noop

        memory `shouldBe`
          (Right ((def :: World Object)
            { _table = IM.fromList
              [ (0, Rc 2 ONone)
              , (1, Rc 1 (ORef 0))
              ]
            }
          ))

      it "References RC counter - Minus" $ do
        memory <- getMemory $ do
          addr <- createVar info ONone
          addr1 <- getRef info (simple addr)
          _ <- collectAddress addr1
          applyLocalGC
          noop

        memory `shouldBe`
          (Right ((def :: World Object)
            { _table = IM.fromList
              [ (0, Rc 1 ONone)
              ]
            }
          ))


      it "References RC counter - Minus 2" $ do
        memory <- getMemory $ do
          addr <- createVar info ONone
          _ <- collectAddress addr
          addr1 <- getRef info (simple addr)
          _ <- collectAddress addr1
          applyLocalGC
          noop

        memory `shouldBe` Right (def :: World Object)

      it "References RC" $ do
        test <- liftIO $ Address <$> getNewID

        memory <- getMemory $ do
          addr <- createVar info ONone
          addr1 <- assign info (simple test) addr
          addr2 <- getRef info (simple addr1)
          noop

        memory `shouldBe`
          (Right ((def :: World Object)
            { _table = IM.fromList
              [ (0, Rc 2 ONone)
              , (1, Rc 1 ONone)
              , (2, Rc 1 $ ORef 0)
              ]
            }
          ))

      it "Cyclic References" $ do
        memory <- getMemory $ do
          addr <- createVar info ONone
          addr1 <- getRef info (simple addr)
          assign info (simple addr) addr1 -- It should keep at the same raw value
          noop

        memory `shouldBe`
          (Right ((def :: World Object)
            { _table = IM.fromList
              [ (0, Rc 2 ONone)
              , (1, Rc 1 $ ORef 0)
              ]
            }
          ))

      it "Cyclic References" $ do
        test <- liftIO $ Address <$> getNewID
        test2 <- liftIO $ Address <$> getNewID
        --test3 <- liftIO $ Address <$> getNewID

        memory <- getMemory $ do
          addr <- createVar info ONone -- #2
          testAdr <- assign info (simple test) addr -- #0
          addr1 <- getRef info (simple testAdr) -- #3
          assign info (simple test2) addr1  -- #1
          assign info (simple test) test2 -- Cyclic
          noop

        memory `shouldBe`
          (Right ((def :: World Object)
            { _table = IM.fromList
              [ (0, Rc 2 ONone)
              , (2, Rc 1 ONone)
              , (3, Rc 1 $ ORef 0)
              , (1, Rc 1 $ ORef 0)
              ]
            }
          ))
