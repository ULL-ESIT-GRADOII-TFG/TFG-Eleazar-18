module ScriptLang.Program
  ( interpretProgram
  ) where

import           Control.Monad
import           Control.Monad.State
import           Data.Functor
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Vector             as V
import           Lens.Simple

import           ScriptLang.Env
import           ScriptLang.Language.AST
import           ScriptLang.Primitives


-- |
interpretProgram :: Program a -> StWorld ()
interpretProgram ast = do
  -- Retrieve type definitions and functions
  mainStmts <- filterM (\st -> functionDef st >> typeDef st) ast
  mapM_ interpretStmt mainStmts


functionDef :: Statement a -> StWorld Bool
functionDef (SFunction _ nameId nameArgs bodyFun) = do
  env <- get
  let fun = PFun env (\primitives -> intoScope $ do
              addVar nameId fun -- Se establece la capacidad de recursividad
                      -- Esta recursividad es incontrolada e ineficiente

              mapM_ (uncurry addVar) (zip nameArgs primitives)
              last <$> mapM interpretStmt bodyFun)
                   -- El último estamento es el valor que se retorna
  addVar nameId fun
  return True
functionDef _ = return False


typeDef :: Statement a -> StWorld Bool
typeDef (SType _ _definition) = do
  -- TODO: Construir los metodos y atributos
  return True
typeDef _ = return False


forLoop :: Exp -> Maybe NameId -> Program a -> StWorld ()
forLoop expr mName prog = evalExpr expr >>= (intoScope . loop)
  where
    loop prim =
      mapPrimitive prim (\value ->
          case mName of
            Just name -> do
              addVar name value
              interpretProgram prog
              return PNone
            Nothing   -> do
              implicit .= value
              interpretProgram prog
              return PNone
        )

whileLoop :: Exp -> Program a -> StWorld Primitive
whileLoop expr prog = intoScope (go >> return PNone)
  where
    go = do
      prim <- evalExpr expr
      when (checkBoolean prim) (mapM_ interpretStmt prog >> go)

--
interpretStmt :: Statement a -> StWorld Primitive
interpretStmt stmt = case stmt of
  SMkScope _ expr program -> intoScope $ do
    prim <- evalExpr expr
    implicit .= prim
    interpretProgram program
    use implicit
  SFor _ expr mNamed prog -> intoScope $
    forLoop expr mNamed prog >> return PNone
  SWhile _ expr prog -> intoScope $
    whileLoop expr prog
  SIf _ expr prog -> do
    prim <- evalExpr expr
    when (checkBoolean prim) (intoScope $ interpretProgram prog)
    return PNone
  SIfElse _ expr trueProg falseProg -> do
    prim <- evalExpr expr
    if checkBoolean prim then
      intoScope $ interpretProgram trueProg
    else
      intoScope $ interpretProgram falseProg
    return PNone
  SExpr _ expr -> evalExpr expr
  _ -> error "intepretStmt, can't interpret Function and Type definitions"


evalExpr :: Exp -> StWorld Primitive
evalExpr expr = case expr of
  EApply path exprs -> mapM evalExpr exprs >>= search path
  EString text      -> return $ PText text
  EInt int          -> return $ PInt int
  EDouble double    -> return $ PDouble double
  EBool bool        -> return $ PBool bool
  ENone             -> return PNone
  ERegex regex      -> return undefined -- TODO:
  EShell shell      -> return undefined
  EVector exprs     -> PList . V.fromList <$> mapM evalExpr exprs
  EDict dict        -> PDict . M.fromList <$> mapM (\(key, value) -> do
                                                      key' <- evalExpr key
                                                      value' <- evalExpr value)) dict



        -- CallCommand name objs next -> do
        --   baseFuncs <- use baseFunctions
        --   eObj <- baseFuncs name objs
        --   case eObj of
        --     Left (ObjectError _err) -> do
        --       self <- use currentObject
        --       _ <- searchIntoObject objs self name
        --       return ()
        --     Right obj                -> do
        --
