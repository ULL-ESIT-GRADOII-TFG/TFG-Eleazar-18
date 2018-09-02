module Compiler.Ast.Th where

-- import Template.Haskell


-- mkAST :: Dec -> QDecs
-- mkASt dec = case dec of
--   DataD _ nameData varBndr _ constrs _ ->


-- extendConstrs :: TyVarBndr -> Con -> (Con, Text)
-- extendConstrs varBndr con = case con of
--   NormalC nameCon argsTypes ->
--     NormalC nameCon ((Bang NoSourceUnpackedness NoSourceStrictness, [t| $xNameCon $varBndr |]):argsTypes)

-- mkFamilies :: TyVarBndr -> [Name] -> Decs
-- mkFamilies varBndr names =
--   map (\name -> [d| type families $name $tyBndr |]) names

-- mkPattern :: Name -> Name -> Name -> [BangTypes] -> Q Decs
-- mkPattern nameData nameConstr nameExt args =
--   let namePattern
--   let patPattern
--   [d|
--     pattern $namePattern :: $(f args) -> $nameData
--     pattern $patPattern <- $nameConstr _ acc _
--       where $patPattern = $nameConstr void
--     |]
