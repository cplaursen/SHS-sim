module AST_Operations where

import SHPTypes
import Types
import Data.Generics hiding ( typeRep, empty )
import Data.Set ( Set, insert, empty, singleton, union )
import Lens.Micro.Platform

-----------------------------
-- Retrieve used variables --
-----------------------------
getPExprVar :: PExpr -> Set String
getPExprVar (PVar s) = singleton s
getPExprVar _ = empty

getAssnVars :: PSHP -> Set String
getAssnVars (PAssn x _) = singleton x
getAssnVars (PRandAssn x _ _) = singleton x
getAssnVars _ = empty

getPDiffVars :: PDiff -> Set String
getPDiffVars (PDiff x _) = singleton x

getVarQ :: GenericQ (Set String)
getVarQ = mkQ empty getPExprVar `extQ` getAssnVars `extQ` getPDiffVars

allVars :: Data a => a -> Set String
allVars = everything union getVarQ

allPExprVars :: Data a => a -> Set String
allPExprVars = everything union (mkQ empty getPExprVar)

--------------------------
-- Substitute constants --
--------------------------
substDef :: Data a => Def -> a -> a
substDef (Def v e1) = everywhere (mkT helper)
    where
        helper (PVar w)
          | v == w = e1
          | otherwise = PVar w
        helper x = x

replaceEnum :: Data a => String -> a -> a
replaceEnum e = everywhere (mkT helper)
    where
        helper (PVar v)
          | e == v = PEnum e
          | otherwise = PVar v
        helper x = x

evalBlocks :: Blocks -> PSHP
evalBlocks block = replaceDefs (block^. constBlock) $ replaceEnums (block ^. enumBlock) (block ^. shpBlock)
    where replaceEnums strings value = foldr replaceEnum value strings
          replaceDefs defs value = foldr substDef value defs
