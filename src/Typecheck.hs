{-# LANGUAGE ExistentialQuantification, MonoLocalBinds #-}
module Typecheck where

import Types
import SHPTypes
import Type.Reflection
import Control.Monad
import Data.Map ( Map, (!) )
import Data.Type.Equality

instance MonadFail (Either String)
    where fail = Left

testEqualityEither :: TestEquality f => String -> f a -> f b -> Either String (a :~: b)
testEqualityEither err a b = case testEquality a b of
                               Just Refl -> Right Refl
                               Nothing -> Left err

instance Eq ASHPType where
    ASHPType t1 == ASHPType t2 = case t1 `testEquality` t2 of
                                   Just Refl -> True
                                   Nothing -> False
-- Hide away the type 
data AExpr = forall a. Typeable a => Expr a ::: SHPType a

-- Our type universe is given by SHPTypes
type Env = Map String ASHPType

data AUOp = forall a b. (Typeable a, Typeable b) =>
    AUOp (UOperator a b) (SHPType a) (SHPType b)

typeUOp :: String -> SHPType a -> Either String AUOp
typeUOp op typ
  | op == "-" = case typ of
                  SHPReal -> Right (AUOp Neg SHPReal SHPReal)
                  _ -> Left "Couldn't match type of (-) with argument"
  | op == "~" = case typ of
                  SHPBool -> Right (AUOp Not SHPBool SHPBool)
                  _ -> Left "Coudln't match type of (~) with argument"

data ABOp = forall a b c.
    (Typeable a, Typeable b, Typeable c) => ABOp (BOperator a b c) (SHPType a) (SHPType b) (SHPType c)

typeBOp :: String -> SHPType a -> SHPType b -> Either String ABOp
typeBOp op arg1 arg2 = 
    case op of
        "+" -> case (arg1, arg2) of
                 (SHPReal, SHPReal) -> Right $ ABOp Plus SHPReal SHPReal SHPReal
                 _ -> Left "Wrong type"
        "-" -> case (arg1, arg2) of
                 (SHPReal, SHPReal) -> Right $ ABOp Minus SHPReal SHPReal SHPReal
                 _ -> Left "Wrong type"
        "*" -> case (arg1, arg2) of
                 (SHPReal, SHPReal) -> Right $ ABOp Times SHPReal SHPReal SHPReal
                 _ -> Left "Wrong type"
        "/" -> case (arg1, arg2) of
                 (SHPReal, SHPReal) -> Right $ ABOp Divide SHPReal SHPReal SHPReal
                 _ -> Left "Wrong type"
        -- Ideally this would be done with testEquality but then it can't resolve the typeclass Eq
        "==" -> case (arg1, arg2) of
                  (SHPReal, SHPReal) -> Right $ ABOp Eq arg1 arg2 SHPBool
                  (SHPBool, SHPBool) -> Right $ ABOp Eq arg1 arg2 SHPBool
                  (SHPEnum, SHPEnum) -> Right $ ABOp Eq arg1 arg2 SHPBool
                  _ -> Left  "Wrong types"
        "<=" -> case (arg1, arg2) of
                 (SHPReal, SHPReal) -> Right (ABOp Leq SHPReal SHPReal SHPBool)
                 _ -> Left "Wrong type"
        "<" -> case (arg1, arg2) of
                 (SHPReal, SHPReal) -> Right (ABOp Le SHPReal SHPReal SHPBool)
                 _ -> Left "Wrong type"
        ">=" -> case (arg1, arg2) of
                 (SHPReal, SHPReal) -> Right (ABOp Geq SHPReal SHPReal SHPBool)
                 _ -> Left "Wrong type"
        ">" -> case (arg1, arg2) of
                 (SHPReal, SHPReal) -> Right (ABOp Ge SHPReal SHPReal SHPBool)
                 _ -> Left "Wrong type"

typeCheckPExpr :: Env -> PExpr -> Either String AExpr
typeCheckPExpr env e =
    case e of
      PReal r -> Right $  Real r ::: SHPReal
      PBool b -> Right $ EBool b ::: SHPBool
      PEnum e -> Right $  Enum e ::: SHPEnum
      PVar v -> 
          let t = env!v
           in Right $ case t of
                        ASHPType SHPReal -> EVar (Var v SHPReal) ::: SHPReal
                        ASHPType SHPBool -> EVar (Var v SHPBool) ::: SHPBool
                        ASHPType SHPEnum -> EVar (Var v SHPEnum) ::: SHPEnum
      PBop op f g -> do
          f_typed ::: f_t <- typeCheckPExpr env f
          g_typed ::: g_t <- typeCheckPExpr env g
          ABOp op t1 t2 ret_typ <- typeBOp op f_t g_t
          -- Refl statements prove type equality to the type checker
          Refl <- testEqualityEither "" f_t t1
          Refl <- testEqualityEither "" g_t t2
          return $ Bop op f_typed g_typed ::: ret_typ
      PUop op f -> do
          f_typed ::: f_t <- typeCheckPExpr env f
          AUOp op t1 ret_typ <- typeUOp op f_t
          -- Show that t1 has the same type as f_t
          Refl <- testEqualityEither "" f_t t1
          return $ Uop op f_typed ::: ret_typ

typeCheckPDiff :: Env -> PDiff -> Either String Diff
typeCheckPDiff env (PDiff var expr) = do
    expr_typed ::: SHPReal <- typeCheckPExpr env expr
    return $ Diff (Var var SHPReal) expr_typed

typeCheckDiffs :: Env -> [PDiff] -> Either String [Diff]
typeCheckDiffs env = mapM (typeCheckPDiff env)

typeCheckPSHP :: Env -> PSHP -> Either String SHP
typeCheckPSHP env shp =
    case shp of
        PAssn var exp -> do
            let var_typ = env ! var
            exp_typed ::: typ <- typeCheckPExpr env exp
            guard (ASHPType typ == var_typ)
            return $ Assn (Var var typ) exp_typed
        -- Not super sure if this will work
        PRandAssn var lo hi -> do
            guard (case env!var of
                     ASHPType SHPReal -> True
                     _ -> False)
            lo_typed ::: SHPReal <- typeCheckPExpr env lo
            hi_typed ::: SHPReal <- typeCheckPExpr env hi
            return $ RandAssn (Var var SHPReal) lo_typed hi_typed
        PChoice prob left right -> do
            prob_typed ::: SHPReal <- typeCheckPExpr env prob
            left_shp <- typeCheckPSHP env left
            right_shp <- typeCheckPSHP env right
            return $ Choice prob_typed left_shp right_shp
        PComp left right -> do
            left_shp <- typeCheckPSHP env left
            right_shp <- typeCheckPSHP env right
            return $ Composition left_shp right_shp
        PWhile exp prog -> do
            exp_typed ::: SHPBool <- typeCheckPExpr env exp
            prog_typed <- typeCheckPSHP env prog
            return $ While exp_typed prog_typed
        PAbort -> Right Abort
        PSkip -> Right Skip
        PCond exp left right -> do
            exp_typed ::: SHPBool <- typeCheckPExpr env exp
            left_shp <- typeCheckPSHP env left
            right_shp <- typeCheckPSHP env right
            return $ Cond exp_typed left_shp right_shp
        PSDE flow drift boundary -> do
            flow_typed <- typeCheckDiffs env flow
            drift_typed <- typeCheckDiffs env drift
            boundary_typed ::: SHPBool <- typeCheckPExpr env boundary
            return $ SDE flow_typed drift_typed boundary_typed
