{-# LANGUAGE ExistentialQuantification, MonoLocalBinds #-}
module Typecheck where

import Types
import ParsingTypes
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

typeUOp :: String -> SHPType a -> Either String AExpr
typeUOp op typ
  | op == "-" = case typ of
                  SHPReal -> Right (EUop Neg ::: (SHPReal :-> SHPReal))
                  SHPInt  -> Right (EUop Neg ::: (SHPInt :-> SHPInt))
                  _ -> Left "Couldn't match type of (-) with argument"
  | op == "~" = case typ of
                  SHPBool -> Right (EUop Not ::: (SHPBool :-> SHPBool))
                  _ -> Left "Coudln't match type of (~) with argument"
  | op == "sin" = case typ of
                  SHPReal -> Right (EUop Sin ::: (SHPReal :-> SHPReal))
                  _ -> Left "Couldn't match type of sin with argument"
  | op == "cos" = case typ of
                  SHPReal -> Right (EUop Cos ::: (SHPReal :-> SHPReal))
                  _ -> Left "Couldn't match type of cos with argument"

typeBOp :: String -> SHPType a -> SHPType b -> Either String AExpr
typeBOp op arg1 arg2 = 
    case op of
        "+" -> case (arg1, arg2) of
                 (SHPReal, SHPReal) -> Right $ EBop Plus ::: (SHPReal :-> SHPReal :-> SHPReal)
                 _ -> Left "Wrong type"
        "-" -> case (arg1, arg2) of
                 (SHPReal, SHPReal) -> Right $ EBop Minus ::: (SHPReal :-> SHPReal :-> SHPReal)
                 _ -> Left "Wrong type"
        "*" -> case (arg1, arg2) of
                 (SHPReal, SHPReal) -> Right $ EBop Times ::: (SHPReal :-> SHPReal :-> SHPReal)
                 _ -> Left "Wrong type"
        "/" -> case (arg1, arg2) of
                 (SHPReal, SHPReal) -> Right $ EBop Divide ::: (SHPReal :-> SHPReal :-> SHPReal)
                 _ -> Left "Wrong type"
        -- Ideally this would be done with testEquality but then it can't resolve the typeclass Eq
        "=" -> case (arg1, arg2) of
                  (SHPReal, SHPReal) -> Right $ EBop Eq ::: (SHPReal :-> SHPReal :-> SHPBool)
                  (SHPBool, SHPBool) -> Right $ EBop Eq ::: (SHPBool :-> SHPBool :-> SHPBool)
                  (SHPEnum, SHPEnum) -> Right $ EBop Eq ::: (SHPEnum :-> SHPEnum :-> SHPBool)
                  _ -> Left  "Wrong types"
        "<=" -> case (arg1, arg2) of
                 (SHPReal, SHPReal) -> Right (EBop Leq ::: (SHPReal :-> SHPReal :-> SHPBool))
                 _ -> Left "Wrong type"
        "<" -> case (arg1, arg2) of
                 (SHPReal, SHPReal) -> Right (EBop Le ::: (SHPReal :-> SHPReal :-> SHPBool))
                 _ -> Left "Wrong type"
        ">=" -> case (arg1, arg2) of
                 (SHPReal, SHPReal) -> Right (EBop Geq ::: (SHPReal :-> SHPReal :-> SHPBool))
                 _ -> Left "Wrong type"
        ">" -> case (arg1, arg2) of
                 (SHPReal, SHPReal) -> Right (EBop Ge ::: (SHPReal :-> SHPReal :-> SHPBool))
                 _ -> Left "Wrong type"
        "&&" -> case (arg1, arg2) of
                 (SHPBool, SHPBool) -> Right (EBop And ::: (SHPBool :-> SHPBool :-> SHPBool))
        "||" -> case (arg1, arg2) of
                 (SHPBool, SHPBool) -> Right (EBop Or ::: (SHPBool :-> SHPBool :-> SHPBool))

typecheckPExpr :: Env -> PExpr -> Either String AExpr
typecheckPExpr env e =
    case e of
      PReal r -> Right $ EReal r ::: SHPReal
      PBool b -> Right $ EBool b ::: SHPBool
      PEnum e -> Right $ EEnum e ::: SHPEnum
      PVar v -> 
          let t = env!v
           in Right $ unwrapASHPType (\typ -> EVar (Var v typ) ::: typ) t
      PBop op f g -> do
          f_typed ::: f_t <- typecheckPExpr env f
          g_typed ::: g_t <- typecheckPExpr env g
          op_typed ::: (t1 :-> t2 :-> ret_typ) <- typeBOp op f_t g_t
          -- Refl statements prove type equality to the type checker
          Refl <- testEqualityEither "" f_t t1
          Refl <- testEqualityEither "" g_t t2
          return $ EApp (EApp op_typed f_typed) g_typed ::: ret_typ
      PUop op f -> do
          f_typed ::: f_t <- typecheckPExpr env f
          op_typed ::: (t1 :-> ret_typ) <- typeUOp op f_t
          -- Show that t1 has the same type as f_t
          Refl <- testEqualityEither "" f_t t1
          return $ EApp op_typed f_typed ::: ret_typ

typecheckPDiff :: Env -> PDiff -> Either String Diff
typecheckPDiff env (PDiff var expr) = do
    expr_typed ::: SHPReal <- typecheckPExpr env expr
    return $ Diff (Var var SHPReal) expr_typed

typecheckDiffs :: Env -> [PDiff] -> Either String [Diff]
typecheckDiffs env = mapM (typecheckPDiff env)

typecheckPSHP :: Env -> PSHP -> Either String SHP
typecheckPSHP env shp =
    case shp of
        PAssn var exp -> do
            let var_typ = env ! var
            exp_typed ::: typ <- typecheckPExpr env exp
            guard (ASHPType typ == var_typ)
            return $ Assn (Var var typ) exp_typed
        PRandAssn var lo hi -> do
            guard (case env!var of
                     ASHPType SHPReal -> True
                     _ -> False)
            lo_typed ::: SHPReal <- typecheckPExpr env lo
            hi_typed ::: SHPReal <- typecheckPExpr env hi
            return $ RandAssn (Var var SHPReal) lo_typed hi_typed
        PInput var -> Right $ unwrapASHPType (Input . Var var) (env!var)
        PChoice prob left right -> do
            prob_typed ::: SHPReal <- typecheckPExpr env prob
            left_shp <- typecheckPSHP env left
            right_shp <- typecheckPSHP env right
            return $ Choice prob_typed left_shp right_shp
        PComp left right -> do
            left_shp <- typecheckPSHP env left
            right_shp <- typecheckPSHP env right
            return $ Composition left_shp right_shp
        PWhile exp prog -> do
            exp_typed ::: SHPBool <- typecheckPExpr env exp
            prog_typed <- typecheckPSHP env prog
            return $ While exp_typed prog_typed
        PLoop prog -> Loop <$> typecheckPSHP env prog
        PAbort -> Right Abort
        PSkip -> Right Skip
        PCond exp left right -> do
            exp_typed ::: SHPBool <- typecheckPExpr env exp
            left_shp <- typecheckPSHP env left
            right_shp <- typecheckPSHP env right
            return $ Cond exp_typed left_shp right_shp
        PSDE flow drift boundary -> do
            flow_typed <- typecheckDiffs env flow
            drift_typed <- typecheckDiffs env drift
            boundary_typed ::: SHPBool <- typecheckPExpr env boundary
            return $ SDE flow_typed drift_typed boundary_typed
