{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Typecheck where

import Control.Monad
import Data.Coerce (coerce)
import Data.Map (Map, (!), (!?))
import Data.String.Interpolate
import Data.Type.Equality
import ParsingTypes
import Type.Reflection
import Types

instance MonadFail (Either String) where
  fail = Left

testEqualityEither :: (TestEquality f) => String -> f a -> f b -> Either String (a :~: b)
testEqualityEither err a b = case testEquality a b of
  Just Refl -> Right Refl
  Nothing -> Left err

typeUOp :: String -> SHPType a -> Either String AExpr
typeUOp op typ
  | op == "-" = case typ of
      SHPReal -> Right (EUop Neg ::: (SHPReal :-> SHPReal))
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
      _ -> Left "Wrong types"
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

maybeToEither :: Maybe a -> b -> Either b a
maybeToEither Nothing x = Left x
maybeToEither (Just a) _ = Right a

typecheckPExpr :: Env -> PExpr -> Either String AExpr
typecheckPExpr env e =
  case e of
    PReal r -> Right $ EReal r ::: SHPReal
    PBool b -> Right $ EBool b ::: SHPBool
    PEnum e -> Right $ EEnum e ::: SHPEnum
    PVar v -> do
      ASHPType typ <-
        maybeToEither
          (env !? v)
          [i|Error - undefined variable in expression: #{v}|]
      return $ EVar (Var v typ) ::: typ
    PBop op f g -> do
      f_typed ::: f_t <- typecheckPExpr env f
      g_typed ::: g_t <- typecheckPExpr env g
      op_typed ::: (t1 :-> t2 :-> ret_typ) <- typeBOp op f_t g_t
      -- Refl statements prove type equality to the type checker
      Refl <- testEqualityEither [i|Error: expression #{f_typed} does not match operand type #{t1}|] f_t t1
      Refl <- testEqualityEither [i|Error: expression #{g_typed} does not match operand type #{t2}|] g_t t2
      return $ EApp (EApp op_typed f_typed) g_typed ::: ret_typ
    PUop op f -> do
      f_typed ::: f_t <- typecheckPExpr env f
      op_typed ::: (t1 :-> ret_typ) <- typeUOp op f_t
      -- Show that t1 has the same type as f_t
      Refl <- testEqualityEither [i|Type mismatch for operator #{op}: #{f_t} does not match #{t1}|] f_t t1
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
      ASHPType var_typ <- maybeToEither (env !? var) [i|Error: variable #{var} undefined in assignment|]
      exp_typed ::: typ <- typecheckPExpr env exp
      Refl <-
        testEqualityEither
          [i|Type mismatch for assignment. Variable #{var} does not match type #{typ}|]
          var_typ
          typ
      return $ Assn (Var var typ) exp_typed
    PRandAssn var lo hi -> do
      ASHPType SHPReal <- maybeToEither (env !? var) [i|Undefined variable #{var} in random assignment|]
      lo_typed ::: SHPReal <- typecheckPExpr env lo
      hi_typed ::: SHPReal <- typecheckPExpr env hi
      return $ RandAssn (Var var SHPReal) lo_typed hi_typed
    PInput var -> do
      typ <- maybeToEither (env !? var) [i|Undefined variable #{var} in assignment|]
      Right $ unwrapASHPType (Input . Var var) typ
    PComp left right -> do
      left_shp <- typecheckPSHP env left
      right_shp <- typecheckPSHP env right
      return $ Composition left_shp right_shp
    PWhile exp prog -> do
      exp_typed ::: SHPBool <- typecheckPExpr env exp
      prog_typed <- typecheckPSHP env prog
      return $ While exp_typed prog_typed
    PLoop prog -> do
      prog_typed <- typecheckPSHP env prog
      return $ Loop prog_typed
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
