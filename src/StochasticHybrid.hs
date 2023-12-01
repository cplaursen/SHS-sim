{-# LANGUAGE MonoLocalBinds #-}
module StochasticHybrid where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map ( Map, (!), (!?), fromList, insert )
import Data.List (sortBy)
import Data.Dynamic
import Data.Maybe ( fromJust )
import Data.Set ( Set )
import Type.Reflection

import Control.Monad.Writer.Lazy (runWriterT)

import Control.Monad.Primitive (PrimState)
import Control.Monad.RWS (RWST, ask, get, asks, tell, put, lift, when)

import Lens.Micro.Platform

import Parser
import Lexer
import Types
import AST_Operations

import qualified System.Random.MWC as MWC
import System.Random.MWC.Distributions (standard)

import Euler_Maruyama

import Typecheck

evalExpr :: Store -> Expr a -> a
evalExpr ctx expr =
    case expr of
      Real r -> r
      EVar (Var x typ) -> case fromDynamic (ctx!x) of
                        Just v -> v
                        Nothing -> error $ "Compiler type error: expected variable of type " ++ show typ ++ ", got " ++ show (ctx!x)
      EBool b -> b
      Bop op l r -> evalBOperator op (evalExpr ctx l) (evalExpr ctx r)
      Uop op x -> evalUOperator op (evalExpr ctx x)
      Enum x -> x

-- Evaluates an expression, evaluating variables to their vector index if they appear in the map
evalExprVector :: Store -> Map String Int -> Expr a -> Vector Double -> a
evalExprVector ctx variables expr vector =
    case expr of
      Real r -> r
      EVar (Var x typ) ->
          case (typ, variables !? x) of
            (SHPReal, Just n) -> vector V.! n
            _ -> case fromDynamic (ctx!x) of
                   Just v -> v
                   Nothing -> error "Compiler type error"
      EBool b -> b
      Bop op l r -> evalBOperator op (evalExprVector ctx variables l vector)
                                     (evalExprVector ctx variables r vector)
      Uop op x -> evalUOperator op (evalExprVector ctx variables x vector)
      Enum x -> x

diag :: a -> Vector a -> Vector (Vector a)
diag fill vec = fmap (V.fromList . helper 0) (V.enumFromN 0 (length vec))
    where helper n k
            | n >= length vec = []
            | k == n = vec V.! n : helper (n+1) k
            | otherwise = fill : helper (n+1) k

-- Assumes all variables in vars are Double-valued in the store
storeToVec :: [String] -> Store -> Vector Double
storeToVec vars store = V.fromList $ map (fromJust . fromDynamic . (store!)) vars

-- Assumes vars and vector are aligned
vecToStore :: [String] -> Store -> Vector Double -> Store
vecToStore vars store vector = foldr (uncurry insert) store keyValues
    where keyValues = zip vars (map toDyn (V.toList vector))

getDiffVar :: Diff -> String
getDiffVar (Diff (Var s _) _) = s

getDiffExpr :: Diff -> Expr Double
getDiffExpr (Diff _ exp) = exp

readAExpr :: Env -> Set String -> String -> Either String AExpr
readAExpr env enums string = do
    parsed <- runAlex string parseExpr 
    let withEnums = replaceEnum enums parsed
    typeCheckPExpr env withEnums

runSHP :: SHP -> Execution IO ()
runSHP shp =
    case shp of
      SDE drift noise boundary -> do
          state <- use store
          let flowVars = map getDiffVar drift
          let flowVarMap = fromList $ zip flowVars [0..]
          let stateVec = storeToVec flowVars state
          let eval = evalExprVector state flowVarMap
          let flowF v t = V.fromList $ map (flip eval v . getDiffExpr) drift
          let noiseF v t = diag 0 $ V.fromList $ map (flip eval v . getDiffExpr) noise
          let evalBoundary t = evalExprVector state flowVarMap boundary
          new_state <- eulerMaruyamaTrace flowF noiseF evalBoundary stateVec
          let new_store = vecToStore flowVars state new_state
          store .= new_store
      Assn (Var v typ) exp -> do
          disc <- use store
          store . at v ?= toDyn (evalExpr disc exp)
      RandAssn (Var v typ) lo hi -> do
          g <- asks gen
          disc <- use store
          val <- MWC.uniformR (evalExpr disc hi, evalExpr disc lo) g
          store . at v ?= toDyn val
      Input (Var v typ) -> do
          env <- asks types
          enums <- asks enums
          disc <- use store
          lift $ putStrLn $ "Input expression for variable " ++ v
          response <- lift getLine
          let expression = do
                    expr ::: eTyp <- readAExpr env enums response
                    Refl <- testEqualityEither ("Incorrect type for variable " ++ v) typ eTyp
                    return (toDyn (evalExpr disc expr))
          case expression of
            Left err -> lift (print err) >> runSHP shp
            Right expr -> store . at v ?= expr
      Choice prob n m -> do
          g <- asks gen
          disc <- use store
          val <- MWC.uniformR (0,1) g
          if val < evalExpr disc prob
             then runSHP n
             else runSHP m
      Composition n m -> runSHP n >> runSHP m
      While p m -> do
          disc <- use store
          when (evalExpr disc p) $ runSHP m >> runSHP shp
      Abort -> error "abort"
      Skip -> return ()
      Cond p n m -> do
          disc <- use store
          if evalExpr disc p
             then runSHP n
             else runSHP m
