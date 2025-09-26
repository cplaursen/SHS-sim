{-# LANGUAGE MonoLocalBinds #-}

module Execution where

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.List (sortBy)
import Data.Map.Strict (Map, fromList, insert, keysSet, mapWithKey, (!), (!?))
import Data.Maybe (fromJust)
import Data.Set (Set, isSubsetOf, toList, (\\))
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V
import Expression
import Type.Reflection

import Control.Monad.Writer.Lazy (runWriterT)

import Control.Monad.Primitive (PrimState)
import Control.Monad.RWS (ask, asks, get, guard, lift, put, tell, when)

import Lens.Micro.Platform (at, use, (.=), (?=))

import AST_Operations
import Lexer (runAlex)
import Parser (parseExpr)
import Types

import Control.Monad.ST
import System.IO (hPutStrLn, stderr)
import System.Random.MWC qualified as MWC
import System.Random.MWC.Distributions (standard)

import Euler_Maruyama (eulerMaruyamaTraceDiag)

import Data.HashMap.Internal.Strict qualified as M
import Tracing
import Typecheck (testEqualityEither)

inputUser :: String -> SHPType a -> Config IO w -> IO (Expr a)
inputUser v typ config = do
  hPutStrLn stderr $ "Input expression for variable " ++ v
  response <- getLine
  let expression = do
        expr ::: eTyp <- readAExpr (types config) (enums config) response
        Refl <- testEqualityEither ("Incorrect type for variable " ++ v) typ eTyp
        return expr
  case expression of
    Left err -> inputUser v typ config
    Right expr -> return expr

inputStream :: String -> SHPType a -> Config (ST s) w -> ST s (Expr a)
inputStream = undefined

runSHP ::
  (forall a. String -> SHPType a -> Config m w -> m (Expr a)) ->
  SHP ->
  Execution m w ()
runSHP inputFun shp =
  case shp of
    SDE drift noise boundary -> do
      state <- use store
      let flowVars = map getDiffVar drift
      let flowVarMap = fromList $ zip flowVars [0 ..]

      let
        eval :: Expr a -> Vector Double -> a
        eval = evalExprVector state flowVarMap
        flowF v t = V.fromList $ map (flip eval v . getDiffExpr) drift
        noiseF v t = V.fromList $ map (flip eval v . getDiffExpr) noise
        evalBoundary t = evalExprVector state flowVarMap boundary
      new_state <- eulerMaruyamaTraceDiag flowF noiseF evalBoundary (storeToVec flowVars state) flowVarMap
      store .= vecToStore flowVars state new_state
    Assn (Var v typ) exp -> do
      disc <- use store
      store . at v ?= toDyn (evalExprStore disc exp)
    RandAssn (Var v typ) lo hi -> do
      g <- asks gen
      disc <- use store
      val <- MWC.uniformR (evalExprStore disc hi, evalExprStore disc lo) g
      store . at v ?= toDyn val
    Input (Var v typ) -> do
      config <- ask
      enums <- asks enums
      disc <- use store
      expression <- lift $ inputFun v typ config
      store . at v ?= toDyn (evalExprStore disc expression)
    Composition n m -> runSHP inputFun n >> runSHP inputFun m
    While p m -> do
      disc <- use store
      when (evalExprStore disc p) $ runSHP inputFun m >> runSHP inputFun shp
    Loop prog -> do
      maxT <- asks maxTime
      t <- use time
      when (t < maxT) $ runSHP inputFun prog >> runSHP inputFun shp
    Skip -> return ()
    Cond p n m -> do
      disc <- use store
      if evalExprStore disc p
        then runSHP inputFun n
        else runSHP inputFun m
