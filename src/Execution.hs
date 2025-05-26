{-# LANGUAGE MonoLocalBinds #-}
module Execution where
import Data.Vector.Unboxed (Vector)
import Expression
import qualified Data.Vector.Unboxed as V
import Data.Map.Strict ( Map, (!), (!?), fromList, insert, keysSet, mapWithKey )
import Data.List (sortBy)
import Data.Dynamic ( toDyn, fromDynamic, Dynamic )
import Data.Maybe ( fromJust )
import Data.Set ( Set, isSubsetOf, (\\), toList )
import Type.Reflection

import Control.Monad.Writer.Lazy (runWriterT)

import Control.Monad.Primitive (PrimState)
import Control.Monad.RWS ( ask, get, asks, tell, put, lift, when, guard )

import Lens.Micro.Platform ((.=), (?=), use, at)

import Parser ( parseExpr )
import Lexer ( runAlex )
import Types
import AST_Operations

import qualified System.Random.MWC as MWC
import System.Random.MWC.Distributions (standard)
import System.IO ( hPutStrLn, stderr )
import Control.Monad.ST

import Euler_Maruyama ( eulerMaruyamaTraceDiag )

import Typecheck ( testEqualityEither )
import qualified Data.HashMap.Internal.Strict as M
import Tracing

inputUser :: String -> SHPType a -> Config IO -> IO (Expr a)
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

inputStream :: String -> SHPType a -> Config (ST s) -> ST s (Expr a)
inputStream = undefined

runSHP :: TracingMode w 
       -> (forall a. String -> SHPType a -> Config m -> m (Expr a))
       -> SHP
       -> Execution m w ()
runSHP traceMode inputFun shp =
    case shp of
      SDE drift noise boundary -> do
          state <- use store
          let flowVars = map getDiffVar drift
          let flowVarMap = fromList $ zip flowVars [0..]
          let eval = evalExprVector state flowVarMap :: Expr a -> Vector Double -> a
          let flowF v t = V.fromList $ map (flip eval v . getDiffExpr) drift
          let noiseF v t = V.fromList $ map (flip eval v . getDiffExpr) noise
          let evalBoundary t = evalExprVector state flowVarMap boundary
          new_state <- eulerMaruyamaTraceDiag (runTrace traceMode flowVarMap) flowF noiseF evalBoundary (storeToVec flowVars state)
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
              --      return (toDyn (evalExprStore disc expr))
            -- Right expr -> store . at v ?= expr
      Choice prob n m -> do
          g <- asks gen
          disc <- use store
          val <- MWC.uniformR (0,1) g
          if val < evalExprStore disc prob
             then runSHP traceMode inputFun n
             else runSHP traceMode inputFun m
      Composition n m -> runSHP traceMode inputFun n >> runSHP traceMode inputFun m
      While p m -> do
          disc <- use store
          when (evalExprStore disc p) $ runSHP traceMode inputFun m >> runSHP traceMode inputFun shp
      Loop m -> do
          Config {maxTime=maxTime} <- ask
          t <- use time
          when (t <= maxTime) $ runSHP traceMode inputFun m >> runSHP traceMode inputFun shp
      Abort -> error "abort"
      Skip -> return ()
      Cond p n m -> do
          disc <- use store
          if evalExprStore disc p
             then runSHP traceMode inputFun n
             else runSHP traceMode inputFun m
