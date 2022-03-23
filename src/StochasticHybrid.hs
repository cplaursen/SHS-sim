{-# LANGUAGE NamedFieldPuns #-}
module StochasticHybrid where

import Data.Vector.Unboxed (Vector)

import Data.HashMap.Strict (HashMap, (!), (!?), insert)

import Control.Monad.Writer (runWriter)

import Control.Monad.Primitive (PrimState)
import Control.Monad.RWS (RWST, ask, get, tell, put)

import SHPParser
import SHPLexer

import qualified System.Random.MWC as MWC
import System.Random.MWC.Distributions (standard)

import Euler_Maruyama

type Vars = HashMap String Double

type Execution = RWST Config [Vector Double] Vars IO ()

{- TODO
    - Implement pre-SHP definition blocks
    - Extract vectors of continuous variables from the state
    - Implement SHS evolution
-}

mapPredExpr :: (Expr -> Expr) -> Pred -> Pred
mapPredExpr f pred = case pred of
                    Compare op m n -> Compare op (f m) (f n)
                    And p q -> And (mapPredExpr f p) (mapPredExpr f q)
                    Or p q -> Or (mapPredExpr f p) (mapPredExpr f q)
                    Not p -> Not (mapPredExpr f p)
                    _ -> pred

mapSHPExpr :: (Expr -> Expr) -> SHP -> SHP
mapSHPExpr f shp =
    case shp of
      SDE drift noise boundary ->
          let fDiff (Diff s e) = Diff s (f e)
           in SDE (map fDiff drift) (map fDiff noise) (mapPredExpr f boundary)
      Assn v exp -> Assn v (f exp)
      Choice p n m -> Choice p (mapSHPExpr f n) (mapSHPExpr f m)
      Composition n m -> Composition (mapSHPExpr f n) (mapSHPExpr f m)
      While p m -> While (mapPredExpr f p) (mapSHPExpr f m)
      Cond p m n -> Cond (mapPredExpr f p) (mapSHPExpr f n) (mapSHPExpr f m)
      _ -> shp


replaceVarExpr :: Vars -> Expr -> Expr
replaceVarExpr consts e = 
    case e of
      Var v -> maybe e Real (consts !? v)
      Bop op a b -> Bop op (replaceVarExpr consts a) (replaceVarExpr consts b)
      _ -> e

replaceVarSHP = mapSHPExpr . replaceVarExpr 

evalExpr :: Vars -> Expr -> Double
evalExpr s exp =
    case exp of
      Real x -> x
      Var v -> s ! v
      Bop op a b -> evalExpr s a `op` evalExpr s b

evalPred :: Vars -> Pred -> Bool
evalPred s pred =
    case pred of
        Compare op a b -> evalExpr s a `op` evalExpr s b
        And p q -> evalPred s p && evalPred s q
        Or p q -> evalPred s p || evalPred s q
        Not p -> not $ evalPred s p
        Bool b -> b

-- diffToFlow :: [Diff] -> State -> Flow
-- diffToFlow diff s = undefined

runSHP :: SHP -> Execution
runSHP shp =
    case shp of
      {-SDE drift noise boundary -> do
          state <- get
          opts <- ask
          (newState, trace) <- runWriter $ euler_maruyama flow noise state time cfg -- TODO
          tell trace
          put newState-}
      Assn v exp -> do
          s <- get 
          put (insert v (evalExpr s exp) s)
      RandAssn v lo hi -> do
          opts <- ask
          state <- get
          val <- MWC.uniformR ((evalExpr state hi), (evalExpr state lo)) (gen opts)
          put $ insert v val state
      Choice prob n m -> do
          opts <- ask
          val <- MWC.uniformR (0,1) (gen opts)
          if val < prob then runSHP n else runSHP m
      Composition n m -> runSHP n >> runSHP m
      While p m -> do
          state <- get
          if evalPred state p
             then runSHP m >> runSHP shp
             else return ()
      Abort -> error "abort"
      Skip -> return ()
      Cond pred n m -> do
          state <- get
          if (evalPred state pred) then runSHP n else runSHP m
