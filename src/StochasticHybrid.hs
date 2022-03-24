{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}
module StochasticHybrid where

import Data.Vector (Vector)

import Data.HashMap.Strict (HashMap, (!), (!?), insert)

import Control.Monad.Writer (runWriter)

import Control.Monad.Primitive (PrimState)
import Control.Monad.RWS (RWST, ask, get, tell, put, modify)

import Lens.Micro.Platform

import SHPParser
import SHPLexer

import qualified System.Random.MWC as MWC
import System.Random.MWC.Distributions (standard)

import Euler_Maruyama

type Vars = HashMap String Double

data State = State
    { _discrete :: Vars
    , _continuous :: Vector Double
    , _indices :: HashMap String Int
    } deriving (Show, Eq)

makeLenses ''State

type Execution = RWST Config [Vector Double] State IO ()

{- TODO
    - Implement pre-SHP definition blocks
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

evalExpr :: Expr -> Vars -> Double
evalExpr exp s =
    case exp of
      Real x -> x
      Var v -> s ! v
      Bop op a b -> evalExpr a s `op` evalExpr b s

evalPred :: Pred -> Vars -> Bool
evalPred pred s =
    case pred of
        Compare op a b -> evalExpr a s `op` evalExpr b s
        And p q -> evalPred p s && evalPred q s
        Or p q -> evalPred p s || evalPred q s
        Not p -> not $ evalPred p s
        Bool b -> b

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
          disc <- use discrete
          (discrete . at v) ?= (evalExpr exp (state ^. discrete))
      RandAssn v lo hi -> do
          g <- gets gen
          disc <- use discrete
          val <- MWC.uniformR ((evalExpr hi disc), (evalExpr lo disc)) g
          discrete . at v ?= val
      Choice prob n m -> do
          g <- gets gen
          val <- MWC.uniformR (0,1) g
          if val < prob
             then runSHP n
             else runSHP m
      Composition n m -> runSHP n >> runSHP m
      While p m -> do
          disc <- use discrete
          if evalPred p disc
             then runSHP m >> runSHP shp
             else return ()
      Abort -> error "abort"
      Skip -> return ()
      Cond p n m -> do
          disc <- gets discrete
          if evalPred p disc
             then runSHP n
             else runSHP m
