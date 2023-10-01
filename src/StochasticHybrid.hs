module StochasticHybrid where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.HashMap.Strict (HashMap, (!), (!?), fromList)
import Data.List (sortBy)

import Control.Monad.Writer.Lazy (runWriterT)

import Control.Monad.Primitive (PrimState)
import Control.Monad.RWS (RWST, ask, get, asks, tell, put, lift)

import Lens.Micro.Platform

import SHPParser
import SHPLexer
import Types

import qualified System.Random.MWC as MWC
import System.Random.MWC.Distributions (standard)

import Euler_Maruyama

{- TODO
    - Implement definition blocks for continuous variables, constants and enumerations
    - Split the normalisation into several data types, e.g. SHP_parsed, SHP_norm, etc.
-}

liftArithExpr :: (ArithExpr -> ArithExpr) -> Expr -> Expr
liftArithExpr f (ArithExpr e) = ArithExpr (f e)
liftArithExpr f (Enum e) = Enum e

toArithExpr :: Expr -> ArithExpr
toArithExpr (ArithExpr e) = e
toArithExpr (Enum e) = error ("Expected arithmetic expression, got enum: " ++ e)

-- Apply f to all the variable names and replace the variable with the result
substVar :: (String -> ArithExpr) -> ArithExpr -> ArithExpr
substVar f (Var s) = f s
substVar f (Bop op a b) = Bop op (substVar f a) (substVar f b)
substVar f e = e

mapPredArithExpr :: (ArithExpr -> ArithExpr) -> Pred -> Pred
mapPredArithExpr f pred = case pred of
                    PredEq m n -> PredEq (liftArithExpr f m) (liftArithExpr f n)
                    Compare op m n -> Compare op (f m) (f n)
                    And p q -> And (mapPredArithExpr f p) (mapPredArithExpr f q)
                    Or p q -> Or (mapPredArithExpr f p) (mapPredArithExpr f q)
                    Not p -> Not (mapPredArithExpr f p)
                    _ -> pred

mapPredExpr :: (Expr -> Expr) -> Pred -> Pred
mapPredExpr f pred =
    case pred of
      PredEq m n -> PredEq (f m) (f n)
      Compare op m n -> Compare op
                                (toArithExpr (f (ArithExpr m)))
                                (toArithExpr (f (ArithExpr n)))
      And p q -> And (mapPredExpr f p) (mapPredExpr f q)
      Or p q -> Or (mapPredExpr f p) (mapPredExpr f q)
      Not p -> Not (mapPredExpr f p)
      _ -> pred


-- Map some function on expressions over every expression in an SHP
mapSHPExpr :: (Expr -> Expr) -> SHP -> SHP
mapSHPExpr f shp =
    case shp of
      SDE drift noise boundary ->
          let fDiff (Diff s e) = Diff s (toArithExpr (f (ArithExpr e)))
           in SDE (map fDiff drift) (map fDiff noise) (mapPredExpr f boundary)
      Assn v exp -> Assn v (f exp)
      Choice p n m -> Choice p (mapSHPExpr f n) (mapSHPExpr f m)
      Composition n m -> Composition (mapSHPExpr f n) (mapSHPExpr f m)
      While p m -> While (mapPredExpr f p) (mapSHPExpr f m)
      Cond p m n -> Cond (mapPredExpr f p) (mapSHPExpr f n) (mapSHPExpr f m)
      _ -> shp

-- Replace variables with enums.
--   TODO: Raise an error if an enum is mentioned deep in some arithmetic expression
replaceEnums :: [String] -> SHP -> SHP
replaceEnums enums = mapSHPExpr f
    where f e@(ArithExpr (Var str)) = if str `elem` enums then Enum str else e
          f e = e

-- Replace variables mentioned in definitions with their assigned value
replaceConsts :: [Definition] -> SHP -> SHP
replaceConsts defs = mapSHPExpr (liftArithExpr (substVar f))
    where consts_map = fromList (map (\(Definition a b) -> (a,b)) defs)
          f str = maybe (Var str) id (consts_map !? str)

replaceVarExpr :: Vars -> ArithExpr -> ArithExpr
replaceVarExpr vars = substVar (\s -> maybe (Var s) (Real) (fromSHPReal <$> (vars !? s)))

replaceVarSHP :: Vars -> SHP -> SHP
replaceVarSHP = mapSHPExpr . liftArithExpr . replaceVarExpr

-- Replace variables tagged as continuous with their index in the continuous vector
replaceContVars :: HashMap String Int -> ArithExpr -> ArithExpr
replaceContVars vars = substVar (\s -> (maybe (Var s) Cont (vars !? s)))

-- Construct vector of ODEs, with order given by indices
diffsToVec :: HashMap String Int -> Int -> [Diff] -> Vector ArithExpr
diffsToVec indices vectorLength diffs =
    let pairs = map (\(Diff s e) -> (indices ! s, e)) diffs
        sortedPairs = sortBy (\(a,_) (b,_) -> compare a b) pairs
        makeList n [] = replicate (vectorLength-n-1) (Real 0)
        makeList n lst@((ix,e):xs) = if ix == n
                                        then e : makeList (n+1) xs
                                        else Real 0 : makeList (n+1) lst
     in V.fromList $ makeList 0 sortedPairs

-- Execute the replacement specified by the definition blocks on the SHP
evalBlocks :: Blocks -> SHP
evalBlocks (Blocks shp consts enums conts) = replaceEnums enums $ replaceConsts consts shp
        

evalOp :: String -> (Double -> Double -> Double)
evalOp s = case s of
             "+" -> (+)
             "-" -> (-)
             "*" -> (*)
             "/" -> (/)
             _   -> error ("Undefined binary operator: " ++ s)

-- Discrete variables only - might allow continuous variables in the future
evalExpr :: Expr -> Vars -> SHPTypes
evalExpr (Enum s) _ = SHPEnum s
evalExpr (ArithExpr exp) s =
    case exp of
      Real x -> SHPReal x
      Var v -> (s ! v)
      Bop op a b -> SHPReal $ (evalOp op) (evalArithExpr a s) (evalArithExpr b s)
      Cont _ -> error "Continuous variables are not allowed in discrete expressions"

evalArithExpr :: ArithExpr -> Vars -> Double
evalArithExpr exp s =
    case exp of
      Real x -> x
      Var v -> fromSHPReal (s ! v)
      Bop op a b -> (evalOp op) (evalArithExpr a s) (evalArithExpr b s)
      Cont _ -> error "Continuous variables are not allowed in discrete expressions"

-- Continuous variables only - use replaceVarExpr on discrete ones
evalContExpr :: ArithExpr -> Vector Double -> Double
evalContExpr exp vec =
    case exp of
      Real x -> x
      Bop op a b -> (evalOp op) (evalContExpr a vec) (evalContExpr b vec)
      Cont ix -> vec V.! ix
      Var v -> error "Discrete variables should be replaced prior to using evalContExpr"

evalComp :: String -> (Double -> Double -> Bool)
evalComp s = case s of
               "<"  -> (<)
               ">"  -> (>)
               ">=" -> (>=)
               "<=" -> (<=)
               _    -> error ("Undefined comparison operator: " ++ s)

evalPred :: Pred -> Vars -> Bool
evalPred pred s =
    case pred of
        PredEq a b -> a == b
        Compare op a b -> (evalComp op) (evalArithExpr a s) (evalArithExpr b s)
        And p q -> evalPred p s && evalPred q s
        Or p q -> evalPred p s || evalPred q s
        Not p -> not $ evalPred p s
        Bool b -> b

evalContPred :: Pred -> Vector Double -> Bool
evalContPred pred s =
    case pred of
        Compare op a b -> (evalComp op) (evalContExpr a s) (evalContExpr b s)
        And p q -> evalContPred p s && evalContPred q s
        Or p q -> evalContPred p s || evalContPred q s
        Not p -> not $ evalContPred p s
        Bool b -> b

diffsToFlow :: Vars -> HashMap String Int -> Int -> [Diff] -> Flow
diffsToFlow vars indices vecLength diffs y t =
    let evalDiffs = map (\(Diff i e) -> Diff i (replaceVarExpr vars e)) diffs
        exprVec = diffsToVec indices vecLength evalDiffs
     in fmap ((flip evalContExpr) y) exprVec

diag :: a -> Vector a -> Vector (Vector a)
diag zero vec = fmap (V.fromList . (helper 0)) (V.enumFromN 0 (length vec))
    where helper n k
            | n >= length vec = []
            | k == n = vec V.! n : helper (n+1) k
            | otherwise = zero : helper (n+1) k

runSHP :: SHP -> Execution s
runSHP shp =
    case shp of
      SDE drift noise boundary -> do
          state <- get
          opts <- ask
          let disc = state ^. discrete
          let cont = state ^. continuous
          let flowF = diffsToFlow disc (contIx opts) (length cont) drift
          let noiseF y t = diag 0 (diffsToFlow disc (contIx opts) (length cont) noise y t)
          let boundary' = mapPredArithExpr (replaceContVars (contIx opts) . replaceVarExpr disc) boundary
          -- lift $ print $ noiseF cont 0
          em_result <- lift $ 
              euler_maruyama flowF noiseF cont (state ^. time) (\v t -> evalContPred boundary' v && t < (maxTime opts)) (dt opts) (gen opts) []
          let ((t,y): trace) = em_result
          tell (fmap snd trace)
          put (State disc y t)
      Assn v exp -> do
          disc <- use discrete
          (discrete . at v) ?= (evalExpr exp disc)
      RandAssn v lo hi -> do
          g <- asks gen
          disc <- use discrete
          val <- MWC.uniformR ((evalArithExpr hi disc), (evalArithExpr lo disc)) g
          discrete . at v ?= (SHPReal val)
      Choice prob n m -> do
          g <- asks gen
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
          disc <- use discrete
          if evalPred p disc
             then runSHP n
             else runSHP m
