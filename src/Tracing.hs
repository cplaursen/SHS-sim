{-# LANGUAGE MonoLocalBinds #-}
module Tracing where

import Types
import Control.Monad.RWS (Endo (Endo), appEndo, tell )
import Lens.Micro.Platform (use)
import Data.Map (Map)
import Data.Semigroup
import Data.Vector.Unboxed (Vector)
import Expression

type Trace m w = Vector Double -> Execution m w ()

data TracingMode w where
    RawTrace :: TracingMode (Endo [Vector Double])
    FullTrace :: Expr Double -> TracingMode (Endo [Double]) 
    MinTrace :: Expr Double -> TracingMode (Maybe (Min Double))
    MaxTrace :: Expr Double -> TracingMode (Maybe (Max Double))
    AnyTrace :: Expr Bool -> TracingMode Any
    AllTrace :: Expr Bool -> TracingMode All
    NoTrace :: TracingMode ()

data SomeTracingMode = forall a. Monoid a => SomeTracingMode (TracingMode a)

runTrace :: TracingMode w -> Map String Int -> Trace m w
runTrace RawTrace _ state = tell (Endo ([state] ++))
runTrace (FullTrace expr) flowVarMap vector = do
    state <- use store
    tell (Endo ([evalExprVector state flowVarMap expr vector] ++))
runTrace (MaxTrace expr) flowVarMap vector = do
    state <- use store
    tell (Just $ Max (evalExprVector state flowVarMap expr vector))
runTrace (MinTrace expr) flowVarMap vector = do
    state <- use store
    tell (Just $ Min (evalExprVector state flowVarMap expr vector))
runTrace (AnyTrace expr) flowVarMap vector = do
    state <- use store
    tell (Any (evalExprVector state flowVarMap expr vector))
runTrace (AllTrace expr) flowVarMap vector = do
    state <- use store
    tell (All (evalExprVector state flowVarMap expr vector))
runTrace NoTrace _ _ = return ()

extractTrace :: TracingMode w -> w -> String
extractTrace RawTrace tr = show $ appEndo tr []
extractTrace (FullTrace _) tr = show $ appEndo tr []
extractTrace (MaxTrace _) tr = case tr of
    Just max -> show $ getMax max
    Nothing -> "NaN"
extractTrace (MinTrace _) tr = case tr of
    Just min -> show $ getMin min
    Nothing -> "NaN"
extractTrace (AnyTrace _) tr = show $ getAny tr
extractTrace (AllTrace _) tr = show $ getAll tr
extractTrace NoTrace _ = ""
