{-# LANGUAGE MonoLocalBinds #-}

module Tracing where

import Control.Monad.Primitive (PrimMonad)
import Control.Monad.RWS (Endo (Endo), appEndo, tell)
import Data.Map (Map)
import Data.Semigroup
import Data.Vector.Unboxed (Vector)
import Expression
import Lens.Micro.Platform (use)
import Types

runTrace :: (PrimMonad m) => TracingMode w -> Map String Int -> Trace m w
runTrace NoTrace _ _ = return ()
runTrace RawTrace _ state = tell (Endo ([state] ++))
runTrace tr flowVarMap vector = do
  state <- use store
  let
    evalExprHelper :: Expr a -> a
    evalExprHelper expr = evalExprVector state flowVarMap expr vector
  tell $ case tr of
    FullTrace e -> Endo ([evalExprHelper e] ++)
    MaxTrace e -> Just $ Max (evalExprHelper e)
    MinTrace e -> Just $ Min (evalExprHelper e)
    AnyTrace e -> Any (evalExprHelper e)
    AllTrace e -> All (evalExprHelper e)

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
