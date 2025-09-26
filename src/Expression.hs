{-# LANGUAGE MonoLocalBinds #-}

module Expression where

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.List (sortBy)
import Data.Map.Strict (Map, fromList, insert, keysSet, mapWithKey, (!), (!?))
import Data.Maybe (fromJust)
import Data.Set (Set, isSubsetOf, toList, (\\))
import Data.Vector.Unboxed (Unbox, Vector)
import Data.Vector.Unboxed qualified as V
import Type.Reflection

import Control.Monad.Writer.Lazy (runWriterT)

import Control.Monad.Primitive (PrimState)
import Control.Monad.RWS (ask, asks, get, guard, lift, put, tell, when)

import Lens.Micro.Platform (at, use, (.=), (?=))

import AST_Operations
import Lexer (runAlex)
import Parser (parseExpr)
import Types

import System.IO (hPutStrLn, stderr)
import System.Random.MWC qualified as MWC
import System.Random.MWC.Distributions (standard)

import Data.HashMap.Internal.Strict qualified as M
import Typecheck (testEqualityEither, typecheckPExpr)

{-# INLINE evalExpr #-}
evalExpr :: (String -> Dynamic) -> Expr a -> a
evalExpr ctx expr =
  case expr of
    EReal r -> r
    EVar (Var x typ) -> case fromDynamic (ctx x) of
      Just v -> v
      Nothing -> error $ "Compiler type error: expected variable of type " ++ show typ ++ ", got " ++ show (ctx x)
    EBool b -> b
    EBop op -> evalBOperator op
    EUop op -> evalUOperator op
    EApp f e -> evalExpr ctx f (evalExpr ctx e)
    EEnum x -> x

{-# INLINE evalExprStore #-}
evalExprStore :: Store -> Expr a -> a
evalExprStore store = evalExpr (store !)

-- Evaluates an expression, evaluating variables to their vector index if they appear in the map
{-# INLINE evalExprVector #-}
evalExprVector :: Store -> Map String Int -> Expr a -> Vector Double -> a
evalExprVector ctx variables expr vector = evalExpr helper expr
 where
  helper str = case variables !? str of
    Just ix -> toDyn (vector V.! ix)
    Nothing -> ctx ! str

overrideStore :: Store -> Map String Int -> Vector Double -> Store
overrideStore ctx variables vector = mapWithKey helper ctx
 where
  helper key value = case variables !? key of
    Just ix -> toDyn $ vector V.! ix
    Nothing -> value

-- Diagonal n x n matrix from length n vector
-- diag :: Unbox a => a -> Vector a -> Vector (Vector a)
-- diag fill vec = V.map (V.fromList . helper 0) (V.enumFromN 0 (V.length vec))
--     where helper n k
--             | n >= V.length vec = []
--             | k == n = vec V.! n : helper (n+1) k
--             | otherwise = fill : helper (n+1) k

-- Assumes all variables in vars are Double-valued in the store
storeToVec :: [String] -> Store -> Vector Double
storeToVec vars store = V.fromList $ map (fromJust . fromDynamic . (store !)) vars

-- Assumes vars and vector are aligned
vecToStore :: [String] -> Store -> Vector Double -> Store
vecToStore vars store vector = foldr (uncurry insert) store keyValues
 where
  keyValues = zip vars (map toDyn (V.toList vector))

getDiffVar :: Diff -> String
getDiffVar (Diff (Var s _) _) = s

getDiffExpr :: Diff -> Expr Double
getDiffExpr (Diff _ exp) = exp

readAExpr :: Env -> Set String -> String -> Either String AExpr
readAExpr env enums string = do
  parsed <- runAlex string parseExpr
  let withEnums = replaceEnum enums parsed
  let vars = allPExprVars withEnums
  let diff = vars \\ keysSet env
  if null diff
    then Right ()
    else Left $ "Unrecognised variable names: " ++ unwords (toList diff)
  typecheckPExpr env withEnums
