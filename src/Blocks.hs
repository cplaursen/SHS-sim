module Blocks where

import Data.Vector qualified as V
import Data.Vector.Unboxed (Vector, singleton, (!))
import System.Random.MWC (createSystemRandom, withSystemRandomST)

import AST_Operations
import Control.Monad
import Data.Bifunctor (second)
import Data.Dynamic
import Data.List (foldl')
import Data.Map as Map (empty, fromList, insert, update)
import Data.Set (Set)
import Data.Set qualified as Set
import Expression (evalExpr, readAExpr)
import Lens.Micro.Platform
import Lexer
import Parser
import ParsingTypes
import Typecheck
import Types

parseSHPBlocks :: String -> Either String Blocks
parseSHPBlocks prog = runAlex prog parseSHPProg

blocksEnv :: Blocks -> Env
blocksEnv blocks = Map.fromList $ do
  declaration <- blocks ^. varsBlock
  case declaration of
    Left (vars, typ) -> fmap (,toASHPType typ) vars
    Right (var, typ, expr) -> return (var, toASHPType typ)

blocksEnums :: Blocks -> Set String
blocksEnums blocks = Set.fromList $ blocks ^. enumBlock

typecheckBlocks :: Blocks -> Either String SHP
typecheckBlocks blocks = typecheckPSHP vars prog_untyped
 where
  prog_untyped = evalBlocks blocks
  vars = blocksEnv blocks

parseAndTypecheck :: String -> Either String SHP
parseAndTypecheck = parseSHPBlocks >=> typecheckBlocks
