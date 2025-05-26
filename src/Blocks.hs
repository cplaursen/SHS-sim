module Blocks where

import Data.Vector.Unboxed (Vector, singleton, (!))
import qualified Data.Vector as V
import System.Random.MWC (createSystemRandom, withSystemRandomST)

import Parser
import Lexer
import ParsingTypes
import AST_Operations
import Typecheck
import Types
import Data.List ( foldl' )
import Data.Map as Map ( fromList, empty, update, insert )
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Dynamic
import Data.Bifunctor ( second )
import Control.Monad
import Lens.Micro.Platform
import Expression (evalExpr, readAExpr)

parseSHPBlocks :: String -> Either String Blocks
parseSHPBlocks prog = runAlex prog parseSHPProg

blocksEnv :: Blocks -> Env
blocksEnv blocks = Map.fromList $ do
    declaration <- blocks ^. varsBlock
    case declaration of
        Left (vars, typ) -> fmap (, toASHPType typ) vars
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
