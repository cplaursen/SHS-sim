module Execution where

import Data.Vector (Vector, singleton, (!))
import qualified Data.Vector as V
import System.Random.MWC (createSystemRandom, withSystemRandomST)

import Parser
import Lexer
import ParsingTypes
import AST_Operations
import Typecheck
import Types
import Data.Map as Map
import Data.Set as Set
import Data.Bifunctor ( second )
import Control.Monad
import Lens.Micro.Platform

parseSHPBlocks :: String -> Either String Blocks
parseSHPBlocks prog = runAlex prog parseSHPProg

getEnv :: Blocks -> Env
getEnv blocks = Map.fromList $ fmap (second toASHPType . \(a,b,_) -> (a,b)) (blocks ^. varsBlock)

getEnums :: Blocks -> Set String
getEnums blocks = Set.fromList $ blocks ^. enumBlock

typecheckBlocks :: Blocks -> Either String SHP
typecheckBlocks blocks = typeCheckPSHP vars prog_untyped
    where
        prog_untyped = evalBlocks blocks
        vars = getEnv blocks

parseAndTypecheck :: String -> Either String SHP
parseAndTypecheck = parseSHPBlocks >=> typecheckBlocks
