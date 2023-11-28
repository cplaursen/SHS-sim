module Execution where

import Euler_Maruyama
import Data.Vector (Vector, singleton, (!))
import qualified Data.Vector as V
import System.Random.MWC (createSystemRandom, withSystemRandomST)

import Parser
import Lexer
import Types
import AST_Operations
import Typecheck
import SHPTypes
import Data.Map ( fromList, empty, insert )
import Data.Bifunctor ( second )
import Control.Monad
import Lens.Micro.Platform

parseSHPBlocks :: String -> Either String Blocks
parseSHPBlocks prog = runAlex prog parseSHPProg

typecheckBlocks :: Blocks -> Either String SHP
typecheckBlocks blocks = typeCheckPSHP vars prog_untyped
    where
        prog_untyped = evalBlocks blocks
        vars = fromList $ fmap (second toASHPType . \(a,b,_) -> (a,b)) (blocks ^. varsBlock)

parseAndTypecheck :: String -> Either String SHP
parseAndTypecheck = parseSHPBlocks >=> typecheckBlocks
