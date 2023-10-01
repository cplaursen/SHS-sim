{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)
import System.Random.MWC (GenST)
import Lens.Micro.Platform
import Control.Monad.RWS (RWST)
import Control.Monad.ST (ST)

-- Lexing
data Token = TokenIdent String
           | TokenLParen
           | TokenRParen 
           | TokenReal Double
           | TokenBool Bool
           | TokenDW     
           | TokenDT     
           | TokenAbort  
           | TokenSkip
           | TokenIf 
           | TokenThen 
           | TokenElse  
           | TokenWhile
           | TokenSHP
           | TokenPrime
           | TokenComma
           | TokenLCurl  
           | TokenRCurl  
           | TokenSemi  
           | TokenAmpersand
           | TokenAssign  
           | TokenStar  
           | TokenUnion 
           | TokenQuestion 
           | TokenOr
           | TokenAnd
           | TokenNot
           | TokenLEQ
           | TokenGEQ
           | TokenLT
           | TokenGT
           | TokenEq
           | TokenPlus
           | TokenMinus
           | TokenDiv
           deriving (Show, Eq)

-- Parsing

-- Holds a stochastic hybrid program
data SHP = Assn String Expr
      | RandAssn String ArithExpr ArithExpr -- Uniform distribution [fst..snd]
      | Choice Double SHP SHP
      | Composition SHP SHP
      | While Pred SHP
      | Abort -- Replaces test
      | Skip
      | Cond Pred SHP SHP
      | SDE [Diff] [Diff] Pred
      deriving (Show, Eq)
   
-- Single differential equality - an ODE is a list of these
data Diff = Diff String ArithExpr
    deriving (Show, Eq)

-- An expression is either arithmetic or some enumeration symbol
data Expr = ArithExpr ArithExpr | Enum String -- Enumeration constant
    deriving (Show, Eq)

-- Arithmetic expressions always evaluate to a Double
--  Change Double to rationals (or something else)
data ArithExpr = Real Double
          | Var String
          | Cont Int -- Continuous variable - not used in parsing
          | Bop String ArithExpr ArithExpr -- Binary Operation
          deriving (Show, Eq)

-- Predicates on expressions
data Pred = PredEq Expr Expr
          | Compare String ArithExpr ArithExpr
          | And Pred Pred
          | Or Pred Pred
          | Not Pred
          | Bool Bool
          deriving (Show, Eq)

-- Single definition in a block
data Definition = Definition String ArithExpr
    deriving (Show, Eq)

-- Holds an SHP, together with any meta-information about it: constants, symbols and continuous variables
data Blocks = Blocks
    { _shpBlock :: SHP
    , _constBlock :: [Definition]
    , _enumBlock :: [String]
    , _contBlock :: [String]
    }
    deriving (Show, Eq)

makeLenses ''Blocks

-- SHP
type Flow = Vector Double -> Double -> Vector Double 
type Noise = Vector Double -> Double -> Vector (Vector Double)

data SHPTypes = SHPReal Double | SHPBool Bool | SHPEnum String
    deriving (Show, Eq)

fromSHPReal :: SHPTypes -> Double
fromSHPReal (SHPReal a) = a
fromSHPReal (SHPEnum a) = error ("Type error - expected number: " ++ a)

type Vars = HashMap String SHPTypes

data Config s = Config
    { maxTime :: Double
    , dt :: Double
    , gen :: GenST s
    , contIx :: HashMap String Int
    }

data State = State
    { _discrete :: Vars
    , _continuous :: Vector Double
    , _time :: Double
    } deriving (Show, Eq)

makeLenses ''State

type Execution s = RWST (Config s) [Vector Double] State (ST s) ()

