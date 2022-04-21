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

data SHP = Assn String Expr
      | RandAssn String Expr Expr -- Uniform distribution [fst..snd]
      | Choice Double SHP SHP
      | Composition SHP SHP
      | While Pred SHP
      | Abort -- Replaces test
      | Skip
      | Cond Pred SHP SHP
      | SDE [Diff] [Diff] Pred
      deriving (Show, Eq)
   
data Diff = Diff String Expr
    deriving (Show, Eq)

data Expr = Real Double
          | Var String
          | Cont Int -- Continuous variable - not used in parsing
          | Const String -- Enumeration
          | Bop String Expr Expr -- Binary Operation
          deriving (Show, Eq)

data Pred = Compare String Expr Expr
          | And Pred Pred
          | Or Pred Pred
          | Not Pred
          | Bool Bool
          deriving (Show, Eq)

data Block = SHPBlock SHP | DefBlock String [Definition]
    deriving (Show, Eq)

data Definition = Definition String Expr
    deriving (Show, Eq)

-- SHP
type Flow = Vector Double -> Double -> Vector Double 
type Noise = Vector Double -> Double -> Vector (Vector Double)

type Vars = HashMap String Double

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

