{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)
import System.Random.MWC (GenIO)
import Lens.Micro.Platform
import Control.Monad.RWS (RWST)

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
   
data Diff = Diff String Expr

data Expr = Real Double
          | Var String
          | Cont Int -- Continuous variable - not used in parsing
          | Const String -- Enumeration
          | Bop (Double -> Double -> Double) Expr Expr -- Binary Operation

data Pred = Compare (Double -> Double -> Bool) Expr Expr
          | And Pred Pred
          | Or Pred Pred
          | Not Pred
          | Bool Bool

data Block = SHPBlock SHP | DefBlock String [Definition]

data Definition = Definition String Expr

-- SHP
type Flow = Vector Double -> Double -> Vector Double 
type Noise = Vector Double -> Double -> Vector (Vector Double)

type Vars = HashMap String Double

data Config = Config
    { maxTime :: Double
    , dt :: Double
    , gen :: GenIO
    , contIx :: HashMap String Int
    }

data State = State
    { _discrete :: Vars
    , _continuous :: Vector Double
    , _time :: Double
    } deriving (Show, Eq)

makeLenses ''State

type Execution = RWST Config [Vector Double] State IO ()

