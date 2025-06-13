{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module ParsingTypes where

import Control.Monad (ap)
import Data.Generics (Data)
import Data.Vector (Vector)
import Lens.Micro.Platform (makeLenses)

-- Lexing
data Token
  = TokenIdent String
  | TokenLParen
  | TokenRParen
  | TokenReal Double
  | TokenBool Bool
  | TokenDW
  | TokenDT
  | TokenSkip
  | TokenInput
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenWhile
  | TokenLoop
  | TokenChoice
  | TokenSHP
  | TokenPrime
  | TokenComma
  | TokenLCurl
  | TokenRCurl
  | TokenSemi
  | TokenAmpersand
  | TokenAssign
  | TokenStar
  | TokenQuestion
  | TokenOr
  | TokenAnd
  | TokenNot
  | TokenSin
  | TokenCos
  | TokenLEQ
  | TokenGEQ
  | TokenLT
  | TokenGT
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenDiv
  | TokenPow
  | TokenEOF
  deriving (Show, Eq)

-- Parsing
type Variable = String

data PExpr
  = PReal Double
  | PBool Bool
  | PEnum String
  | PVar Variable
  | PBop String PExpr PExpr
  | PUop String PExpr
  deriving (Show, Eq, Data)

data PSHP
  = PAssn Variable PExpr
  | PRandAssn Variable PExpr PExpr
  | PInput Variable
  | PComp PSHP PSHP
  | PWhile PExpr PSHP
  | PCond PExpr PSHP PSHP
  | PSkip
  | PSDE [PDiff] [PDiff] PExpr
  deriving (Show, Eq, Data)

-- Single differential equality - an ODE is a list of these
data PDiff = PDiff Variable PExpr
  deriving (Show, Eq, Data)

-- Single definition in a block
data Def = Def Variable PExpr
  deriving (Show, Eq, Data)

data PSHPType = HPReal | HPBool | HPInt | HPEnum
  deriving (Show, Eq, Data)

-- Holds an SHP, together with any meta-information about it: constants, symbols and variables
data Blocks = Blocks
  { _shpBlock :: PSHP
  , _constBlock :: [Def]
  , _enumBlock :: [String]
  , _varsBlock :: [Either ([Variable], PSHPType) (Variable, PSHPType, PExpr)]
  }
  deriving (Show, Eq)

makeLenses ''Blocks

-- Exception monad for the parser
data E a = Ok a | Failed String
  deriving (Eq, Show, Functor)

instance Applicative E where
  pure = Ok
  (<*>) = ap

instance Monad E where
  return = pure
  m >>= f = case m of
    Ok a -> f a
    Failed x -> Failed x
