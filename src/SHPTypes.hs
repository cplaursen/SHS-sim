{-# LANGUAGE TemplateHaskell, GADTs, StandaloneDeriving #-}
module SHPTypes where

import Data.Map.Strict (Map)
import Data.Vector (Vector)
import Data.Monoid (Endo)
import System.Random.MWC (Gen)
import Lens.Micro.Platform
import Control.Monad.RWS (RWST)
import Control.Monad.Primitive
import Data.Dynamic
import Data.Type.Equality
import Types

data Var a where
    Var :: Typeable a => String -> SHPType a -> Var a

deriving instance Show (Var a)

type Enumeration = String

-- Expressions
data BOperator a b c where
    Plus :: forall a. Num a => BOperator a a a
    Minus :: forall a. Num a => BOperator a a a
    Times :: forall a. Num a => BOperator a a a
    Divide :: forall a. Fractional a => BOperator a a a

    Eq :: forall a. Eq a => BOperator a a Bool
    Leq :: forall a. Ord a => BOperator a a Bool
    Le :: forall a. Ord a => BOperator a a Bool
    Geq :: forall a. Ord a => BOperator a a Bool
    Ge :: forall a. Ord a => BOperator a a Bool

deriving instance Show (BOperator a b c)

evalBOperator :: BOperator a b c -> a -> b -> c
evalBOperator b = case b of
                    Plus -> (+)
                    Minus -> (-)
                    Times -> (*)
                    Divide -> (/)
                    Eq -> (==)
                    Leq -> (<=)
                    Le -> (<)
                    Geq -> (>=)
                    Ge -> (>)

data UOperator a b where
    Neg :: forall a. Num a => UOperator a a
    Not :: UOperator Bool Bool

evalUOperator :: UOperator a b -> a -> b
evalUOperator Neg = negate
evalUOperator Not = not

deriving instance Show (UOperator a b)

data Expr a where
    Real :: Double -> Expr Double
    EBool :: Bool -> Expr Bool
    Enum :: String -> Expr String
    EVar :: Var a -> Expr a
    --Cont :: Int -> Expr Double
    Bop :: forall a b c. (Typeable a, Typeable b, Typeable c) =>
        BOperator a b c -> Expr a -> Expr b -> Expr c
    Uop :: forall a b. (Typeable a, Typeable b) =>
        UOperator a b -> Expr a -> Expr b

deriving instance Show (Expr a)

-- State
type Store = Map String Dynamic
--
-- Single differential equality - an ODE is a list of these
data Diff = Diff (Var Double) (Expr Double)
 deriving Show

-- SHP execution
type Flow = Vector Double -> Double -> Vector Double
type Noise = Vector Double -> Double -> Vector (Vector Double)

-- Holds a stochastic hybrid program
data SHP where
    Assn :: forall a. Var a -> Expr a -> SHP
    RandAssn :: Var Double -> Expr Double -> Expr Double -> SHP -- Uniform distribution [fst..snd]
    Choice :: Expr Double -> SHP -> SHP -> SHP
    Composition :: SHP -> SHP -> SHP
    While :: Expr Bool -> SHP -> SHP
    Abort :: SHP
    Skip :: SHP
    Cond :: Expr Bool -> SHP -> SHP -> SHP
    -- Can store SDEs as a single term to ensure the two lists refer to the same variables
    SDE :: [Diff] -> [Diff] -> Expr Bool -> SHP

deriving instance Show SHP
   
data SHPType a where
    SHPReal :: SHPType Double
    SHPBool :: SHPType Bool
    SHPEnum :: SHPType String

deriving instance Show (SHPType a)

instance TestEquality SHPType where
    testEquality :: SHPType a -> SHPType b -> Maybe (a :~: b)
    testEquality SHPReal SHPReal = Just Refl
    testEquality SHPBool SHPBool = Just Refl
    testEquality SHPEnum SHPEnum = Just Refl
    testEquality _       _       = Nothing

data ASHPType = forall a. Typeable a => ASHPType (SHPType a)

toASHPType :: PSHPType -> ASHPType
toASHPType HPReal = ASHPType SHPReal
toASHPType HPEnum = ASHPType SHPEnum
toASHPType HPBool = ASHPType SHPBool

deriving instance Show ASHPType

data Config m = Config
    { maxTime :: Double
    , dt :: Double
    , gen :: Gen (PrimState m)
    }

data State = State
    { _store :: Store
    , _time :: Double
    } 
    deriving Show

makeLenses ''State

type Execution m a = PrimMonad m => RWST (Config m) (Endo [Vector Double]) State m a
