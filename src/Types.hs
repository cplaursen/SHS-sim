{-# LANGUAGE TemplateHaskell, GADTs, StandaloneDeriving #-}
module Types where

import Data.Map.Strict ( Map )
import Data.Vector.Unboxed (Vector)
import Data.Set ( Set )
import Data.Semigroup ( Min, Max )

import Data.Dynamic
import Data.Type.Equality
import Data.Kind

import System.Random.MWC (Gen)

import Lens.Micro.Platform

import Control.Monad.RWS ( RWST, Endo )
import Control.Monad.Primitive
import Data.Primitive.MutVar

import ParsingTypes

{---------------------------
-- Type universe for SHPs --
----------------------------}

data SHPType a where
    SHPReal :: SHPType Double
    SHPBool :: SHPType Bool
    SHPEnum :: SHPType String
    SHPInt  :: SHPType Int
    (:->)  :: forall arg res. Typeable res =>
        SHPType arg -> SHPType res -> SHPType (arg -> res)
infixr 0 :->

deriving instance Show (SHPType a)

instance TestEquality SHPType where
    testEquality :: SHPType a -> SHPType b -> Maybe (a :~: b)
    testEquality SHPReal SHPReal = Just Refl
    testEquality SHPBool SHPBool = Just Refl
    testEquality SHPEnum SHPEnum = Just Refl
    testEquality (a :-> b) (c :-> d) = do
        Refl <- testEquality a c
        Refl <- testEquality b d
        return Refl
    testEquality _       _       = Nothing

-- Existential for SHPType lets us hide the type information to e.g. store it in a map
data ASHPType = forall a. Typeable a => ASHPType (SHPType a)
deriving instance Show ASHPType

toASHPType :: PSHPType -> ASHPType
toASHPType HPReal = ASHPType SHPReal
toASHPType HPInt  = ASHPType SHPInt
toASHPType HPEnum = ASHPType SHPEnum
toASHPType HPBool = ASHPType SHPBool

-- Utility function to unwrap and rewrap the existentially quantified ASHPType
unwrapASHPType :: (forall a. Typeable a => SHPType a -> b) -> ASHPType -> b
unwrapASHPType f a = case a of
                       ASHPType x -> f x

instance Eq ASHPType where
    ASHPType t1 == ASHPType t2 =
        case t1 `testEquality` t2 of
          Just Refl -> True
          Nothing -> False

{--------------------------------------------
-- Elements of a stochastic hybrid program --
---------------------------------------------}

-- Variables are strings that carry their type with them
data Var a where
    Var :: Typeable a => String -> SHPType a -> Var a

deriving instance Show (Var a)

-- BOperator is a dictionary of sorts for our allowed binary operators
data BOperator a b c where
    Plus :: forall a. Num a => BOperator a a a
    Minus :: forall a. Num a => BOperator a a a
    Times :: forall a. Num a => BOperator a a a
    Divide :: forall a. Fractional a => BOperator a a a
    Power :: forall a. Integral a => BOperator a a a

    Eq :: forall a. Eq a => BOperator a a Bool
    Leq :: forall a. Ord a => BOperator a a Bool
    Le :: forall a. Ord a => BOperator a a Bool
    Geq :: forall a. Ord a => BOperator a a Bool
    Ge :: forall a. Ord a => BOperator a a Bool

    And :: BOperator Bool Bool Bool
    Or :: BOperator Bool Bool Bool

deriving instance Show (BOperator a b c)

evalBOperator :: BOperator a b c -> a -> b -> c
evalBOperator b = case b of
                    Plus -> (+)
                    Minus -> (-)
                    Times -> (*)
                    Divide -> (/)
                    Power -> (^)
                    Eq -> (==)
                    Leq -> (<=)
                    Le -> (<)
                    Geq -> (>=)
                    Ge -> (>)
                    And -> (&&)
                    Or -> (||)

data ABOp = forall a b c. (Typeable a, Typeable b, Typeable c) =>
    ABOp (BOperator a b c) (SHPType a) (SHPType b) (SHPType c)

data UOperator a b where
    Neg :: forall a. Num a => UOperator a a
    Not :: UOperator Bool Bool
    Sin :: UOperator Double Double
    Cos :: UOperator Double Double

deriving instance Show (UOperator a b)

evalUOperator :: UOperator a b -> a -> b
evalUOperator Neg = negate
evalUOperator Not = not
evalUOperator Sin = sin
evalUOperator Cos = cos

data AUOp = forall a b. (Typeable a, Typeable b) =>
    AUOp (UOperator a b) (SHPType a) (SHPType b)

data Expr a where
    EReal :: Double -> Expr Double
    EBool :: Bool -> Expr Bool
    EEnum :: String -> Expr String
    EVar :: Var a -> Expr a
    EApp :: forall a b. (Typeable a, Typeable b) =>
        Expr (a -> b) -> Expr a -> Expr b
    EBop :: forall a b c. (Typeable a, Typeable b, Typeable c) =>
        BOperator a b c -> Expr (a -> b -> c)
    EUop :: forall a b. (Typeable a, Typeable b) =>
        UOperator a b -> Expr (a -> b)

deriving instance Show (Expr a)

data AExpr = forall a. Typeable a => Expr a ::: SHPType a

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
    Input :: forall a. Var a -> SHP
    Choice :: Expr Double -> SHP -> SHP -> SHP
    Composition :: SHP -> SHP -> SHP
    While :: Expr Bool -> SHP -> SHP
    Loop :: SHP -> SHP
    Abort :: SHP
    Skip :: SHP
    Cond :: Expr Bool -> SHP -> SHP -> SHP
    -- Can store SDEs as a single term to ensure the two lists refer to the same variables
    SDE :: [Diff] -> [Diff] -> Expr Bool -> SHP

deriving instance Show SHP 

-- Our type universe is given by SHPTypes
type Env = Map String ASHPType

{--------------
-- Execution --
---------------}

data Config m = Config
    { maxTime :: Double
    , dt :: Double
    , gen :: Gen (PrimState m)
    , types :: Env
    , enums :: Set String
    }

data State = State
    { _store :: Store
    , _time :: Double
    } 
    deriving Show

makeLenses ''State

-- Options for w are Endo [Vector Double] to store the whole trace,
-- Max Double to record the highest value of a function, resp. Min Double,
type Execution m w a = (PrimMonad m, Monoid w) =>
    RWST (Config m) w State m a
