{-# LANGUAGE NamedFieldPuns #-}
module Euler_Maruyama where

import qualified Data.Vector as V
import Data.Vector (Vector, fromList)
import Linear.Vector
import Linear.Matrix

import Control.Monad (replicateM)
import Control.Monad.Writer.Strict (WriterT, tell, lift)
import Control.Monad.Primitive (PrimMonad, PrimState)

import System.Random.MWC (Gen, GenIO)
import System.Random.MWC.Distributions (normal)

type Flow = Vector Double -> Double -> Vector Double 
type Noise = Vector Double -> Double -> Vector (Vector Double)

data Config = Config
    { maxTime :: Double
    , dt :: Double
    , gen :: GenIO
    }
euler :: Flow -- ODE
      -> Double -- Target time
      -> Double -- Step size
      -> Vector Double -- Initial value
      -> Double -- Initial time
      -> Vector Double
euler f t h yz tz
  | t <= tz = yz
  | otherwise = euler f t h y' t'
    where 
        y' = V.zipWith (+) yz ((*h) <$> f yz tz)
        t' = tz + h

-- Step of length dt for a k-dimensional Wiener process
dWiener_n :: PrimMonad m
          => Double            -- step size
          -> Int               -- length of vector
          -> Gen (PrimState m) -- random seed
          -> m (Vector Double)
dWiener_n dt k gen = fromList <$> replicateM k (normal 0 (sqrt dt) gen)

euler_maruyama :: Flow
               -> Noise
               -> Vector Double -- current state
               -> Double -- current time
               -> (Vector Double -> Double -> Bool) -- Boundary
               -> Config
               -> WriterT [(Double, Vector Double)] IO (Double, Vector Double)
euler_maruyama f sigma y t boundary cfg@Config{maxTime, dt, gen}
  | t >= maxTime || not (boundary y t) = return (t,y)
  | otherwise = do
      w_n <- lift $ dWiener_n dt (length y) gen
      let curr_step = y ^+^ ((dt *^ f y t) ^+^ (sigma y t !* w_n))
      tell [(t + dt, curr_step)]
      euler_maruyama f sigma curr_step (t + dt) boundary cfg
