module Euler_Maruyama where

import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ( Vector, fromList )
import Lens.Micro.Platform

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST
import Control.Monad.RWS

import System.Random.MWC (Gen, GenIO, GenST)
import System.Random.MWC.Distributions (normal) 

import Types
import Tracing

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
        y' = V.zipWith (+) yz (V.map (*h) (f yz tz))
        t' = tz + h

-- Step of length dt for a k-dimensional Wiener process
-- - Vector of normally distributed values with variance dt
dWiener :: PrimMonad m
          => Double            -- step size
          -> Int               -- length of vector
          -> Gen (PrimState m) -- random seed
          -> m (Vector Double)
dWiener dt k gen = V.generateM k (\_ -> normal 0 (sqrt dt) gen)

{-# INLINE dWiener #-}
{-# SPECIALISE dWiener :: Double -> Int -> GenIO -> IO (Vector Double) #-}
{-# SPECIALISE dWiener :: Double -> Int -> GenST s -> ST s (Vector Double) #-}

-- eulerMaruyamaStep :: PrimMonad m
--                   => Flow -- Vector Double -> Double -> Vector Double
--                   -> Noise -- Vector Double -> Double -> Vector (Vector Double)
--                   -> Vector Double -- current state
--                   -> Double -- current time
--                   -> Double -- timestep 
--                   -> Gen (PrimState m) -- random number generator
--                   -> m (Vector Double)
-- eulerMaruyamaStep flow noise state t dt gen = do
--       w_n <- dWiener dt (length state) gen
--       return $ state ^+^ ((dt *^ flow state t) ^+^ (noise state t !* w_n))
-- {-# INLINE eulerMaruyamaStep #-}

eulerMaruyamaStepDiag :: PrimMonad m
                  => Flow -- Vector Double -> Double -> Vector Double
                  -> Flow -- Vector Double -> Double -> Vector (Vector Double)
                  -> Vector Double -- current state
                  -> Double -- current time
                  -> Double -- timestep 
                  -> Gen (PrimState m) -- random number generator
                  -> m (Vector Double)
eulerMaruyamaStepDiag flow noise state t dt gen = do
    w_n <- dWiener dt (V.length state) gen
    return $ V.zipWith (+) state (V.zipWith (+) (V.map (*dt) (flow state t)) (V.zipWith (*) (noise state t) w_n))
{-# INLINE eulerMaruyamaStepDiag #-}

-- eulerMaruyamaTrace :: (PrimMonad m, Monoid w)
--               => Tracing.Trace m w
--               -> Flow
--               -> Noise
--               -- boundary - execution stops when this evaluates to false
--               -> (Double -> Vector Double -> Bool)
--               -> Vector Double -- current state
--               -> Execution m w (Vector Double)
-- eulerMaruyamaTrace traceF flow noise boundary state =
--     do
--         t <- use time
--         Config {maxTime=maxTime, dt=dt, gen=gen} <- ask
--         if maxTime <= t || not (boundary t state)
--            then return state
--            else do
--               nextState <- eulerMaruyamaStep flow noise state t dt gen
--               traceF nextState
--               time %= (dt+)
--               eulerMaruyamaTrace traceF flow noise boundary nextState
-- 
-- {-# INLINE eulerMaruyamaTrace #-}

eulerMaruyamaTraceDiag :: (PrimMonad m, Monoid w)
              => Tracing.Trace m w
              -> Flow
              -> Flow
              -- boundary - execution stops when this evaluates to false
              -> (Double -> Vector Double -> Bool)
              -> Vector Double -- current state
              -> Execution m w (Vector Double)
eulerMaruyamaTraceDiag traceF flow noise boundary state =
    do
        t <- use time
        Config {maxTime=maxTime, dt=dt, gen=gen} <- ask
        if maxTime <= t || not (boundary t state)
           then return state
           else do
              nextState <- eulerMaruyamaStepDiag flow noise state t dt gen
              traceF nextState
              time %= (dt+)
              eulerMaruyamaTraceDiag traceF flow noise boundary nextState
{-# INLINE eulerMaruyamaTraceDiag #-}
