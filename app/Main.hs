module Main where

import Euler_Maruyama
import Data.Vector (Vector, singleton, (!))
import qualified Data.Vector as V
import Linear.Vector
import System.Random.MWC (createSystemRandom, GenIO)
import System.Random.MWC.Distributions

import Graphics.Matplotlib

import StochasticHybrid
import SHPParser
import SHPLexer
import Data.HashMap.Strict (empty)
import Control.Monad.RWS (runRWST)
import Control.Monad.Writer.Strict (runWriter, lift)

const_drift :: Double -> Flow
const_drift r = (\_ _ -> singleton r)

const_noise :: Double -> Noise
const_noise sigma = (\_ _ -> singleton (singleton sigma))

{-

euler_run :: IO [(Double, Double)]
euler_run = do
    gen <- create
    x <- standard gen
    let e <- runWriter $ lift $ euler_maruyama (const_drift 1) (const_noise 0.1) (singleton x) 0 (\_ t -> t < 10) (Config 0.001 10 gen)
    return ((\(x,y) -> (x, (y!0))) <$> e)

bunch_of_normals :: GenIO -> Int -> IO [Double]
bunch_of_normals gen k
  | k == 0 = return []
  | otherwise = do
     v <- standard gen
     (v:) <$> (bunch_of_normals gen (k-1))

plot_sde :: IO ()
plot_sde = do
    v <- euler_run
    print v
    let (x,y) = Prelude.unzip v
    print =<< (file "a.png" $ plot x y)
-}

interpret :: IO ()
interpret = do
    c <- getContents
    g <- createSystemRandom
    let prog = parseSHP $ alexScanTokens c
    print =<< runRWST (runSHP prog) (Config 0 0.01 g) (State empty V.empty empty)

{-
parse_print :: IO ()
parse_print = do
    c <- getContents
    print $ parseSHP $ alexScanTokens c

lex_print :: IO ()
lex_print = do
    c <- getContents
    print $ alexScanTokens c
-}

main = interpret
