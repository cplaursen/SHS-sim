module Main where

import Euler_Maruyama
import Data.Vector (Vector, singleton, (!))
import qualified Data.Vector as V
import Linear.Vector
import System.Random.MWC (createSystemRandom, withSystemRandomST)
import System.Random.MWC.Distributions

-- import Graphics.Matplotlib

import StochasticHybrid
import SHPParser
import SHPLexer
import Types
import Data.HashMap.Strict (empty)
import qualified Data.HashMap.Strict as M
import Control.Monad.RWS (runRWST)
import Control.Monad.Writer.Strict (runWriter, lift)
import Control.Monad.ST

import Options.Applicative
import Data.Semigroup ((<>))

data Input = FileIn FilePath | StrIn String | StdIn
data Mode = Parse | Simulate 

data CLIOpt = CLIOpt
    { program :: Input
    , mode :: Mode
    }

cliopt :: Parser CLIOpt
cliopt = CLIOpt <$> (fileInput <|> strInput <|> stdInput) <*> (flag' Parse (long "parse") <|> flag' Simulate (long "simulate") <|> pure Parse)
    where
        fileInput = FileIn <$> strOption
            (  long "file"
            <> short 'f'
            <> metavar "FILENAME"
            <> help "Input file" )
        strInput = StrIn <$> argument str (metavar "PROG")
        stdInput = flag' StdIn 
            (  long "stdin"
            <> help "Read from stdin" )



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

interpret text = do
    let prog = parseSHPProg $ alexScanTokens text
    a <- withSystemRandomST (\g -> runRWST (runSHP (evalBlocks prog)) (Config 200000 0.01 g (M.fromList [("t", 0)])) (State Data.HashMap.Strict.empty (singleton 0) 0))
    return a

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

runProgram :: CLIOpt -> IO ()
runProgram opt = do
    prog <- case program opt of
              FileIn p -> readFile p
              StrIn s -> return s
              StdIn -> getContents
    case mode opt of
      Parse -> print $ evalBlocks $ parseSHPProg $ alexScanTokens prog
      Simulate -> interpret prog >>= print

main :: IO ()
main = runProgram =<< execParser opts
    where
        opts = info (cliopt <**> helper)
            (  fullDesc 
            <> progDesc "Simulate supplied SHP" 
            <> header "SHS-sim - a simulator for stochastic hybrid programs" )
