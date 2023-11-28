module CLI where

import Euler_Maruyama
import Data.Vector (Vector, singleton, (!))
import qualified Data.Vector as V
import Linear.Vector
import System.Random.MWC (createSystemRandom, withSystemRandomST)
import System.Random.MWC.Distributions

import StochasticHybrid
import Parser
import Lexer
import Types
import AST_Operations
import Typecheck
import SHPTypes
import Data.Map ( fromList, empty, insert )
import Data.Bifunctor ( second )
import Control.Monad.RWS ( runRWST, appEndo )
import Control.Monad.Writer.Strict (runWriter, lift)
import Control.Monad.ST

import Options.Applicative hiding ( empty )
import Lens.Micro.Platform
import Execution

data Input = FileIn FilePath | StrIn String | StdIn
data Mode = Parse | Typecheck | Simulate 

data CLIOpt = CLIOpt
    { program :: Input
    , mode :: Mode
    }

cliopt :: Parser CLIOpt
cliopt = CLIOpt <$>
    (fileInput <|> strInput <|> stdInput)
     <*> (   flag' Parse (long "parse")
         <|> flag' Typecheck (long "typecheck")
         <|> flag' Simulate (long "simulate")
         <|> pure Parse
         )
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

runProgram :: CLIOpt -> IO ()
runProgram opt = do
    prog <- case program opt of
              FileIn p -> readFile p
              StrIn s -> return s
              StdIn -> getContents
    case mode opt of
      Parse -> print $ runAlex prog parseSHPProg
      Typecheck -> print $ parseAndTypecheck prog
      Simulate -> do
          let typechecked = parseAndTypecheck prog
          case typechecked of
            Left err -> print err
            Right prog -> do
                (_, state, writer) <- withSystemRandomST (\g -> runRWST (runSHP prog) (Config 200 0.01 g) (State Data.Map.empty 0))
                print (state, appEndo writer [])


runCLI :: IO ()
runCLI = execParser opts >>= runProgram
    where
        opts = info (cliopt <**> helper)
            (  fullDesc 
            <> progDesc "Simulate supplied SHP" 
            <> header "SHS-sim - a simulator for stochastic hybrid programs" )
