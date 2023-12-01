{-# LANGUAGE NoMonomorphismRestriction #-}

module CLI where

import Euler_Maruyama
import Data.Vector (Vector, singleton, (!))
import qualified Data.Vector as V
import Linear.Vector
import System.Random.MWC (createSystemRandom, withSystemRandom)
import System.Random.MWC.Distributions
import System.IO ( hPutStrLn, stderr )

import StochasticHybrid
import Parser
import Lexer
import Types
import Data.Map ( fromList, empty, insert )
import Data.Bifunctor ( second )
import Control.Monad.RWS ( runRWST, appEndo )
import Control.Monad.Writer.Strict (runWriter, lift)
import Control.Monad.ST

import Options.Applicative hiding ( empty )
import Lens.Micro.Platform
import Execution

data Input = FileIn FilePath | StrIn String | StdIn
data Output = FileOut FilePath | StdOut
data Mode = Parse | Typecheck | Simulate 

data CLIOpt = CLIOpt
    { program :: Input
    , output :: Output
    , mode :: Mode
    , nruns :: Int
    }

cliopt :: Parser CLIOpt
cliopt = CLIOpt <$>
    (fileInput <|> strInput <|> stdInput)
     <*> ( fileOutput <|> pure StdOut )
     <*> (   flag' Parse (long "parse")
         <|> flag' Typecheck (long "typecheck")
         <|> flag' Simulate (long "simulate")
         <|> pure Simulate
         )
    <*> nRuns
    where
        fileInput = FileIn <$> argument str (metavar "FILENAME")
        fileOutput = FileOut <$> strOption
            (  short 'o'
            <> metavar "FILENAME"
            <> help "Output file")
        strInput = StrIn <$> strOption
            (  long "prog"
            <> metavar "PROGRAM"
            <> help "Program to run" )
        stdInput = flag' StdIn 
            (  long "stdin"
            <> help "Read from stdin" )
        nRuns = option auto
            ( short 'n'
           <> help "Amount of times to run the SHP"
           <> showDefault
           <> value 1
           <> metavar "INT" )


runProgram :: CLIOpt -> IO ()
runProgram opt = do
    prog <- case program opt of
              FileIn p -> readFile p
              StrIn s -> return s
              StdIn -> getContents
    let outputF = case output opt of
                    FileOut path -> writeFile path
                    StdOut -> putStrLn
    case mode opt of
      Parse -> outputF . show $ runAlex prog parseSHPProg
      Typecheck -> outputF . show $ parseAndTypecheck prog
      Simulate ->
          let v = do
                parsed <- parseSHPBlocks prog
                let env = getEnv parsed
                let enums = getEnums parsed
                typechecked <- typecheckBlocks parsed
                return (env, enums, typechecked)
           in case v of
                Left err -> hPutStrLn stderr err
                Right (env, enums, typechecked) -> do
                    seed <- createSystemRandom
                    (_, _, trace) <- runRWST (runSHP typechecked) (Config 200 0.01 seed env enums) (State Data.Map.empty 0)
                    outputF $ unlines $ map (unwords . map show . V.toList) $ appEndo trace []
            
runCLI :: IO ()
runCLI = execParser opts >>= runProgram
    where
        opts = info (cliopt <**> helper)
            (  fullDesc 
            <> progDesc "Simulate supplied SHP" 
            <> header "SHS-sim - a simulator for stochastic hybrid programs" )
