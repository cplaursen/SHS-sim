{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module CLI where

import Data.Vector.Unboxed (Vector, singleton, (!))
import Data.Vector.Unboxed qualified as U

import System.IO (hPutStrLn, stderr)
import System.Random.MWC (createSystemRandom, uniformM, withSystemRandom)

import Blocks
import Execution
import Expression
import Lexer
import Parser
import Tracing
import Types

import Control.Monad
import Control.Monad.RWS (RWST (runRWST), replicateM_)
import Control.Monad.Writer.Strict (lift, runWriter)
import Data.Bifunctor (second)
import Data.Map (Map, empty, fromList, insert)
import Data.Set (Set)

import Data.List (intercalate)
import Options.Applicative hiding (empty)

data Input = FileIn FilePath | StrIn String | StdIn
data Output = FileOut FilePath | StdOut
data Mode = Parse | Typecheck | Simulate | Benchmark
data CLITracing
  = CLIRawTrace
  | CLIFullTrace String
  | CLIMinTrace String
  | CLIMaxTrace String
  | CLIAnyTrace String
  | CLIAllTrace String
  | CLINoTrace

parseCLITracing :: CLITracing -> Env -> Set String -> Either String SomeTracingMode
parseCLITracing CLIRawTrace _ _ = Right $ SomeTracingMode RawTrace
parseCLITracing CLINoTrace _ _ = Right $ SomeTracingMode NoTrace
parseCLITracing (CLIFullTrace expr) env enums = do
  expr_typed ::: SHPReal <- readAExpr env enums expr
  return $ SomeTracingMode (FullTrace expr_typed)
parseCLITracing (CLIMinTrace expr) env enums = do
  expr_typed ::: SHPReal <- readAExpr env enums expr
  return $ SomeTracingMode (MinTrace expr_typed)
parseCLITracing (CLIMaxTrace expr) env enums = do
  expr_typed ::: SHPReal <- readAExpr env enums expr
  return $ SomeTracingMode (MaxTrace expr_typed)
parseCLITracing (CLIAnyTrace expr) env enums = do
  expr_typed ::: SHPBool <- readAExpr env enums expr
  return $ SomeTracingMode (AnyTrace expr_typed)
parseCLITracing (CLIAllTrace expr) env enums = do
  expr_typed ::: SHPBool <- readAExpr env enums expr
  return $ SomeTracingMode (AllTrace expr_typed)

-- Parameters that can be extracted from the command line
data CLIOpt = CLIOpt
  { program :: Input
  , output :: Output
  , mode :: Mode
  , tracing :: CLITracing
  , getTime :: Double
  , timestep :: Double
  , nruns :: Int
  }

cliopt :: Parser CLIOpt
cliopt =
  CLIOpt
    <$> (fileInput <|> strInput <|> stdInput)
    <*> (fileOutput <|> pure StdOut)
    <*> ( flag' Parse (long "parse")
            <|> flag' Typecheck (long "typecheck")
            <|> flag' Simulate (long "simulate")
            <|> flag' Benchmark (long "benchmark")
            <|> pure Simulate
        )
    <*> traceMode
    <*> time
    <*> timestep
    <*> nRuns
 where
  fileInput = FileIn <$> argument str (metavar "FILENAME")
  fileOutput =
    FileOut
      <$> strOption
        ( short 'o'
            <> metavar "FILENAME"
            <> help "Output file"
        )
  strInput =
    StrIn
      <$> strOption
        ( long "prog"
            <> metavar "PROGRAM"
            <> help "Program to run"
        )
  stdInput =
    flag'
      StdIn
      ( long "stdin"
          <> help "Read from stdin"
      )
  traceMode =
    flag' CLIRawTrace (long "rawtrace")
      <|> flag' CLINoTrace (long "notrace")
      <|> (CLIFullTrace <$> strOption (long "fulltrace"))
      <|> (CLIMinTrace <$> strOption (long "mintrace"))
      <|> (CLIMaxTrace <$> strOption (long "maxtrace"))
      <|> pure CLIRawTrace
  time =
    option
      auto
      ( short 't'
          <> help "Maximum time to run simulation"
          <> showDefault
          <> value 200
          <> metavar "FLOAT"
      )
  timestep =
    option
      auto
      ( long "timestep"
          <> help "Timestep for Euler-Maruyama simualtion"
          <> showDefault
          <> value 0.01
          <> metavar "FLOAT"
      )
  nRuns =
    option
      auto
      ( short 'n'
          <> help "Amount of times to run the SHP"
          <> showDefault
          <> value 1
          <> metavar "INT"
      )

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
    Benchmark -> do
      g <- createSystemRandom
      a <- U.replicateM 10000000 $ uniformM g :: IO (Vector Int)
      print $ U.foldl' (+) 0 a
      return ()
    Simulate ->
      let v = do
            parsed <- parseSHPBlocks prog
            let env = blocksEnv parsed
            let enums = blocksEnums parsed
            typechecked <- typecheckBlocks parsed
            traceF <- parseCLITracing (tracing opt) env enums
            return (env, enums, typechecked, traceF)
       in case v of
            Left err -> hPutStrLn stderr err
            Right (env, enums, typechecked, traceF) -> replicateM_ (nruns opt) (runProgram >>= outputF)
             where
              runProgram = case traceF of
                SomeTracingMode typedTraceF -> do
                  seed <- createSystemRandom
                  (_, _, trace) <-
                    runRWST
                      (runSHP inputUser typechecked)
                      (Config (getTime opt) (timestep opt) seed env enums typedTraceF)
                      (State Data.Map.empty 0)
                  return $ extractTrace typedTraceF trace

runCLI :: IO ()
runCLI = execParser opts >>= runProgram
 where
  opts =
    info
      (cliopt <**> helper)
      ( fullDesc
          <> progDesc "Simulate supplied SHP"
          <> header "SHS-sim - a simulator for stochastic hybrid programs"
      )
