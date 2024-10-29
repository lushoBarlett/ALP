module Main (main) where

import Parser (qcparser, lexer, E(..))
import Common (showState)
import PrettyPrint (prettyPrint)
import Eval (eval, defaultRunEnv, defaultState, run)
import System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine, outputStrLn)
import Control.Monad.Trans (MonadTrans(lift))

-- parses and prints the given string
printAST :: String -> IO ()
printAST s = do
  let qc = qcparser $ lexer s
  case qc of
    Failed err -> putStrLn err
    Ok ast -> putStrLn $ prettyPrint ast

-- parses and evaluates the given string
parseAndEval :: String -> IO ()
parseAndEval s = do
  let qc = qcparser $ lexer s
  case qc of
    Failed err -> putStrLn err
    Ok ast -> do
      let evalRes = eval ast
      ran <- run evalRes defaultRunEnv defaultState
      case ran of
        Left err -> print err
        Right ((), state) -> putStrLn $ showState state

-- main function
main :: IO ()
main = runInputT defaultSettings (intro >> loop)

-- prints the intro message of the app
intro :: InputT IO ()
intro = outputStrLn $
  "Welcome to the Quantum Circuit Language Interpreter!\n" ++
  "CLI is not yet supported, but you can execute files and print their ASTs.\n" ++
  "Type :f <file> to execute a file\n" ++
  "Type :p <file> to print its AST (not quite pretty-printing)\n" ++
  "Type :q to quit\n"

-- main loop of the app
loop :: InputT IO ()
loop = do
  input <- getInputLine "λ "
  case input of
    Nothing -> return ()
    Just s -> executeCommand s

-- command interpreter
executeCommand :: String -> InputT IO ()
executeCommand s =
  case words s of
    [] -> loop
    (command : rest) -> case command of
      ":f" -> fileOrDeath rest parseAndEval >> loop
      ":p" -> fileOrDeath rest printAST >> loop
      ":q" -> return ()
      _ -> outputStrLn "Unknown command"

fileOrDeath :: [String] -> (String -> IO ()) -> InputT IO ()
fileOrDeath [] _ = outputStrLn "No file given"
fileOrDeath (f : _) m = lift $ readFile f >>= m