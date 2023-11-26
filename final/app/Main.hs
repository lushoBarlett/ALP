module Main (main) where

import Parser (qcparser, lexer)
import Common (showState)
import PrettyPrint (prettyPrint)
import Eval (eval, defaultRunEnv, defaultState, run)
import System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine, outputStrLn)
import Control.Monad.Trans (MonadTrans(lift))

printAST :: String -> IO ()
printAST = print . prettyPrint . qcparser . lexer

parseAndEval :: String -> IO ()
parseAndEval s = do
  let evalRes = (eval . qcparser . lexer) s
  ran <- run evalRes defaultRunEnv defaultState
  case ran of
    Left err -> print err
    Right ((), state) -> putStrLn $ showState state

main :: IO ()
main = runInputT defaultSettings loop

loop :: InputT IO ()
loop = do
  input <- getInputLine "λ (auto-execute-file-mode) "
  case input of
    Nothing -> return ()
    Just s -> do
      if length (words s) == 1
        then lift $ parseAndEval s
        else executeCommand ":f Ejemplos/example.qc" -- auto execute file for quick testing
      loop

executeCommand :: String -> InputT IO ()
executeCommand s =
  let [command, file] = take 2 $ words s
  in case command of
    ":f" -> lift $ readFile file >>= parseAndEval
    ":p" -> lift $ readFile file >>= printAST
    _ -> outputStrLn "Unknown command"