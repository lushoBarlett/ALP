module Main (main) where

import Parser (qcparser, lexer)
import Common (showState, State)
import PrettyPrint (prettyPrint)
import Eval (eval, EvalT(..), defaultRunEnv)
import System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine, outputStrLn)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans (MonadTrans(lift))

printAST :: String -> IO ()
printAST = print . prettyPrint . qcparser . lexer

parseAndEval :: String -> EvalT State
parseAndEval = eval . qcparser . lexer

main :: IO ()
main = runInputT defaultSettings loop

loop :: InputT IO ()
loop = do
  input <- getInputLine "λ "
  case input of
    Nothing -> return ()
    Just s -> do
      if length (words s) == 1
        then lift $ return (showState <$> parseAndEval s)
        else executeCommand s
      loop

executeCommand :: String -> InputT IO (EvalT String)
executeCommand s =
  let [command, file] = take 2 $ words s
  in case command of
    ":f" -> lift $ readFile file >>= \s -> print $ showState <$> parseAndEval s
    ":p" -> lift $ readFile file >>= printAST
    _ -> outputStrLn "Unknown command"