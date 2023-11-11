module Main (main) where

import Parser (qcparser, lexer)
import Common (showState)
import PrettyPrint (prettyPrint)
import Eval (eval, EvalT(..), defaultRunEnv)
import System.Console.Haskeline (runInputT, defaultSettings, getInputLine, outputStrLn)
import Control.Monad.IO.Class (MonadIO(liftIO))

printAST s = outputStrLn $ prettyPrint $ qcparser $ lexer s

evalIO s = do
  let result = eval $ qcparser $ lexer s
  case result of
    Left err -> outputStrLn err
    Right state -> outputStrLn $ showState state

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = do
      input <- getInputLine "λ "
      case input of
        Nothing -> return ()
        Just s -> do
          if length (words s) == 1
            then evalIO s
            else let [command, file] = take 2 $ words s
                  in case command of
                    ":f" -> do
                      sfile <- liftIO $ readFile file
                      evalIO sfile
                    ":p" -> do
                      sfile <- liftIO $ readFile file
                      printAST sfile
          loop