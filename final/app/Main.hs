module Main (main) where

import Parser (qcparser, lexer)
import Common (showState)
import PrettyPrint (prettyPrint)
import Eval (eval)
import System.Console.Haskeline (runInputT, defaultSettings, getInputLine, outputStrLn)
import Control.Monad.IO.Class (MonadIO(liftIO))

printAST s = outputStrLn $ prettyPrint $ qcparser $ lexer s

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = do
      input <- getInputLine "λ "
      case input of
        Nothing -> return ()
        Just s -> do
          if length (words s) == 1
            then printAST s
            else let [command, file] = take 2 $ words s
                  in case command of
                    ":f" -> do
                      sfile <- liftIO $ readFile file
                      printAST sfile
                    _ -> outputStrLn "Unknown command"
          loop