module Main (main) where

import           Control.Monad            (forM_)
import           Control.Monad.Except     (runExcept)
import           Control.Monad.Trans      (MonadTrans (lift))
import           Data.Maybe               (listToMaybe)
import           Eval                     (eval)
import           Matrix                   (Matrix (cols))
import           Parser                   (E (..), lexer, qcparser)
import           PrettyPrint              (pp, ppState)
import           State                    (Operator)
import           System.Console.Haskeline (InputT, defaultSettings,
                                           getInputLine, outputStrLn, runInputT)
import           Typecheck                (tc)
import           QBit                     (colMatrixFromNumber)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- parses and prints the given string
printAST :: String -> InputT IO ()
printAST s =
  case qcparser $ lexer s of
    Failed err -> outputStrLn err
    Ok ast     -> outputStrLn $ pp ast

-- parses and evaluates the given string
parseAndEval :: String -> InputT IO ()
parseAndEval s =
  case qcparser $ lexer s of
    Failed err -> outputStrLn err
    Ok ast -> do
      let tcd = runExcept $ tc ast
      either outputStrLn (\tast -> do
        let op = runExcept $ eval tast
        either outputStrLn loopStateOrQuit op) tcd

loopStateOrQuit :: Operator -> InputT IO ()
loopStateOrQuit op = do
  input <- getInputLine "(integer or :q) λ "
  forM_ input stateOrQuit
    where
      stateOrQuit ":q" = return ()
      stateOrQuit input = case maybeRead input :: Maybe Int of
        Nothing -> outputStrLn "Expected an integer or :q" >> loopStateOrQuit op
        Just n  -> (outputStrLn . ppState) (op <> initstate n) >> loopStateOrQuit op

      initstate n = colMatrixFromNumber n $ opdimension

      opdimension = ceiling (logBase 2 $ fromIntegral $ cols op :: Double) :: Int

-- main function
main :: IO ()
main = runInputT defaultSettings (intro >> loop)

-- prints the intro message of the app
intro :: InputT IO ()
intro = outputStrLn $
  "Welcome to the Quantum Circuit Language Interpreter!\n" ++
  "CLI is not yet supported, but you can execute files and pretty print them.\n" ++
  "Type :f <file> to execute a file\n" ++
  "Type :p <file> to pretty print the program\n" ++
  "Type :q to quit\n"

-- main loop of the app
loop :: InputT IO ()
loop = do
  input <- getInputLine "λ "
  forM_ input executeCommand

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

fileOrDeath :: [String] -> (String -> InputT IO ()) -> InputT IO ()
fileOrDeath [] _ = outputStrLn "No file given"
fileOrDeath (f : _) m = lift (readFile f) >>= m
