{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Eval(eval, EvalT(..), defaultRunEnv, defaultState, run) where

import Common (QC(..), State(..), Environment(..), Matrix(..), tensorQBit, qbitFromNumber, tensor, eye, Operator, Name)
import Control.Monad.State (MonadState(..), StateT(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.Except (MonadError(..), ExceptT(..), runExceptT)
import Data.Complex (Complex(..))
import qualified Data.Map as Map
import Data.List (elemIndex)

newtype EvalT a = EvalT {
  runEvalT :: ReaderT Environment (StateT State (ExceptT String IO)) a
} deriving (Functor, Applicative, Monad, MonadReader Environment, MonadState State, MonadError String)

run :: EvalT a -> Environment -> State -> IO (Either String (a, State))
run evalM initEnv initState =
  let evaluator = runEvalT evalM
      withInitialEnv = runReaderT evaluator initEnv
      withInitialState = runStateT withInitialEnv initState
  in
    runExceptT withInitialState

defaultRunEnv :: Environment
defaultRunEnv = Environment {
  circuits = Map.empty,
  gates = Map.fromList [
    ("I", QCI),
    ("X", QCX),
    ("Y", QCY),
    ("Z", QCZ),
    ("H", QCH)
  ]
}

defaultState :: State
defaultState = State {
  qbits = Matrix 1 1 [1],
  qbitnames = []
}

updateState :: (State -> State) -> EvalT ()
updateState f = do
  s <- get
  put $ f s

expand :: [Name] -> [Operator] -> EvalT Operator
expand names partialOps = do
  s <- get

  let allnames = qbitnames s

  let nameInPartialOps name = elemIndex name names

  let decideOperator name = case nameInPartialOps name of
        Nothing -> eye 2
        Just i -> partialOps !! i

  let fillMissing = [decideOperator name | name <- allnames]

  return $ foldr1 tensor fillMissing

eval :: QC -> EvalT ()
eval (QCCircuit _ preps body) = mapM_ eval preps >> evalSeq body
eval (QCPreparation n name) = updateState $ \s -> tensorQBit s name $ qbitFromNumber n
eval _ = undefined

evalSeq :: [QC] -> EvalT ()
evalSeq [] = return ()
evalSeq ((QCOperation names qc):qcs) = do
  op <- evalOperator names qc
  updateState $ \s -> s { qbits = qbits s <> op }
  evalSeq qcs
evalSeq (gate@(QCGate name _ _):qcs) =
  local (\env -> env { gates = Map.insert name gate (gates env) }) $ evalSeq qcs
evalSeq (qc:_) = error $ "Not implemented: evalSeq for " ++ show qc

evalOperator :: [Name] -> QC -> EvalT Operator
evalOperator names (QCArrow qc1 qc2) = do
  op1 <- evalOperator names qc1
  op2 <- evalOperator names qc2
  return $ op1 <> op2

evalOperator names (QCTensors tensors) = do
  partialOps <- evalTensors names tensors
  expand names partialOps

evalOperator names (QCVariable name) = do
  env <- ask
  case Map.lookup name (gates env) of
    Nothing -> throwError $ "Variable " ++ name ++ " not found (in global scope, local is not implemented)"
    Just qc -> evalOperator names qc

evalOperator _ (QCOperation names qc) = evalOperator names qc
evalOperator _ QCI = return $ Matrix 2 2 [1, 0, 0, 1]
evalOperator _ QCX = return $ Matrix 2 2 [0, 1, 1, 0]
evalOperator _ QCY = return $ Matrix 2 2 [0, 0 :+ (-1), 0 :+ 1, 0]
evalOperator _ QCZ = return $ Matrix 2 2 [1, 0, 0, -1]
evalOperator _ QCH = return $ Matrix 2 2 [1 / sqrt 2, 1 / sqrt 2, 1 / sqrt 2, -1 / sqrt 2]
evalOperator _ (QCGate _ args body) = compileOperator args body
evalOperator _ qc = error $ "Not implemented: evalOperator for " ++ show qc

compileOperator :: [Name] -> [QC] -> EvalT Operator
compileOperator args body = foldl1 (<>) <$> sequenceA (evalOperator args <$> body)

evalTensors :: [Name] -> [QC] -> EvalT [Operator]
evalTensors _ [] = throwError "Qbit/Operator mismatch"
evalTensors names (t:tensors) = do
  let n = arguments t
  if n > length names
    then throwError "Qbit/Operator mismatch"
    else do
      op <- evalOperator (take n names) t
      ops <- evalTensors (drop n names) tensors
      return $ op : ops

arguments :: QC -> Int
arguments (QCGate _ names _) = length names
arguments QCI = 1
arguments QCX = 1
arguments QCY = 1
arguments QCZ = 1
arguments QCH = 1
arguments qc = error $ "Not implemented: arguments for " ++ show qc