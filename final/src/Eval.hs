{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Eval(eval, EvalT(..), defaultRunEnv) where

import Common (QC(..), State(..), Environment(..), Matrix(..), addCircuit, tensorQBit, qbitFromNumber, tensor, eye, Operator, Name)
import Control.Monad.State (MonadState(..), StateT(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.Except (MonadError(..), ExceptT(..))
import Data.Complex (Complex(..))
import qualified Data.Map as Map
import Data.List (elemIndex)

newtype EvalT a = EvalT {
  runEvalT :: ExceptT String (ReaderT Environment (StateT State IO)) a
} deriving (Functor, Applicative, Monad, MonadError String, MonadReader Environment, MonadState State)

defaultRunEnv :: EvalT ()
defaultRunEnv = do
  put $ State {
    qbits = Matrix 0 0 [],
    qbitnames = [],
    localenv = Environment {
      circuits = mempty,
      gates = mempty
    }
  }
  return ()

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

eval :: QC -> EvalT State
eval qc@(QCCircuit name preps body) = do
  s <- get
  put $ addCircuit name qc s
  mapM_ eval preps
  mapM_ eval body
  get

eval (QCPreparation n name) = do
  s <- get
  put $ tensorQBit s name $ qbitFromNumber n
  get

eval (QCOperation names qc) = do
  op <- evalOperation names qc
  s <- get
  let qbs = qbits s
  put $ s { qbits = qbs <> op }
  get

eval _ = undefined

evalOperation :: [Name] -> QC -> EvalT Operator
evalOperation names (QCArrow qc1 qc2) = do
  op1 <- evalOperation names qc1
  op2 <- evalOperation names qc2
  return $ op1 <> op2

evalOperation names (QCTensors tensors) = do
  partialOps <- evalTensors names tensors
  expand names partialOps

evalOperation _ (QCVariable name) = do
  env <- ask
  case Map.lookup name (gates env) of
    Nothing -> throwError $ "Variable " ++ name ++ " not found (in global scope, local is not implemented)"
    Just qc -> evalOperation [] qc

evalOperation _ QCI = return $ Matrix 2 2 [1, 0, 0, 1]
evalOperation _ QCX = return $ Matrix 2 2 [0, 1, 1, 0]
evalOperation _ QCY = return $ Matrix 2 2 [0, 0 :+ (-1), 0 :+ 1, 0]
evalOperation _ QCZ = return $ Matrix 2 2 [1, 0, 0, -1]
evalOperation _ QCH = return $ Matrix 2 2 [1 / sqrt 2, 1 / sqrt 2, 1 / sqrt 2, -1 / sqrt 2]

evalOperation _ _ = undefined

evalTensors :: [Name] -> [QC] -> EvalT [Operator]
evalTensors names = mapM (evalOperation names)