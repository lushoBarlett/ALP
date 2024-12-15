{-# LANGUAGE FlexibleContexts #-}

module Typecheck(tc, TQC(..)) where

import           AST                  (QC (..), AngleExpr (..))
import           Control.Monad.Except (throwError, Except)
import           PrettyPrint (pp)

-- The type of a quantum circuit is a natural number
-- representing the number of qubits it acts on
data TQC
  = TQCSkip Int
  | TQCRx AngleExpr Int
  | TQCRy AngleExpr Int
  | TQCRz AngleExpr Int
  | TQCSwap Int
  | TQCSeq TQC TQC Int
  | TQCPar TQC TQC Int
  | TQCControl TQC Int
  deriving Show

tc :: QC -> Except String TQC
tc QCSkip = return $ TQCSkip 1

tc (QCRx a) = return $ TQCRx a 1
tc (QCRy a) = return $ TQCRy a 1
tc (QCRz a) = return $ TQCRz a 1

tc QCSwap = return $ TQCSwap 2

tc s@(QCSeq a b) = do
  atd <- tc a
  btd <- tc b
  if getType atd == getType btd
    then return $ TQCSeq atd btd $ getType atd
    else throwError $ "Type mismatch in QCSeq:\n" ++ pp s ++ "\n" ++ "qbit counts: " ++ show (getType atd) ++ " and " ++ show (getType btd)

tc (QCPar a b) = do
  atd <- tc a
  btd <- tc b
  return $ TQCPar atd btd $ getType atd + getType btd

tc (QCControl a) = do
  atd <- tc a
  return $ TQCControl atd $ getType atd + 1

getType :: TQC -> Int
getType (TQCSkip n) = n
getType (TQCRx _ n) = n
getType (TQCRy _ n) = n
getType (TQCRz _ n) = n
getType (TQCSwap n) = n
getType (TQCSeq _ _ n) = n
getType (TQCPar _ _ n) = n
getType (TQCControl _ n) = n