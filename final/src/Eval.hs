module Eval(eval) where

import           AST                  (AngleExpr (..))
import           Control.Monad.Except (Except)
import           Data.Bits            (testBit, (.&.))
import           Data.Complex         (Complex (..))
import           Matrix               (ColMatrix (..), Matrix (..),
                                       RowMatrix (..), fromRowToCol)
import           QBit                 (QBitBase (..), toColMatrix, colMatrixFromNumber)
import           State                (Operator, castFromInt,
                                       linearTransformation, tensoreye)
import           Typecheck            (TQC (..))

eval :: TQC -> Except String Operator
eval (TQCSkip _) = return $ castFromInt $ tensoreye 1

eval (TQCRx a _) = return $ fromRowToCol $ RowMatrix 2 2 [
    cos (f / 2) :+ 0, 0 :+ (-sin (f / 2)),
    0 :+ (-sin (f / 2)), cos (f / 2) :+ 0]
    where
      f = evalAngle a

eval (TQCRy a _) = return $ fromRowToCol $ RowMatrix 2 2 [
    cos (f / 2) :+ 0, (-sin (f / 2)) :+ 0,
    sin (f / 2) :+ 0,   cos (f / 2)  :+ 0]
    where
      f = evalAngle a

eval (TQCRz a _) = return $ fromRowToCol $ RowMatrix 2 2 [
    exp (0 :+ (-f / 2)),             0 :+ 0,
                 0 :+ 0, exp (0 :+ (f / 2))]
    where
      f = evalAngle a

eval (TQCSwap _) = return swap

eval (TQCSeq a b _) = do
  op1 <- eval a
  op2 <- eval b
  return $ op2 <> op1

eval (TQCPar a b _) = do
  op1 <- eval a
  op2 <- eval b
  return $ op1 `tensor` op2

eval (TQCControl a n) = do
  op <- eval a

  let one = colMatrixFromNumber 1 1
  let states = 2 ^ (n - 1)
  let opcol base = ColMatrix states 1 $ col (val base .&. (states - 1)) op

  let f base | testBit base (n - 1) = one `tensor` opcol base
             | otherwise = toColMatrix base

  return $ linearTransformation n f

swap :: Operator
swap = linearTransformation 2 $ s . val
  where
    s 0 = toColMatrix $ QBitBase 0 2
    s 1 = toColMatrix $ QBitBase 2 2
    s 2 = toColMatrix $ QBitBase 1 2
    s 3 = toColMatrix $ QBitBase 3 2
    s _ = error "out of bounds for 2 qbits"

evalAngle :: AngleExpr -> Double
evalAngle (AngleConst a) = a
evalAngle AnglePi = pi
evalAngle (AngleNeg a) = - evalAngle a
evalAngle (AngleAdd a b) = evalAngle a + evalAngle b
evalAngle (AngleSub a b) = evalAngle a - evalAngle b
evalAngle (AngleMul a b) = evalAngle a * evalAngle b
evalAngle (AngleDiv a b) = evalAngle a / evalAngle b
evalAngle (AngleParen a) = evalAngle a