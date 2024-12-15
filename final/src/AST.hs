module AST(QC(..), AngleExpr(..)) where

data QC
  = QCSkip
  | QCRx AngleExpr
  | QCRy AngleExpr
  | QCRz AngleExpr
  | QCSwap
  | QCSeq QC QC
  | QCPar QC QC
  | QCControl QC
  deriving Show

data AngleExpr
  = AngleConst Double
  | AnglePi
  | AngleNeg AngleExpr
  | AngleAdd AngleExpr AngleExpr
  | AngleSub AngleExpr AngleExpr
  | AngleMul AngleExpr AngleExpr
  | AngleDiv AngleExpr AngleExpr
  | AngleParen AngleExpr
  deriving Show