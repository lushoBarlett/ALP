module AST where

data QC
  = QCDeclaration String QC
  | QCArrow QC QC
  | QCTensor QC QC
  | QCVariable String
  | QCIdentity