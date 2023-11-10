module Common (
  QC(..),
  State,
  Environment(..),
  showState,
) where
import Data.Complex (Complex)
import Data.Map.Strict (Map)

type Name = String

data QC
  = QCCircuit Name [QC] [QC]
  | QCPreparation Int Name
  | QCGate Name [Name] [QC]
  | QCArrow QC QC
  | QCTensor QC QC
  | QCVariable Name
  | QCIdentity
  deriving Show

type State = Map Name (Complex Double)

showState :: State -> String
showState = foldMap (\value -> show value ++ "\n")

data Environment = Environment {
  getState :: State,
  getOperators :: Map Name (State -> State)
}