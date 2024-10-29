module Common (
  Name,
  Operator,
  QC(..),
  State(..),
  Environment(..),
  linearTransformation,
  tensoreye,
  addCircuit,
  addGate,
  showState,
  tensorQBit,
  castFromInt,
  castFromReal,
  enumerate,
  renumerate
) where

import Data.Complex (Complex(..))
import qualified Data.Map.Strict as Map
import Matrix (Matrix(..), ColMatrix(..))
import QBit (QBitBase(..), allBases, toColMatrix, fromCols)

-- a variable name is a string
type Name = String

-- AST
data QC
  = QCProgram [QC] [QC]
  | QCPreparation Int Name
  | QCCircuit Name [Name] [QC]
  | QCOperation [Name] QC
  | QCControl [QC] [QC]
  | QCArrow QC QC
  | QCTensors [QC]
  | QCVariable Name
  | QCNegatedVariable Name
  | QCI | QCX | QCY | QCZ | QCH
  deriving Show

-- given a function that takes a base and returns a column vector,
-- returns the matrix form asoociated with that function
linearTransformation :: Int -> (QBitBase -> ColMatrix a) -> ColMatrix a
linearTransformation n f = fromCols $ f <$> allBases n

-- returns the identity matrix of dimension 2^n
tensoreye :: Int -> ColMatrix Int
tensoreye n = linearTransformation n toColMatrix

-- a qbit is a unit vector in a 2^n-dimensional complex vector space
type QBit = ColMatrix (Complex Double)
-- an operator is of the same form, but they are square matrices
type Operator = QBit

-- the environment knows about the circuits and gates defined
-- the circuits are currently a future feature
-- the gates can only be defined in the top-level of circuits
-- and are to be defined before they are used
data Environment = Environment {
  circuits :: Map.Map Name QC,
  gates :: Map.Map Name QC
}

-- the state of the quantum computer is a 2^n-dimensional vector
-- of complex numbers, where n is the number of qubits
-- the qbitnames are the names of the qubits, in the order they were
-- defined in the circuit, and the qbitcontext are the names of the
-- qubits that are in scope at the current point in the circuit
-- e.g. if we are inside a gate, the qbitcontext will be the arguments
data State = State {
  qbits :: QBit,
  qbitnames :: [Name],
  qbitcontext :: [Name]
}

-- pretty-prints the state
showState :: State -> String
showState state = concat $ ppcomplex <$> cmAsList (qbits state)
  where ppcomplex (r :+ i) = concat [show r, " + ", show i, "i\n"]

-- adds a circuit to the environment
addCircuit :: Name -> QC -> Environment -> Environment
addCircuit name circuit env = env {
  circuits = Map.insert name circuit (circuits env)
}

-- adds a gate to the environment
addGate :: Name -> QC -> Environment -> Environment
addGate name gate env = env {
  gates = Map.insert name gate (gates env)
}

-- prepare a new qbit in the state by tensoring it at the end
tensorQBit :: State -> Name -> QBit -> State
tensorQBit s1 name qbit = State {
  qbits = tensor (qbits s1) qbit,
  qbitnames = qbitnames s1 ++ [name],
  qbitcontext = qbitcontext s1 ++ [name]
}

-- utility to generalize integer matrices
castFromInt :: (RealFloat a, Functor f) => f Int -> f (Complex a)
castFromInt = fmap $ (:+ 0) . fromIntegral

-- utility to generalize float matrices
castFromReal :: (RealFloat a, Functor f) => f a -> f (Complex a)
castFromReal = fmap (:+ 0)

-- enumerate a list
enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

-- enumerate a list with backward indices
renumerate :: Int -> [a] -> [(Int, a)]
renumerate n = zip $ reverse [0..n-1]
