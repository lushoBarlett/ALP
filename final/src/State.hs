module State (
  State,
  Operator,
  linearTransformation,
  tensoreye,
  castFromInt,
  castFromReal,
  enumerate,
  renumerate
) where

import           Data.Complex    (Complex (..))
import           Matrix          (ColMatrix (..))
import           QBit            (QBitBase (..), allBases, fromCols,
                                  toColMatrix)

-- the state of the quantum computer is a 2^n-dimensional vector
-- of complex numbers, where n is the number of qubits
type State = ColMatrix (Complex Double)

-- an operator is a unitary 2^n x 2^n matrix of complex numbers
type Operator = ColMatrix (Complex Double)

-- given a function that takes a base and returns a column vector,
-- returns the matrix form asoociated with that function
linearTransformation :: Int -> (QBitBase -> ColMatrix a) -> ColMatrix a
linearTransformation n f = fromCols $ f <$> allBases n

-- returns the identity matrix of dimension 2^n
tensoreye :: Int -> ColMatrix Int
tensoreye n = linearTransformation n toColMatrix

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