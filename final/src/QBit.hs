module QBit where

import Data.Bits

data QBitBase = QBitBase {
  baseValueRepr :: Int,
  tensorDimension :: Int
}

applyToBit f b = reverse [f b i | i <- [0..tensorDimension b - 1]]

instance Eq QBitBase where
  a == b = tensorDimension a == tensorDimension b && baseValueRepr a == baseValueRepr b

instance Show QBitBase where
  show b = "|" ++ applyToBit oz b ++ ">"
    where oz b i = if testBit (baseValueRepr b) i then '1' else '0'

qbitFromNumber :: Int -> QBitBase
qbitFromNumber 0 = QBitBase 0 1
qbitFromNumber 1 = QBitBase 1 1
qbitFromNumber _ = error "qbitFromNumber: number must be 0 or 1"

allBases :: Int -> [QBitBase]
allBases n = [QBitBase i n | i <- [0..2^n-1]]

instance Show a => Show (ColMatrix a) where
  show m = unlines [unwords [show $ (cmAsList m) !! (i + cmcols m * j) | j <- [0..cmcols m - 1]] | i <- [0..cmrows m - 1]]

toColMatrix :: QBitBase -> ColMatrix Int
toColMatrix b = ColMatrix (2^tensorDimension b) 1 $ [oz b i | i <- [0..2^(tensorDimension b)-1]]
  where oz b i = if baseValueRepr b == i then 1 else 0

fromCols :: [ColMatrix Int] -> ColMatrix Int
fromCols cs = ColMatrix (cmrows $ head cs) (length cs) $ concat $ cmAsList <$> cs
