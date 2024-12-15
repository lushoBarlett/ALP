module PrettyPrint(pp, ppState) where

import           AST          (QC (..), AngleExpr (..))
import           Data.Complex (Complex ((:+)))
import           Matrix       (ColMatrix (cmAsList))
import           State        (State)

-- adds parentheses around a string
withParens :: String -> String
withParens s = "(" ++ s ++ ")"

-- adds braces with newlines around a string
withBraces :: String -> String
withBraces s = "{\n" ++ s ++ "\n}"

-- repeats tabs a number of times
repeatTabs :: Int -> String
repeatTabs = flip replicate '\t'

-- helper function for pretty printing that takes care of the indentation
pp' :: QC -> Int -> String
pp' QCSkip        tabs = repeatTabs tabs ++ "skip"
pp' (QCRx a)      tabs = repeatTabs tabs ++ "Rx" ++ withParens (ppAngle a)
pp' (QCRy a)      tabs = repeatTabs tabs ++ "Ry" ++ withParens (ppAngle a)
pp' (QCRz a)      tabs = repeatTabs tabs ++ "Rz" ++ withParens (ppAngle a)
pp' QCSwap        tabs = repeatTabs tabs ++ "swap"
pp' (QCSeq a b)   tabs = pp' a tabs ++ ";\n" ++ pp' b tabs
pp' (QCPar a b)      _ = ppinline a ++ " | " ++ ppinline b
pp' (QCControl a) tabs = "ctrl " ++ withBraces (pp' a (tabs + 1))

ppinline :: QC -> String
ppinline (QCSeq a b) = withParens $ pp' a 0 ++ "; " ++ pp' b 0
ppinline ast = pp' ast 0

-- main pretty-printing function
pp :: QC -> String
pp q = pp' q 0

-- pretty-prints the state
ppState :: State -> String
ppState state = concatMap ppcomplex $ cmAsList state
  where ppcomplex (r :+ i) = concat [show r, " + ", show i, "i\n"]

ppAngle :: AngleExpr -> String
ppAngle (AngleConst a) = if (floor a :: Int) == ceiling a then show (round a :: Int) else show a
ppAngle AnglePi        = "π"
ppAngle (AngleNeg a)   = "-" ++ ppAngle a
ppAngle (AngleAdd a b) = ppAngle a ++ " + " ++ ppAngle b
ppAngle (AngleSub a b) = ppAngle a ++ " - " ++ ppAngle b
ppAngle (AngleMul a b) = ppAngle a ++ " * " ++ ppAngle b
ppAngle (AngleDiv a b) = ppAngle a ++ " / " ++ ppAngle b
ppAngle (AngleParen a) = withParens $ ppAngle a