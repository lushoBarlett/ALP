module PrettyPrint (prettyPrint) where

import Common (QC(..))

-- pretty prints a list of sequential statements
ppbody :: Int -> [QC] -> String
ppbody tabs = foldMap (\b -> pp (tabs + 1) b ++ ";" ++ endl)

-- pretty prints the preparation of qubits
pppreps :: Int -> [QC] -> String
pppreps tabs = foldMap (\p -> pp tabs p ++ ", ")

-- pretty prints the arguments of a gate
ppargs :: [String] -> String
ppargs = foldMap (++ ", ")

-- adds parentheses around a string
withParens :: String -> String
withParens s = "(" ++ s ++ ")"

-- adds braces around a string, in C-like style
withBraces :: Int -> String -> String
withBraces tabs s = "{" ++ endl ++ s ++ repeatTabs tabs ++ "}"

-- adds spaces around a string
withSpaces :: String -> String
withSpaces s = " " ++ s ++ " "

-- standard arrow
arrow :: String
arrow = " -> "

-- standard newline
endl :: String
endl = "\n"

-- repeats tabs a number of times
repeatTabs :: Int -> String
repeatTabs = flip replicate '\t'

-- pretty prints a tensor product, putting spaces between the elements
pptensor :: [QC] -> String
pptensor = foldMap (\b -> pp 0 b ++ " ")

-- wrapper for pretty printing a body with braces
braceBody :: Int -> [QC] -> String
braceBody tabs body = withBraces tabs (ppbody tabs body)

-- helper function for pretty printing that takes care of the indentation
pp :: Int -> QC -> String
pp tabs (QCCircuit name preps body) = repeatTabs tabs ++ "circuit " ++ name ++ withSpaces (withParens (pppreps tabs preps)) ++ braceBody tabs body
pp _    (QCPreparation n name) = show n ++ arrow ++ name
pp tabs (QCGate name args body) = repeatTabs tabs ++ "gate " ++ ppargs args ++ arrow ++ name ++ withSpaces (braceBody tabs body)
pp tabs (QCIf conditions body) = repeatTabs tabs ++ "if " ++ ppargs (pp 0 <$> conditions) ++ withSpaces (braceBody tabs body)
pp tabs (QCOperation qbitnames qc) = repeatTabs tabs ++ ppargs qbitnames ++ arrow ++ pp 0 qc
pp tabs (QCArrow ops1 ops2) = repeatTabs tabs ++ pp tabs ops1 ++ arrow ++ pp tabs ops2
pp tabs (QCTensors ops) = repeatTabs tabs ++ pptensor ops
pp _    (QCVariable name) = name
pp _    (QCNegatedVariable name) = "~" ++ name
pp _    QCI = "|"
pp _    QCX = "X"
pp _    QCY = "Y"
pp _    QCZ = "Z"
pp _    QCH = "H"

-- main pretty-printing function
prettyPrint :: QC -> String
prettyPrint = pp 0