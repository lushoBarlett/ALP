module PrettyPrint (prettyPrint) where

import Common (QC(..))

ppbody :: Int -> [QC] -> String
ppbody tabs = foldMap (\b -> pp (tabs + 1) b ++ ";" ++ endl)

pppreps :: Int -> [QC] -> String
pppreps tabs = foldMap (\p -> pp tabs p ++ ", ")

ppargs :: [String] -> String
ppargs = foldMap (++ ", ")

withParens :: String -> String
withParens s = "(" ++ s ++ ")"

withBraces :: Int -> String -> String
withBraces tabs s = "{" ++ endl ++ s ++ repeatTabs tabs ++ "}"

withSpaces :: String -> String
withSpaces s = " " ++ s ++ " "

arrow :: String
arrow = " -> "

endl :: String
endl = "\n"

repeatTabs :: Int -> String
repeatTabs = flip replicate '\t'

pptensor :: [QC] -> String
pptensor = foldMap (\b -> pp 0 b ++ ".")

pp :: Int -> QC -> String
pp tabs (QCCircuit name preps body) = repeatTabs tabs ++ "circuit " ++ name ++ withSpaces (withParens (pppreps tabs preps)) ++ withBraces tabs (ppbody tabs body)
pp _    (QCPreparation n name) = show n ++ arrow ++ name
pp tabs (QCGate name args body) = repeatTabs tabs ++ "gate " ++ ppargs args ++ arrow ++ name ++ withSpaces (withBraces tabs (ppbody tabs body))
pp tabs (QCOperation qbitnames qc) = repeatTabs tabs ++ ppargs qbitnames ++ arrow ++ pp 0 qc
pp tabs (QCArrow ops1 ops2) = repeatTabs tabs ++ pp tabs ops1 ++ arrow ++ pp tabs ops2
pp tabs (QCTensors ops) = repeatTabs tabs ++ pptensor ops
pp _    (QCVariable name) = name
pp _    QCI = "|"
pp _    QCX = "X"
pp _    QCY = "Y"
pp _    QCZ = "Z"
pp _    QCH = "H"

prettyPrint :: QC -> String
prettyPrint = pp 0