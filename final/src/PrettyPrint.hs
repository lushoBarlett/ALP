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

pp :: Int -> QC -> String
pp tabs (QCCircuit name preps body) = repeatTabs tabs ++ "circuit " ++ name ++ withSpaces (withParens (pppreps tabs preps)) ++ withBraces tabs (ppbody tabs body)
pp _    (QCPreparation n name) = show n ++ arrow ++ name
pp tabs (QCGate name args body) = repeatTabs tabs ++ "gate " ++ ppargs args ++ arrow ++ name ++ withSpaces (withBraces tabs (ppbody tabs body))
pp tabs (QCArrow op1 op2) = repeatTabs tabs ++ pp 0 op1 ++ arrow ++ pp 0 op2
pp tabs (QCTensor op1 op2) = pp tabs op1 ++ pp tabs op2
pp _    (QCVariable name) = name
pp _    QCIdentity = "|"

prettyPrint :: QC -> String
prettyPrint = pp 0