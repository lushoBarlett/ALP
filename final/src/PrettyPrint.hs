module PrettyPrint (pp) where

import Common (QC(..))

ppbody :: [QC] -> String
ppbody = foldMap (\b -> pp b ++ "\n")

pppreps :: [QC] -> String
pppreps = foldMap (\p -> pp p ++ ", ")

ppargs :: [String] -> String
ppargs = foldMap (++ ", ")

withParens :: String -> String
withParens s = "(" ++ s ++ ")"

withBraces :: String -> String
withBraces s = "{" ++ s ++ "}"

pp :: QC -> String
pp (QCCircuit name preps body) = "circuit " ++ name ++ withParens (pppreps preps) ++ withBraces (ppbody body)
pp (QCPreparation n name) = show n ++ "->" ++ name
pp (QCGate name args body) = "gate " ++ ppargs args ++ name ++ withBraces (ppbody body)
pp (QCArrow op1 op2) = pp op1 ++ " -> " ++ pp op2
pp (QCTensor op1 op2) = pp op1 ++ pp op2
pp (QCVariable name) = name
pp QCIdentity = "|"