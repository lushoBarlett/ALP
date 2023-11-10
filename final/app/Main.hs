module Main (main) where

import Parser (qcparser, lexer)
import Common (showState)
import PrettyPrint (pp)
import Eval (eval)

main :: IO ()
main = undefined