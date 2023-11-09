module Main (main) where

import Parser (qcparse, lexer)

main :: IO ()
main = do
  contents <- getContents
  print $ qcparse $ lexer contents