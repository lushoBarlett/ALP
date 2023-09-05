module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "if", "else", "repeat", "skip", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )

-- Santa's little helper function
intBinOp opstring op = do
  e1 <- intexp
  reservedOp lis opstring
  e2 <- intexp
  return $ op e1 e2

boolBinOp opstring op = do
  e1 <- boolexp
  reservedOp lis opstring
  e2 <- boolexp
  return $ op e1 e2

----------------------------------
--- Parser de expressiones enteras
-----------------------------------
intexp :: Parser (Exp Int)
intexp = pint <|> pvar <|> opposite <|> parenthesis <|> plus <|> minus <|> times <|> div <|> assgn <|> seq
  where
    pint = Const <$> int
    pvar = Var <$> identifier
    parenthesis = parens lis intexp

    opposite = reservedOp lis "-" >> Neg <$> intexp

    plus = intBinOp "+" Plus
    minus = intBinOp "-" Minus
    times = intBinOp "*" Times
    div = intBinOp "/" Div

    assgn = do
      v <- identifier
      reservedOp lis "="
      e <- intexp
      return $ EAssgn v e

    seq = intBinOp "," ESeq

-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = ptrue <|> pfalse <|> negation <|> parenthesis <|> lt <|> gt <|> eq <|> neq <|> and <|> or
  where
    ptrue  = reserved lis "true" >> return BTrue
    pfalse = reserved lis "false" >> return BFalse
    negation = reservedOp lis "!" >> Not <$> boolexp
    parenthesis = parens lis boolexp

    lt = intBinOp "<" Lt
    gt = intBinOp ">" Gt
    eq = intBinOp "==" Eq
    neq = intBinOp "!=" NEq

    and = boolBinOp "&&" And
    or = boolBinOp "||" Or

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = pskip <|> passign <|> pif <|> prepeat <|> pseq <|> pwhile
  where
    pskip = reserved lis "skip" >> return Skip

    passign = do
      v <- identifier
      reservedOp lis "="
      e <- intexp
      return $ Let v e

    pseq = do
      c1 <- comm
      reservedOp lis ";"
      c2 <- comm
      return $ Seq c1 c2

    pif = do
      reserved lis "if"
      b <- boolexp
      c <- braces comm
      -- Don't allow "if else" to be parsed
      -- as "if" followed by "else"
      notFollowedBy $ reserved lis "else"
      return $ IfThen b c

    pifelse = do
      reserved lis "if"
      b <- boolexp
      c1 <- braces comm
      reserved lis "else"
      c2 <- braces comm
      return $ IfThenElse b c1 c2

    prepeat = do
      reserved lis "repeat"
      c <- braces comm
      reserved lis "until"
      b <- boolexp
      return $ Repeat c b

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
