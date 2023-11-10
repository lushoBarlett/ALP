{
module Parser where
import Common
import Data.Char
}

%name qcparser
%tokentype { Token }
%error { parseError }

%token
  INT  { TokenInt $$ }
  VAR  { TokenVar $$ }
  '|'  { TokenIdentity }
  '->' { TokenArrow }
  '('  { TokenLParen }
  ')'  { TokenRParen }
  '{'  { TokenLBrace }
  '}'  { TokenRBrace }
  ','  { TokenComma }
  ';'  { TokenSemicolon }
  CIR  { TokenCircuit }
  GATE { TokenGate }

%left "->"

%%

Circuit :: { QC }
Circuit : CIR VAR '(' PrepList ')' CirBody { QCCircuit $2 $4 $6 }

PrepList :: { [QC] }
PrepList : Prep { [$1] }
PrepList : Prep ',' PrepList { $1 : $3 }

Prep :: { QC }
Prep : INT '->' VAR { QCPreparation $1 $3 }

CirBody :: { [QC] }
CirBody : '{' Statements '}' { $2 }

Statements :: { [QC] }
Statements : Expr { [$1] }
Statements : Expr ';' Statements { $1 : $3 }
Statements : Def ';' Statements { $1 : $3 }

Def :: { QC }
Def : GATE ArgList '->' VAR CirBody { QCGate $4 $2 $5 }

ArgList :: { [String] }
ArgList : VAR { [$1] }
ArgList : VAR ',' ArgList { $1 : $3 }

Expr :: { QC }
Expr : Tensor         { $1 }
Expr : Expr '->' Expr { QCArrow $1 $3 }

Tensor :: { QC }
Tensor : Operator        { $1 }
Tensor : Operator Tensor { QCTensor $1 $2 }

Operator :: { QC }
Operator : VAR { QCVariable $1 }
         | '|' { QCIdentity }

{
data Token
  = TokenInt Int
  | TokenVar String
  | TokenEquals
  | TokenIdentity
  | TokenArrow
  | TokenLParen
  | TokenRParen
  | TokenLBrace
  | TokenRBrace
  | TokenComma
  | TokenSemicolon
  | TokenCircuit
  | TokenGate
  deriving Show

parseError :: [Token] -> a
parseError tokenList = error ("Parse error on token list: " ++ show tokenList)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | isSpace c = lexer cs
  | isAlpha c = lexVar (c:cs)
  | isDigit c = lexNum (c:cs)
lexer ('=':cs) = TokenEquals : lexer cs
lexer ('|':cs) = TokenIdentity : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('(':cs) = TokenLParen : lexer cs
lexer (')':cs) = TokenRParen : lexer cs
lexer ('{':cs) = TokenLBrace : lexer cs
lexer ('}':cs) = TokenRBrace : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer (';':cs) = TokenSemicolon : lexer cs

lexVar cs =
  case span isAlpha cs of
    ("circuit",rest) -> TokenCircuit : lexer rest
    ("gate",rest) -> TokenGate : lexer rest
    (var,rest) -> TokenVar var : lexer rest

lexNum cs =
  case span isDigit cs of
    (num,rest) -> TokenInt (read num) : lexer rest

}