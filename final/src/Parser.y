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
Circuit : CIR VAR '(' PreparationList ')' Body { QCCircuit $2 $4 $6 }

PreparationList :: { [QC] }
PreparationList : {- empty -} { [] }
PreparationList : Preparation { [$1] }
PreparationList : Preparation ',' { [$1] } -- optional trailing comma
PreparationList : Preparation ',' PreparationList { $1 : $3 }

Preparation :: { QC }
Preparation : INT '->' VAR { QCPreparation $1 $3 }

Body :: { [QC] }
Body : '{' Statements '}' { $2 }

Statements :: { [QC] }
Statements : {- empty -} { [] }
Statements : Statement ';' Statements { $1 : $3 }

Statement : Definition { $1 }
          | Operation  { $1 }

Definition :: { QC }
Definition : GATE ArgumentList '->' VAR Body { QCGate $4 $2 $5 }

Operation :: { QC }
Operation : ArgumentList '->' Chain { QCOperation $1 $3 }

ArgumentList :: { [String] }
ArgumentList : VAR { [$1] }
ArgumentList : VAR ',' { [$1] } -- optional trailing comma
ArgumentList : VAR ',' ArgumentList { $1 : $3 }

Chain :: { QC }
Chain : Tensor           { QCTensors $1 }
Chain : Chain '->' Chain { QCArrow $1 $3 }

Tensor :: { [QC] }
Tensor : Operator        { [$1] }
Tensor : Operator Tensor { $1 : $2 }

Operator :: { QC }
Operator : VAR { QCVariable $1 }
         | '|' { QCVariable "I" }

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