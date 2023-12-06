{
module Parser where
import Common
import Data.Char
}

%name qcparser
%tokentype { Token }
%error { parseError }
%monad { E } { thenE } { returnE }

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
  '~'  { TokenNegation }
  CIR  { TokenCircuit }
  GATE { TokenGate }
  IF   { TokenIf }

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
          | If         { $1 }

Definition :: { QC }
Definition : GATE ArgumentList '->' VAR Body { QCGate $4 $2 $5 }

Operation :: { QC }
Operation : ArgumentList '->' Chain { QCOperation $1 $3 }

ArgumentList :: { [String] }
ArgumentList : VAR { [$1] }
ArgumentList : VAR ',' { [$1] } -- optional trailing comma
ArgumentList : VAR ',' ArgumentList { $1 : $3 }

If :: { QC }
If : IF IfList Body { QCIf $2 $3 }

IfList :: { [QC] }
IfList : {- empty -} { [] }
IfList : IfCondition { [$1] }
IfList : IfCondition ',' { [$1] } -- optional trailing comma
IfList : IfCondition ',' IfList { $1 : $3 }

IfCondition :: { QC }
IfCondition : VAR { QCVariable $1 }
IfCondition : '~' VAR { QCNegatedVariable $2 }

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
  | TokenNegation
  | TokenCircuit
  | TokenGate
  | TokenIf
  deriving Show

parseError :: [Token] -> E a
parseError tokenList = failE ("Parse error on token list: " ++ show tokenList)

-- stolen from the Happy documentation
-- https://haskell-happy.readthedocs.io/en/latest/using.html#monadic-parsers
data E a = Ok a | Failed String

thenE :: E a -> (a -> E b) -> E b
m `thenE` k =
   case m of
       Ok a     -> k a
       Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k =
   case m of
      Ok a     -> Ok a
      Failed e -> k e

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
lexer ('~':cs) = TokenNegation : lexer cs

lexVar cs =
  case span isAlpha cs of
    ("circuit",rest) -> TokenCircuit : lexer rest
    ("gate",rest) -> TokenGate : lexer rest
    ("if",rest) -> TokenIf : lexer rest
    (var,rest) -> TokenVar var : lexer rest

lexNum cs =
  case span isDigit cs of
    (num,rest) -> TokenInt (read num) : lexer rest

}