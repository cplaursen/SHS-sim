{
module SHPParser where
import SHPLexer
import Types
import Lens.Micro.Platform ((&), (%~), (.~))
}

%name parseSHP SHP
%name parseSHPProg SHPProg
%tokentype { Token }
%error { parseError }

%token
    "SHP"    { TokenIdent "SHP"}
    "Enum"   { TokenIdent "Enum"}
    "Consts" { TokenIdent "Consts"}
    "Continuous" { TokenIdent "Continuous" }
    dW       { TokenDW }
    dt       { TokenDT }
    skip     { TokenSkip }
    abort    { TokenAbort }
    if       { TokenIf }
    then     { TokenThen }
    else     { TokenElse }
    while    { TokenWhile }
    id       { TokenIdent $$ }
    "("      { TokenLParen }
    ")"      { TokenRParen }
    real     { TokenReal $$ }
    bool     { TokenBool $$ }
    "'"      { TokenPrime }
    ","      { TokenComma }
    "{"      { TokenLCurl }
    "}"      { TokenRCurl }
    ";"      { TokenSemi } 
    "&"      { TokenAmpersand }
    ":="     { TokenAssign }
    "*"      { TokenStar  }
    "++"     { TokenUnion }
    "?"      { TokenQuestion }
    or       { TokenOr }
    and      { TokenAnd }
    "~"      { TokenNot }
    "+"      { TokenPlus }
    "-"      { TokenMinus }
    ">"      { TokenGT }
    "<"      { TokenLT }
    ">="     { TokenGEQ }
    "<="     { TokenLEQ }
    "="      { TokenEq }
    "/"      { TokenDiv }

%nonassoc "<" ">" "<=" ">=" ":=" "="

%right then else
%left ";"
%left "++"
%left or
%left and
%left "+" "-"
%left "*" "/"
%left NEG

%%

SHPProg : Blocks { $1 }

Blocks : {- Empty -} { emptyBlock }
       | Blocks "SHP" "{" SHP "}" { $1 & shpBlock .~ $4 }
       | Blocks "Consts" "{" Definitions "}" { $1 & constBlock %~ (++ reverse $4) }
       | Blocks "Enum" "{" Ids "}" { $1 & enumBlock %~ (++ reverse $4) }
       | Blocks "Continuous" "{" Ids "}" { $1 & contBlock %~ (++ reverse $4) }

Definitions : Definition { [$1] }
            | Definitions Definition { $2 : $1 }

Definition : id "=" Expr ";" { Definition $1 $3 }

Ids : id { [$1] }
    | Ids "," id { $3 : $1 }

SHP :: { SHP }
SHP : {- Empty -} { Skip }
    | SHP ";" SHP   { Composition $1 $3 } 
    | SHP "++" SHP { Choice 0.5 $1 $3 } -- Fix this syntax
    | if Pred then SHP else SHP  { Cond $2 $4 $6 }
    | if Pred then SHP { Cond $2 $4 Abort } -- Might want skip instead of abort
    -- | "{" [id "=" Expr] "}" dt "+" "{"[id "=" "{"[id "=" Expr]"}"] "}" dW "&" Pred -- Deal with this later
    | "?" Pred { Cond $2 Skip Abort }
    | "(" SHP ")" { $2 }
    | while Pred SHP { While $2 $3 }
    | id ":=" "{" Expr "," Expr "}" { RandAssn $1 $4 $6 }
    | id ":=" Expr { Assn $1 (ArithExpr $3) }
    | abort { Abort }
    | skip { Skip }
    | SDE { $1 }

SDE :: { SHP }
SDE : Drift "&" Pred { SDE $1 [] $3 }
    | Volatility "&" Pred { SDE [] $1 $3 }
    | Drift "+" Volatility "&" Pred { SDE $1 $3 $5 }

Drift :: { [Diff] }
Drift : "{" Differentials "}" dt { $2 }

Volatility :: { [Diff] }
Volatility : "{" Differentials "}" dW { $2 }

Diff :: { Diff }
Diff : id "'" "=" Expr { Diff $1 $4 }

Differentials :: { [Diff] }
Differentials : Diff { [$1] }
  | Differentials "," Diff { $3 : $1 }

Expr :: { ArithExpr }
Expr : real         { Real $1 }
    | id            { Var $1 }
    | Expr "+" Expr { Bop "+" $1 $3 }
    | Expr "-" Expr { Bop "-" $1 $3 }
    | Expr "*" Expr { Bop "*" $1 $3 }
    | Expr "/" Expr { Bop "/" $1 $3 }
    | "-" Expr %prec NEG { Bop "-" (Real 0) $2 }
    | "(" Expr ")"  { $2 }

Pred :: { Pred }
Pred : bool { Bool $1 }
     | Expr "=" Expr  { PredEq (ArithExpr $1) (ArithExpr $3) }
     | Expr ">" Expr  { Compare ">" $1 $3 }
     | Expr "<" Expr  { Compare "<" $1 $3 }
     | Expr ">=" Expr { Compare ">=" $1 $3 }
     | Expr "<=" Expr { Compare "<=" $1 $3 }
     | Pred or Pred   { Or $1 $3 }
     | Pred and Pred  { And $1 $3 }
     | "~" Pred       { Not $2 }
     | "(" Pred ")"   { $2 }

{ 

parseError :: [Token] -> a
parseError _ = error "Parse error"

emptyBlock :: Blocks
emptyBlock = Blocks Skip [] [] []

}
