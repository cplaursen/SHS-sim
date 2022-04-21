{
module SHPParser where
import SHPLexer
import Types
}

%name parseSHP SHP
%name parseSHPProg SHPProg
%tokentype { Token }
%error { parseError }

%token
    id      { TokenIdent $$ }
    "("     { TokenLParen }
    ")"     { TokenRParen }
    real    { TokenReal $$ }
    bool    { TokenBool $$ }
    dW      { TokenDW }
    dt      { TokenDT }
    skip    { TokenSkip }
    abort   { TokenAbort }
    if      { TokenIf }
    then    { TokenThen }
    else    { TokenElse }
    while   { TokenWhile }
    shp     { TokenSHP }
    "'"     { TokenPrime }
    ","     { TokenComma }
    "{"     { TokenLCurl }
    "}"     { TokenRCurl }
    ";"     { TokenSemi } 
    "&"     { TokenAmpersand }
    ":="    { TokenAssign }
    "*"     { TokenStar  }
    "++"    { TokenUnion }
    "?"     { TokenQuestion }
    or      { TokenOr }
    and     { TokenAnd }
    "~"     { TokenNot }
    "+"     { TokenPlus }
    "-"     { TokenMinus }
    ">"     { TokenGT }
    "<"     { TokenLT }
    ">="    { TokenGEQ }
    "<="    { TokenLEQ }
    "="     { TokenEq }
    "/"     { TokenDiv }

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

SHPProg : Blocks { reverse $1 }

Blocks : Block { [$1] }
       | Blocks Block { $2 : $1 }

Block : id "{" Definitions "}" { DefBlock $1 (reverse $3) }
      | shp "{" SHP "}" { SHPBlock $3 }

Definitions : Definition { [$1] }
            | Definitions Definition { $2 : $1 }

Definition : id "=" Expr ";" { Definition $1 $3 }

SHP :: { SHP }
SHP : SHP ";" SHP   { Composition $1 $3 } 
    | SHP "++" SHP { Choice 0.5 $1 $3 } -- Fix this syntax
    | if Pred then SHP else SHP  { Cond $2 $4 $6 }
    | if Pred then SHP { Cond $2 $4 Abort } -- Might want skip instead of abort
    -- | "{" [id "=" Expr] "}" dt "+" "{"[id "=" "{"[id "=" Expr]"}"] "}" dW "&" Pred -- Deal with this later
    | "?" Pred { Cond $2 Skip Abort }
    | "(" SHP ")" { $2 }
    | while Pred SHP { While $2 $3 }
    | id ":=" "{" Expr "," Expr "}" { RandAssn $1 $4 $6 }
    | id ":=" Expr { Assn $1 $3 }
    | abort { Abort }
    | skip { Skip }
    | SDE { $1 }

SDE : Drift "&" Pred { SDE $1 [] $3 }
    | Volatility "&" Pred { SDE [] $1 $3 }
    | Drift "+" Volatility "&" Pred { SDE $1 $3 $5 }

Drift : "{" Differentials "}" dt { $2 }

Volatility : "{" Differentials "}" dW { $2 }

Diff : id "'" "=" Expr { Diff $1 $4 }

Differentials : Diff { [$1] }
  | Differentials "," Diff { $3 : $1 }

Expr :: { Expr }
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
     | Expr "=" Expr  { Compare "==" $1 $3 }
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
}
