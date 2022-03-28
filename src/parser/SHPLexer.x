{
module SHPLexer where

import Types
}

%wrapper "basic"

$digit = [0-9]
@id         = [A-Za-z][A-Za-z_]*

tokens :-
    $white+;
    "//".*; -- One-line comment
    dW                  { \s -> TokenDW     }
    dt                  { \s -> TokenDT     }
    abort               { \s -> TokenAbort } 
    skip                { \s -> TokenSkip } 
    if                  { \s -> TokenIf }
    then                { \s -> TokenThen }
    else                { \s -> TokenElse } 
    while               { \s -> TokenWhile }
    true                { \s -> TokenBool True }
    false               { \s -> TokenBool False }
    SHP                 { \s -> TokenSHP }
    \+\+                { \s -> TokenUnion }
    :=                  { \s -> TokenAssign } 
    \\\/                { \s -> TokenOr }
    \/\\                { \s -> TokenAnd }
    \~                  { \s -> TokenNot }
    \<=                 { \s -> TokenLEQ }
    >=                  { \s -> TokenGEQ }
    \<                  { \s -> TokenLT }
    \>                  { \s -> TokenGT }
    =                   { \s -> TokenEq }
    \+                  { \s -> TokenPlus }
    \-                  { \s -> TokenMinus }
    \/                  { \s -> TokenDiv }
    \(                  { \s -> TokenLParen }
    \)                  { \s -> TokenRParen }
    \{                  { \s -> TokenLCurl } 
    \}                  { \s -> TokenRCurl } 
    \,                   { \s -> TokenComma }
    \;                  { \s -> TokenSemi } 
    \&                  { \s -> TokenAmpersand } 
    \*                  { \s -> TokenStar } 
    \?                  { \s -> TokenQuestion }
    '                   { \s -> TokenPrime }
    $digit+(\.$digit+)? { \s -> TokenReal (read s)   }
    @id             { \s -> TokenIdent s }
{

}
