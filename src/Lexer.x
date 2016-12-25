
{
{-# LANGUAGE BangPatterns #-}
module Lexer where

}

%wrapper "posn"

$u = [\0-\255]                  -- universal: any character
$reserved = [ \: \. \( \) \[ \] \{ \} \% \" ] -- may not appear in identifiers, delimit ids
$idchar = $printable # [ $white $reserved ] -- may appear in characters

tokens :-

$white+             ;
"%%"             .* ;
"%" [ $white # \n ] .* ;
"%" \n ;
"%{" ([$u # \}] | \} [$u # \%])* (\})* "}%" ;

"%abbrev"           { tok (\p s -> Abbrev p) }
"%infix"            { tok (\p s -> Infix p) }
"%prefix"           { tok (\p s -> Prefix p) }
"%postfix"          { tok (\p s -> Postfix p) }
"%name"          .* ;
"%query"         .* ;
"%clause"           { tok (\p s -> Clause p) }
"%tabled"        .* ;
"%querytabled"   .* ;
"%deterministic" .* ;
"%mode"          .* ;
"%terminates"    .* ;
"%reduces"       .* ;
"%block"         .* ;
"%worlds"        .* ;
"%total"         .* ;
"%freeze"        .* ;
"%theorem"       .* ;
"%prove"         .* ;
"%establish"     .* ;
"%assert"        .* ;
"%use"           .* ;
\{                  { tok (\p s -> BrOpen p) }
\}                  { tok (\p s -> BrClose p) }
\[                  { tok (\p s -> BracketOpen p) }
\]                  { tok (\p s -> BracketClose p) }
\(                  { tok (\p s -> PrOpen p) }
\)                  { tok (\p s -> PrClose p) }
\:                  { tok (\p s -> Col p) }
\.                  { tok (\p s -> Dot p) }

"->"                { tok (\p s -> Arrow p)  }
"<-"                { tok (\p s -> RevArrow p)  }
=                   { tok (\p s -> Eq p) }
type                { tok (\p s -> Type p) }

$idchar +           { tok (\p s -> (Id s p )) }


{
-- \_                              { tok (\p s -> Hole p) }

data Token
  = Id String    AlexPosn
  | Abbrev       AlexPosn
  | Clause       AlexPosn
  | Infix        AlexPosn
  | Prefix       AlexPosn
  | Postfix      AlexPosn
  | BrOpen       AlexPosn
  | BrClose      AlexPosn
  | BracketOpen  AlexPosn
  | BracketClose AlexPosn
  | PrOpen       AlexPosn
  | PrClose      AlexPosn
  | Col          AlexPosn
  | Dot          AlexPosn
  | Arrow        AlexPosn
  | RevArrow     AlexPosn
  | Hole         AlexPosn
  | Eq           AlexPosn
  | Type         AlexPosn
  | NotUsed      AlexPosn -- so happy doesn't generate overlap case pattern warning
    deriving (Eq)

prettyTok :: Token -> String
prettyTok c = "\"" ++ tk ++ "\" at " ++ (prettyAlexPosn pos) where
  (tk,pos) = showTok c

showTok c =
  case c of
    (Id s p) -> (show s,p)
    Abbrev p -> ("%abbrev",p)
    Clause p -> ("%clause",p)
    Infix p -> ("%infix",p)
    Prefix p -> ("%prefix",p)
    Postfix p -> ("%postfix",p)
    Type p -> ("type",p)
    BrOpen p -> ("{",p)
    BrClose p -> ("}",p)
    BracketOpen p -> ("[",p)
    BracketClose p -> ("]",p)
    PrOpen p -> ("(",p)
    PrClose p -> (")",p)
    Col p -> (":",p)
    Dot p -> (".",p)
    Arrow p -> ("->",p)
    RevArrow p -> ("<-",p)
    Hole p -> ("_",p)
    Eq p -> ("=",p)
    _ -> error "not used"

instance Show Token where
  show = fst . showTok


prettyAlexPosn (AlexPn _ line row) = "line " ++ show line ++ ", row " ++ show row

tok f p s = f p s

}
