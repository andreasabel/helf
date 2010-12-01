{ 
module Parser where

import qualified Lexer as T
import qualified OperatorPrecedenceParser as C
-- import qualified Common as C
import qualified Concrete as C

}

%name parse
%tokentype { T.Token }
%error { parseError }

%token

id         { T.Id $$ _ }
'%abbrev'  { T.Abbrev _}
'%infix'   { T.Infix _}
'%prefix'  { T.Prefix _}
'%postfix' { T.Postfix _}
'{'        { T.BrOpen _ }
'}'        { T.BrClose _ }
'['        { T.BracketOpen _ }
']'        { T.BracketClose _ }
'('        { T.PrOpen _ }
')'        { T.PrClose _ }
':'        { T.Col _ }
'.'        { T.Dot _ }
'->'       { T.Arrow _ }
'<-'       { T.RevArrow _ }
'='        { T.Eq _ }
'_'        { T.Hole _ }
type       { T.Type _ }

%%

TopLevel :: { C.Declarations }
TopLevel : Declarations { C.Declarations $1}

Declarations :: { [C.Declaration] }
Declarations 
  : {- empty -}                    { [] }
  | Declaration '.' Declarations   { $1 : $3 }

Declaration :: { C.Declaration }
Declaration 
  : TypeSig                       { $1 }
  | Defn                          { $1 }
  | '%abbrev' Defn                { $2 }
  | '%infix' Assoc Prec id        { C.Fixity $4 (C.Infix $3 $2) }
  | '%prefix' Prec id             { C.Fixity $3 (C.Prefix $2) }
  | '%postfix' Prec id            { C.Fixity $3 (C.Postfix $2) }

Assoc :: { C.Associativity }    
Assoc : id                        { read $1 }

Prec :: { Int }
Prec : id                         { read $1 }

TypeSig :: { C.Declaration }
TypeSig : id ':' Expr             { C.TypeSig $1 $3 }

Defn :: { C.Declaration }
Defn : id ':' Expr '=' Expr       { C.Defn $1 (Just $3) $5 } 
     | id '=' Expr                { C.Defn $1 Nothing $3 } 

-- general form of expression
Expr :: { C.Expr }
Expr : '{' id ':' Expr '}' Expr   { C.Pi $2 $4 $6 }  
     | Expr1 '->' Expr            { C.Fun $1 $3 }
     | Expr1                      { $1 }

-- perform applications
Expr1 :: { C.Expr }
Expr1 : Apps                      { if length $1 == 1 then (head $1) else C.Apps (reverse $1) } -- this should reduce the op stack

-- gather applications
Apps :: { [C.Expr] }
Apps : Atom                       { [$1] }
     | Apps Atom                  { $2 : $1 }  -- this should shift on the op stack

-- atoms
Atom :: { C.Expr }
Atom : type                       { C.Atom C.Typ  }
     | id                         { C.Atom (C.Ident $1) }
     | '(' Expr ')'               { $2 }
     | '[' id ':' Expr ']' Expr   { C.Lam $2 (Just $4) $6 } 
     | '[' id ']' Expr            { C.Lam $2 Nothing $4 }
{-
     | '_'                        { C.Unknown }
-}

{

parseError :: [T.Token] -> a
parseError [] = error "Parse error at EOF"
parseError (x : xs) = error ("Parse error at token " ++ T.prettyTok x) 

}

 



