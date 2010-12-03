module Abstract where

import Concrete (Fixity)

-- | Names in the abstract syntax are unique identifiers
type Name = Int  -- Integer not deemed necessary

newtype Declarations = Declarations { declarations :: [Declaration] }

data Declaration
  = TypeSig Name Type
  | Defn Name (Maybe Type) Expr

type Type = Expr
data Expr
  = Atom  Atom
  | Pi    (Maybe Name) Type Type  -- ^ A -> B or {x:A} B
  | Lam   Name (Maybe Type) Expr  -- ^ [x:A] E or [x]E
  | App   Expr Expr               -- ^ E1 E2 

data Atom 
  = Var Name                      -- ^ locally bound identifier
  | Con Name                      -- ^ declared constant
  | Def Name                      -- ^ defined identifier
  | Typ                           -- ^ type

