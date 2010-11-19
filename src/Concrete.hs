module Concrete where

import Text.PrettyPrint

import Common

newtype Declarations = Declarations { declarations :: [Declaration] }

data Declaration
  = TypeSig Name Expr
  | Defn Name (Maybe Expr) Expr

data Expr
  = Ident Name
  | Type                          -- ^ type
  | Fun   Expr Expr               -- ^ A -> B
  | Pi    Name Expr Expr          -- ^ {x:A} B
  | Lam   Name (Maybe Expr) Expr  -- ^ [x:A] E or [x]E
  | Apps  [Expr]                  -- ^ E1 E2 ... En     (non empty list)

instance Pretty Declarations where
  pretty (Declarations ds) = vcat $ map pretty ds

instance Pretty Declaration where
  pretty (TypeSig x a)        = hsep [ text x , colon , pretty a ] <> dot
  pretty (Defn x (Just a) e)  = hsep [ text x , colon , pretty a , equals , pretty e ] <> dot  
  pretty (Defn x Nothing e)   = hsep [ text x , equals , pretty e ] <> dot  

instance Pretty Expr where
  prettyPrec _ (Ident x)          = text x
  prettyPrec _ Type               = text "type"
  prettyPrec k (Fun a b)          = parensIf (k > 0) $ hsep
    [ prettyPrec 1 a , text "->" , prettyPrec 0 b ]
  prettyPrec k (Pi x a b)         = parensIf (k > 0) $ 
    braces (hsep [ text x , colon , pretty a ]) <+> pretty b
  prettyPrec k (Lam x (Just a) e) = parensIf (k > 0) $ 
    brackets (hsep [ text x , colon , pretty a ]) <+> pretty e
  prettyPrec k (Lam x Nothing e)  = parensIf (k > 0) $ 
    brackets (text x) <+> pretty e
  prettyPrec k (Apps es)          = parensIf (k > 1) $ hsep $ 
    map (prettyPrec 2) es

instance Show Declarations where
  show = render . pretty

instance Show Declaration where
  show = render . pretty

instance Show Expr where
  show = render . pretty
