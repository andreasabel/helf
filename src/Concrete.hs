module Concrete where

import Text.PrettyPrint

import Common
import OperatorPrecedenceParser (Associativity(..))
import qualified OperatorPrecedenceParser as OPP

newtype Declarations = Declarations { declarations :: [Declaration] }

data Declaration
  = TypeSig Name Expr
  | Defn Name (Maybe Expr) Expr
  | Fixity Name Fixity

type Fixity = OPP.Fixity Int   -- precedences: 0 <= ... < 10000  

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
  pretty d = hsep (pr d) <> dot where
    pr (TypeSig x a)       = [ text x , colon , pretty a ] 
    pr (Defn x (Just a) e) = [ text x , colon , pretty a , equals , pretty e ] 
    pr (Defn x Nothing e)  = [ text x , equals , pretty e ] 
    pr (Fixity x (OPP.Prefix p))  = [ text "%prefix" , int p , text x ] 
    pr (Fixity x (OPP.Postfix p)) = [ text "%postfix", int p , text x ] 
    pr (Fixity x (OPP.Infix p a)) = [ text "%infix",  pretty a, int p , text x ] 

instance Pretty Associativity where
  pretty = text . show

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

instance Show Associativity where
  show a = case a of
    AssocLeft  -> "left"
    AssocRight -> "right"
    AssocNone  -> "none"

instance Read Associativity where
  readsPrec _ s = [(a,"")] where 
    a = case s of
      "left"  -> AssocLeft 
      "right" -> AssocRight
      "none"  -> AssocNone 

instance Show Expr where
  show = render . pretty
