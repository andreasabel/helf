{-# LANGUAGE FlexibleInstances #-}

module Concrete where

import Prelude hiding ((<>))

import Text.PrettyPrint

import Util
import OperatorPrecedenceParser (Associativity(..))
import qualified OperatorPrecedenceParser as OPP

type Name = String
noName = ""

newtype Declarations = Declarations { declarations :: [Declaration] }

data Declaration
  = TypeSig Name Type             -- ^ @c : A.@
  | Defn Name (Maybe Type) Expr   -- ^ @d : A = e.@ or @d = e.@
--  | GLet Name Expr                -- ^ @[x = e].@  (global shared expr)
  | Fixity Name Fixity            -- ^ @%infix/%prefix/%postfix ...@

type Fixity = OPP.Fixity Int   -- precedences: 0 <= ... < 10000

type Type = Expr
data Expr
  = Ident Name
  | Typ                           -- ^ type
  | Fun   Type Type               -- ^ A -> B
  | Pi    Name Type Type          -- ^ {x:A} B
  | Lam   Name (Maybe Type) Expr  -- ^ [x:A] E or [x]E
  | Apps  [Expr]                  -- ^ E1 E2 ... En     (non empty list)
  | LLet  Name Expr Expr          -- ^ [x = E] E'       (local shared expr)

instance Pretty Declarations where
  pretty (Declarations ds) = vcat $ map pretty ds

instance Pretty Declaration where
  pretty d = hsep (pr d) <> dot where
    pr (TypeSig x a)       = [ text x , colon , pretty a ]
    pr (Defn x (Just a) e) = [ text x , colon , pretty a , equals , pretty e ]
    pr (Defn x Nothing e)  = [ text x , equals , pretty e ]
--    pr (GLet x e)          = [ text "[", text x , equals , pretty e, text "]" ]
    pr (Fixity x (OPP.Prefix p))  = [ text "%prefix" , int p , text x ]
    pr (Fixity x (OPP.Postfix p)) = [ text "%postfix", int p , text x ]
    pr (Fixity x (OPP.Infix p a)) = [ text "%infix",  pretty a, int p , text x ]

instance Pretty Associativity where
  pretty = text . show

instance Pretty Name where
  pretty = text

instance Pretty Expr where
  prettyPrec _ (Ident x)          = text x
  prettyPrec _ Typ                = text "type"
--   prettyPrec k (Atom a)           = prettyPrec k a
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
{-
    mapReverse (prettyPrec 2) es
      where mapReverse f = foldl (\ ys x -> f x : ys) []
-}
  prettyPrec k (LLet x e e') = parensIf (k > 0) $
    brackets (hsep [ text x , equals , pretty e' ]) <+> pretty e'

{-
data Atom
  = Ident Name
  | Typ                           -- ^ type

instance Pretty Atom where
  prettyPrec _ (Ident x)          = text x
  prettyPrec _ Typ                = text "type"
-}

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
