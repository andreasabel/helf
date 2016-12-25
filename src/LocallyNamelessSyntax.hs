{-# LANGUAGE DeriveFunctor #-}
-- stuff taken from HerBruijnVal

module LocallyNamelessSyntax (BTm(..), DBIndex(..), Annotation(..), toLocallyNameless, fromLocallyNameless) where

import Data.Map (Map)
import qualified Data.Map as Map

import Abstract as A
import Util
import Value

-- * de Bruijn Terms

newtype Annotation a = Annotation { annotation :: a }
  deriving (Show, Functor)

instance Eq (Annotation a) where
  a == b = True

instance Ord (Annotation a) where
  a <= b = True

data DBIndex = DBIndex { index :: Int, identifier :: A.Name }

instance Eq DBIndex where
  i == j = index i == index j

instance Show DBIndex where
  show (DBIndex i x) = show x ++ "@" ++ show i

instance Ord DBIndex where
  compare i j = compare (index i) (index j)

data BTm -- names are only used for quoting
  = B DBIndex     -- bound variable: de Bruijn index
  | BVar A.Name   -- free variable: name
  | BCon A.Name
  | BDef A.Name
  | BApp BTm BTm
  | BLam (Annotation A.Name) BTm  -- name irrelevant for execution
  | BConstLam BTm -- these are lambdas, but not counted
  | BSort Value.Sort
  | BPi BTm BTm
    deriving (Eq, Ord)

instance IsApp BTm where
  isApp (BApp f e) = Just (f, e)
  isApp _          = Nothing


instance Show BTm where
    show = show . fromLocallyNameless

fromLocallyNameless :: BTm -> A.Expr
fromLocallyNameless t =
  case t of
    B i    -> Ident $ Var $ identifier i
    BVar x -> Ident $ Var x
    BCon x -> Ident $ Con x
    BDef x -> Ident $ Def x
    BApp t u -> App (fromLocallyNameless t) (fromLocallyNameless u)
    BPi u (BConstLam t) -> Pi Nothing (fromLocallyNameless u) (fromLocallyNameless t)
    BPi u (BLam (Annotation x) t)    -> Pi (Just x) (fromLocallyNameless u) (fromLocallyNameless t)

    BLam (Annotation x) t -> Lam x Nothing $ fromLocallyNameless t
    BConstLam t -> Lam A.noName Nothing $ fromLocallyNameless t
    BSort Type -> Typ
    -- impossible:  BSort Kind

-- * transformation, only used for evaluate2

-- maybe this should be transferred to Util.hs
data LocBoundList name = LBL {lblsize :: Int, bList :: (Map name Int)}
lbl_empty = LBL 0 (Map.empty)

insert_lbl :: (Ord name) => name -> LocBoundList name -> LocBoundList name
insert_lbl n (LBL k m) = LBL (k+1) (Map.insert n k m) -- note that it does NOT matter whether or not n already had been a key before!

-- fakeinsert :: (Ord name) => LocBoundList name -> LocBoundList name
-- fakeinsert (LBL k m) = LBL k+1 m

lookup_lbl :: (Ord name) => name -> LocBoundList name -> Maybe Int
lookup_lbl x (LBL _ m)= Map.lookup x m


toLocallyNameless :: A.Expr -> BTm
toLocallyNameless = trans lbl_empty where
  trans :: LocBoundList A.Name -> A.Expr -> BTm
  trans lbl (Ident ident) = case ident of
        Var x -> case lookup_lbl x lbl of
                        Just k  -> B $ DBIndex (lblsize lbl - 1 - k) x
                        Nothing -> BVar x
        Con x -> BCon x
        Def x -> BDef x
  trans lbl (App e1 e2) = BApp (trans lbl e1) (trans lbl e2)
  trans lbl (Lam name _ e) = BLam (Annotation name) $ trans (insert_lbl name lbl) e
  trans _ Typ = BSort Type
  trans lbl (Pi mname a b) = case mname of
    Just n ->
      let
        a' = trans lbl a
        b' = trans lbl $ Lam n Nothing b
      in
        BPi a' b'
    Nothing ->
      let
        a' = trans lbl a
        b' = BConstLam $ trans lbl b
      in
        BPi a' b'
