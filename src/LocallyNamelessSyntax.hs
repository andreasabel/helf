-- stuff taken from HerBruijnVal

module LocallyNamelessSyntax where

import Data.Map (Map)
import qualified Data.Map as Map

import Abstract as A
import Value

-- * de Bruijn Terms

data BTm -- names are only used for quoting
  = B Int A.Name  -- bound variable: de Bruijn index
  | BVar A.Name   -- free variable: name
  | BCon A.Name
  | BDef A.Name
  | BApp BTm BTm
  | BLam A.Name BTm  -- name irrelevant for execution
  | BConstLam BTm -- these are lambdas, but not counted
  | BSort Value.Sort
  | BPi BTm BTm


-- * transformation, only used for evaluate2

-- maybe this should be transferred to Util.hs
data (Ord name) => LocBoundList name = LBL {lblsize :: Int, bList :: (Map name Int)}
lbl_empty = LBL 0 (Map.empty)

insert_lbl :: (Ord name) => name -> LocBoundList name -> LocBoundList name
insert_lbl n (LBL k m) = LBL (k+1) (Map.insert n k m) -- note that it does NOT matter whether or not n already had been a key before!

-- fakeinsert :: (Ord name) => LocBoundList name -> LocBoundList name
-- fakeinsert (LBL k m) = LBL k+1 m

lookup_lbl :: (Ord name) => name -> LocBoundList name -> Maybe Int
lookup_lbl x (LBL _ m)= Map.lookup x m


transform :: A.Expr -> BTm
transform = trans lbl_empty where
  trans :: LocBoundList A.Name -> A.Expr -> BTm
  trans lbl (Ident ident) = case ident of
        Var x -> case lookup_lbl x lbl of 
                        Just k  -> B (lblsize lbl - 1 - k) x
                        Nothing -> BVar x
        Con x -> BCon x
        Def x -> BDef x
  trans lbl (App e1 e2) = BApp (trans lbl e1) (trans lbl e2)
  trans lbl (Lam name _ e) = BLam name $ trans (insert_lbl name lbl) e
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



