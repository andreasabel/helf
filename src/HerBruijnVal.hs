{-# LANGUAGE OverlappingInstances, IncoherentInstances, UndecidableInstances,
    PatternGuards, TupleSections #-}

module HerBruijnVal where

import Prelude hiding (pi,abs,mapM)

import Control.Monad.Reader hiding (mapM)
import Control.Applicative
import Control.Monad.Error hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.State hiding (mapM)

import Data.Traversable
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Abstract as A
--import Concrete (Name as CName)
import Value
import TypeCheck
-- import Context
import Signature
import Util
import Value

import DataStructure as DS
import DatastrucImpl.SimpleDynArray (DynArray)
import DatastrucImpl.List (List)
import DatastrucImpl.DynArrayInstance


-- * de Bruijn Terms

data BTm -- names are only used for quoting
  = B Int
  | BVar A.Name
  | BCon A.Name
  | BDef A.Name
  | BApp BTm BTm
  | BLam A.Name BTm
  | BConstLam BTm -- these are lambdas, but not counted
  | BSort Value.Sort
  | BPi BTm BTm

type Env = Map A.UID Val








-- * transformation

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
                        Just k  -> B (lblsize lbl - 1 - k)
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


