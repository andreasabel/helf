{-# LANGUAGE OverlappingInstances, IncoherentInstances, UndecidableInstances,
    PatternGuards, TupleSections #-}

module HerBruijnVal where

import Prelude hiding (pi,abs,mapM,lookup)

import Control.Monad.Reader hiding (mapM)
import Control.Applicative hiding (empty)
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
import Util hiding (lookupSafe)
import Value
import MapEnv as M hiding (mapM)


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


-- * beta normal values

data HVal
  = HBound Int  HVal [HVal]        -- }   (bound head variable)
  | HVar A.Name HVal [HVal]        -- }
  | HCon A.Name HVal [HVal]        -- }-> Head
  | HDef A.Name HVal HVal [HVal]   -- }
  | HLam A.Name HVal
  | HK HVal                        -- constant Lambda 
  | HSort Value.Sort
  | HFun HVal HVal
  | HDontCare

instance Value A.Name HVal where
  typ = HSort Type
  kind = HSort Kind
  freeVar = var
  valView v =
    case v of
      HVar x t vs         -> VNe x t (reverse vs)
      HCon x t vs         -> VNe x t (reverse vs)
      HDef x v t vs       -> VDef x t (reverse vs)
      HLam _ _            -> VAbs
      HK _                -> VAbs
      HSort s             -> VSort s
      HFun a b            -> VPi a b
      -- HDontCare        -> error "Cannot view DontCare Value"
      
      
-- * smart constructors

var :: A.Name -> HVal -> HVal
var x t = HVar x t []

var_ :: A.Name -> HVal
var_ x = var x HDontCare

con :: A.Name -> HVal -> HVal
con x t = HCon x t []

def :: A.Name -> HVal -> HVal -> HVal
def x v t = HDef x v t []


-- * environment handling
-- see MapEnv.hs
type Env' = Env UID HVal

lookupVal :: HVal -> Env' -> HVal
lookupVal v@(HVar x _ _) env = case lookup (uid x) env of
      Just w  -> w
      _       -> v


instance (Applicative m, Monad m, Signature HVal sig, MonadReader sig m) => MonadEval HVal Env' m where

  -- apply :: HVal- HVal -> m HVal
  apply f w =
    case f of
      HVar x t vs                 -> return $ HVar x t (w:vs)
      HCon x t vs                 -> return $ HCon x t (w:vs)
      HDef x v t vs               -> return $ HDef x v t (w:vs)
      --HLam x v                    -> subst v (singleton (uid x) w) 
      HK v                        -> return v

  -- evaluate :: Expr -> Env' -> m HVal
  evaluate expr env =
    let expr' = transform expr
    in evaluate' expr' env
    where
      -- evaluate' :: BTm -> Env' -> m HVal
      evaluate' btm env = case btm of
        B k         -> return $ HBound k HDontCare []
        BVar x      -> return $ lookupVal (var_ x) env
        BCon x      -> con x . symbType . sigLookup' (uid x) <$> ask
        BDef x      -> do
                      SigDef t v <- sigLookup' (uid x) <$> ask
                      return $ def x v t
        BApp t1 t2  -> Util.appM2 apply (evaluate' t1 env) (evaluate' t2 env)
        BLam x t    -> (\z -> return $ HLam x z) =<< (evaluate' t env)
        BConstLam t -> (\z -> return $ HK z) =<< (evaluate' t env)
        BSort sort  -> return $ HSort sort
        BPi a b     -> Util.appM2 (\a' b' -> return $ HFun a' b') (evaluate' a env) (evaluate' b env)
        
  -- evaluate' :: Expr -> m NVal
  evaluate' = flip evaluate empty

        











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


