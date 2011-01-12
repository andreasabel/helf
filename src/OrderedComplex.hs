module OrderedComplex where

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

----------------------------------------------

-- * ordered terms

data OTm -- names are only used for quoting
  = O
  | OVar A.Name
  | OCon A.Name
  | ODef A.Name
  | OApp OTm Int OTm
  | OLam (Maybe A.Name) [Int] (Maybe OTm) OTm -- maybe type annotation
  | OSort Value.Sort
  | OPi OTm Int OTm -- OPi t k (OLam ...)

-- * values

data Val
  = HVar A.Name Val [Val]       -- }
  | HCon A.Name Val [Val]       -- }-> Head
  | HDef A.Name Val Val [Val]   -- }
  | CLam (Maybe A.Name) [Int] OTm Env OSubst
  | Abs A.Name Val Env
  | Sort Value.Sort
  | Fun Val Val
  | DontCare

-- * ordered substitutions and environments

type OSubst = DatastrucImpl.SimpleDynArray.DynArray Val
type Env = Map A.UID Val

-- * 

instance Value A.Name Val where
  typ  = Sort Type 
  kind = Sort Kind
  freeVar = var

  valView v =
    case v of
      HVar x t vs         -> VNe x t (reverse vs)
      HCon x t vs         -> VNe x t (reverse vs)
      HDef x v t vs       -> VDef x t (reverse vs)
      CLam _ _ _ _ _      -> VAbs
      Abs _ _ _           -> VAbs
      Sort s              -> VSort s
      Fun a b             -> VPi a b
      -- DontCare         -> error "Cannot view DontCare Value"

-- * smart constructors

var :: A.Name -> Val -> Val
var x t = HVar x t []

var_ :: A.Name -> Val
var_ x = var x DontCare

con :: A.Name -> Val -> Val
con x t = HCon x t []

def :: A.Name -> Val -> Val -> Val
def x v t = HDef x v t []

-- * environment handling

emptyEnv = Map.empty
updateEnv env x v = Map.insert x v env
lookupEnv :: A.UID -> Env -> Maybe Val
lookupEnv = Map.lookup

-- * evaluation

instance (Applicative m, Monad m, Signature Val sig, MonadReader sig m) => MonadEval Val OSubst m where

  -- apply :: Val -> Val -> Val
  apply f w =
    case f of
      HVar x t vs     -> return $ HVar x t (w:vs)
      HCon x t vs     -> return $ HCon x t (w:vs)
      HDef x v t vs   -> return $ HDef x v t (w:vs)
      --CLam mname ks t env osubst  -> evalTerm 










evalTerm :: (Applicative m, Monad m, Signature Val sig, MonadReader sig m) => OTm -> Env -> OSubst -> m Val
evalTerm t env osubst =
  case t of
    O                       -> return $ DS.get osubst 0 
            -- if size osubst == 1 then DS.get osubst 0 else error "wrong size of osubst when evaluating a single O"
    OVar x                  -> return $ lookupSafe (uid x) env
    OCon x                  -> con x . symbType . sigLookup' (A.uid x) <$> ask
    ODef x                  -> do
                                SigDef t v <- sigLookup' (uid x) <$> ask
                                return $ def x v t
    
    
    
{-    
data OTm -- names are only used for quoting
  = O
  | OVar A.Name
  | OCon A.Name
  | ODef A.Name
  | OApp OTm Int OTm
  | OLam (Maybe A.Name) [Int] (Maybe OTm) OTm -- maybe type annotation
  | OSort Value.Sort
  | OPi OTm Int OTm -- OPi t k (OLam ...)
-}