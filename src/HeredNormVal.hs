{-# LANGUAGE OverlappingInstances, IncoherentInstances, UndecidableInstances,
    PatternGuards, TupleSections #-}

module HeredNormVal where

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


-- * beta-normal values

data NVal
  = NVar A.Name NVal [NVal]        -- }
  | NCon A.Name NVal [NVal]        -- }-> Head
  | NDef A.Name NVal NVal [NVal]   -- }
  | NLam A.Name NVal
  | NK NVal                        -- constant Lambda 
  | NSort Value.Sort
  | NFun NVal NVal
  | NDontCare

instance Value A.Name NVal where
  typ = NSort Type
  kind = NSort Kind
  freeVar = var
  valView v =
    case v of
      NVar x t vs         -> VNe x t (reverse vs)
      NCon x t vs         -> VNe x t (reverse vs)
      NDef x v t vs       -> VDef x t (reverse vs)
      NLam _ _            -> VAbs
      NK _                -> VAbs
      NSort s             -> VSort s
      NFun a b            -> VPi a b
      -- NDontCare        -> error "Cannot view DontCare Value"
 
 
-- * smart constructors

var :: A.Name -> NVal -> NVal
var x t = NVar x t []

var_ :: A.Name -> NVal
var_ x = var x NDontCare

con :: A.Name -> NVal -> NVal
con x t = NCon x t []

def :: A.Name -> NVal -> NVal -> NVal
def x v t = NDef x v t []


-- * environment handling
-- see MapEnv.hs
type Env' = Env UID NVal

lookupVal :: NVal -> Env' -> NVal
lookupVal v@(NVar x _ _) env = case lookup (uid x) env of
      Just w  -> w
      _       -> v

deleteFromEnv :: Env' -> A.UID -> Env'
deleteFromEnv env x = Map.delete x env
{-
type Env = Map A.UID NVal
emptyEnv = Map.empty
updateEnv :: Env -> A.UID -> NVal -> Env
updateEnv env x v = Map.insert x v env
sgEnv v x = Map.singleton x v
lookupEnv :: A.UID -> Env -> Maybe NVal
lookupEnv = Map.lookup
-}

-- * evaluation

instance (Applicative m, Monad m, Signature NVal sig, MonadReader sig m) => MonadEval NVal Env' m where

  -- apply :: NVal- NVal -> m NVal
  apply f w =
    case f of
      NVar x t vs                 -> return $ NVar x t (w:vs)
      NCon x t vs                 -> return $ NCon x t (w:vs)
      NDef x v t vs               -> return $ NDef x v t (w:vs)
      NLam x v                    -> subst v (singleton (uid x) w) 
      NK v                        -> return v
  
  -- evaluate :: Expr -> Env -> m NVal
  evaluate expr env =
    case expr of
      Ident ident     -> case ident of
                        Var x  -> return $ lookupVal (var_ x) env
                        Con x  -> con x . symbType . sigLookup' (A.uid x) <$> ask
                        Def x  -> do
                                    SigDef t v <- sigLookup' (uid x) <$> ask
                                    return $ def x v t
      Typ             -> return $ NSort Value.Type
      Pi mx a b       -> case mx of
                            Just n    -> do
                                          a' <- evaluate a env
                                          b' <- (\z -> return $ NLam n z) =<< (evaluate b $ deleteFromEnv env $ uid n)
                                          return $ NFun a' b'
                            Nothing   -> do
                                          a' <- evaluate a env
                                          b' <- (\z -> return $ NK z) =<< (evaluate b env)
                                          return $ NFun a' b'
      Lam x _ e       -> (\z -> return $ NLam x z) =<< (evaluate e $ deleteFromEnv env $ uid x)
      App e1 e2       -> Util.appM2 apply (evaluate e1 env) (evaluate e2 env) 
  
  -- evaluate' :: Expr -> m NVal
  evaluate' = flip evaluate empty

  abstractPi a (_, NVar x _ []) b = return $ NFun a $ NLam x b
  abstractPi _ _ _                = fail $ "can only abstract a free variable"

  unfold v = 
    case v of
      NDef d f t vs   -> appsR f vs
      _               -> return v

  unfolds v = 
    case v of
      NDef d f t vs   -> unfolds =<< appsR' f vs 
      _               -> return v

  reify v = fail $ "not implemented yet"

-- * substituation

subst :: (Applicative m, Monad m, Signature NVal sig, MonadReader sig m) => NVal -> Env' -> m NVal
subst nval env = case nval of
  NVar x t vs         -> case lookup (uid x) env of
                          Nothing -> (\z -> return $ NVar x t z) =<< (mapM (flip subst env) vs)
                          Just v  -> appsR v =<< mapM (flip subst env) vs
  NCon x t vs         -> (\z -> return $ NCon x t z) =<< mapM (flip subst env) vs
  NDef x v t vs       -> (\z -> return $ NDef x v t z) =<< mapM (flip subst env) vs
  NLam x v            -> (\z -> return $ NLam x z) =<< (subst v $ deleteFromEnv env $ uid x)    
                         -- care! x must not be in range(env), so this must be guaranteed! (however, if all names are unique anyway, 'deleteFromEnv' is not needed)
  NK v                -> (\z -> return $ NK z) =<< (subst v env)
  NFun a b            -> Util.appM2 (\s t -> return $ NFun s t) (subst a env) (subst b env)

  

  
-- * supporting unfolds

appsR' :: (Applicative m, Monad m, MonadEval NVal Env' m) => NVal -> [NVal] -> m NVal 
appsR' f vs = foldr (\ v mf -> mf >>= \ f -> apply' f v) (return f) vs

apply' :: (Applicative m, Monad m, MonadEval NVal Env' m) => NVal -> NVal -> m NVal
apply' f v =
    case f of
      NDef d w t []   -> apply' w v
      NDef d w t ws   -> appsR' w (v:ws)
      _               -> apply f v
      
