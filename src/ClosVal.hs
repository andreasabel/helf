{- | Values implemented as closures.
--
-- We have two kinds of explicit substitutions:
-- * values for expression (bound) variables
-- * values for value (free) variables
-}

{-# LANGUAGE OverlappingInstances, IncoherentInstances, UndecidableInstances,
    PatternGuards, TupleSections #-}

module ClosVal where

import Prelude hiding (pi,abs,mapM)

import Control.Applicative
import Control.Monad.Reader hiding (mapM)

{-
import Data.Map (Map)
import qualified Data.Map as Map
-}
import Data.Traversable

import qualified Abstract as A
import qualified ListEnv as Env
-- import qualified MapEnv as Env
import Signature
import Util
import Value

-- * Values

type Var = A.UID

-- | Heads are identifiers excluding @A.Def@.
type Head = A.Ident

data Val
  = Ne   Head       Val [Val]        -- ^ @x^a vs^-1 | c^a vs^-1@
  | Df   A.Name Val Val [Val]        -- ^ @d=v^a vs^-1@
                                     --   last argument first in list!
  | Sort Sort                        -- ^ @s@
  | CLam A.Name A.Expr Env           -- ^ @(\xe) rho@
  | K    Val                         -- ^ constant function
  | Abs  A.Name Val Subst            -- ^ abstraction
  | Fun  Val  Val                    -- ^ @Pi a ((\xe)rho)@
  | DontCare

{-
instance Value Head Val where
  typ  = Sort Type
  kind = Sort Kind
  freeVar h t = Ne h t []

  valView v =
    case v of
      Fun a b -> VPi a b
      Sort s  -> VSort s
      Ne h       t vs -> VNe  h t (reverse vs)
--      Ne h@Def{} t vs -> VDef h t (reverse vs)
      Df x v t vs     -> VDef (A.Def x) t (reverse vs)
      _               -> VAbs
-}

-- * smart constructors

var :: A.Name -> Val -> Val
var x t = Ne (A.Var x) t []

var_ :: A.Name -> Val
var_ x = var x DontCare

con :: A.Name -> Val -> Val
con x t = Ne (A.Con x) t []

def :: A.Name -> Val -> Val -> Val
def x v t = Df x v t []

-- * projections

boundName :: Val -> A.Name
boundName (CLam n _ _) = n
boundName (Abs n _ _) = n
boundName _ = A.noName

-- * Environments (Values for expression (=bound) variables)

type Env = Env.Env Var Val

{-
update :: Env -> Var -> Val -> Env
update rho x e = Map.insert x e rho
-}

-- * Substitutions (Values for value (=free) variables)

type Subst = Env

emptySubst  = Env.empty
sgSubst     = flip Env.singleton
updateSubst = Env.update
lookupSubst = Env.lookup

-- * Evaluation

instance (Applicative m, Monad m, Signature Val sig, MonadReader sig m) =>
  MonadEval Head Val Env m where

  typ  = return $ Sort Type
  kind = return $ Sort Kind
  freeVar h t = return $ Ne h t []

  valView v = return $
    case v of
      Fun a b -> VPi a b
      Sort s  -> VSort s
      Ne h       t vs -> VNe  h t (reverse vs)
      Df x v t vs     -> VDef (A.Def x) t (reverse vs)
      _               -> VAbs

  apply f v =
    case f of
      K w               -> return $ w
      Ne h t vs         -> return $ Ne h t (v:vs)
      Df h w t vs       -> return $ Df h w t (v:vs)
      CLam x e rho      -> evaluate e (Env.update rho (A.uid x) v)
      Abs x w sigma     -> substs (updateSubst sigma (A.uid x) v) w

  evaluate e rho =
    case e of
      A.Ident (A.Con x) -> con x . symbType . sigLookup' (A.uid x) <$> ask
--      A.Ident (A.Def x) -> symbDef . sigLookup' x  <$> ask
      A.Ident (A.Def x) -> do
        SigDef t v <- sigLookup' (A.uid x) <$> ask
        return $ def x v t
      A.Ident (A.Var x) -> return $ Env.lookupSafe (A.uid x) rho
      A.App f e    -> Util.appM2 apply (evaluate f rho) (evaluate e rho)
      A.Lam x mt e -> return $ CLam x e rho
      A.Pi mx e e' -> Fun <$> (evaluate e rho) <*> case mx of
                        Just x  -> return $ CLam x e' rho
                        Nothing -> K <$> evaluate e' rho
      A.Typ        -> typ

  evaluate' e = evaluate e Env.empty

  unfold v =
    case v of
      Df x f t vs  -> appsR f vs
      _            -> return v

  unfolds v =
    case v of
      Df x f t vs  -> unfolds =<< appsR' f vs -- unfolding application
      _            -> return v

  abstractPi a (n, Ne (A.Var x) _ []) b = return $ Fun a $ Abs x b emptySubst

  reify v = quote v A.initSysNameCounter

-- * Substitution for free variables

-- | @substFree v x w = [v/x]w@
-- substFree :: Val -> Var -> Val -> EvalM Val
substFree :: (Applicative m, Monad m, MonadEval Head Val Env m) =>
             Val -> Var -> Val -> m Val
substFree w x = substs (sgSubst w x)

-- substs :: Subst -> Val -> EvalM Val
substs :: (Applicative m, Monad m, MonadEval Head Val Env m) =>
          Subst -> Val -> m Val
substs sigma = subst where
  subst :: (Applicative m, Monad m, MonadEval Head Val Env m) => Val -> m Val
  subst (Ne h@(A.Var y) a vs) = case lookupSubst (A.uid y) sigma of
    Just w  -> appsR w =<< mapM subst vs
    Nothing -> Ne h <$> subst a <*> mapM subst vs
  subst (Ne h a vs)    = Ne h a <$> mapM subst vs    -- a is closed
  subst (Df h v a vs)  = Df h v a <$> mapM subst vs  -- a,v are closed
  subst (Sort s)       = return (Sort s)
  subst (CLam y e rho) = CLam y e <$> Env.mapM subst rho
  subst (K v)          = K <$> subst v
  subst (Abs x v tau)  = Abs x v . flip Env.union sigma <$> Env.mapM subst tau
    -- composing two substitutions (first tau, then sigma) :
    --   apply sigma to tau
    --   add all bindings from sigma that are not yet present
    --   thus, we can take sigma and overwrite it with [sigma]tau
  subst (Fun a b)      = Fun <$> subst a <*> subst b

{-
apps :: Val -> [Val] -> EvalM Val
apps f vs = foldl (\ mf v -> mf >>= \ f -> apply f v) (return f) vs
-}
-- appsR :: Val -> [Val] -> EvalM Val
-- moved to Value.hs

-- * Unfolding definitions

-- | Unfold head definitions during applying.
-- appsR' :: Val -> [Val] -> EvalM Val
appsR' :: (Applicative m, Monad m, MonadEval Head Val Env m) => Val -> [Val] -> m Val
appsR' f vs = foldr (\ v mf -> mf >>= \ f -> apply' f v) (return f) vs

-- apply' :: Val -> Val -> EvalM Val
apply' :: (Applicative m, Monad m, MonadEval Head Val Env m) => Val -> Val -> m Val
apply' f v =
    case f of
      K w               -> return $ w
      Ne h t vs         -> return $ Ne h t (v:vs)
      Df h w t []       -> apply' w v
      Df h w t vs       -> appsR' w (v:vs)
      CLam x e rho      -> evaluate e (Env.update rho (A.uid x) v)
      Abs x w sigma     -> substs (updateSubst sigma (A.uid x) v) w

-- * Reification

-- quote :: Val -> A.SysNameCounter -> EvalM A.Expr
quote :: (Applicative m, Monad m, MonadEval Head Val Env m) =>
         Val -> A.SysNameCounter -> m A.Expr
quote v i =
  case v of
    Ne h a vs    -> foldr (flip A.App) (A.Ident h) <$> mapM (flip quote i) vs
    Df x f a vs  -> foldr (flip A.App) (A.Ident (A.Def x)) <$> mapM (flip quote i) vs
    Sort Type    -> return A.Typ
    Sort Kind    -> error "cannot quote sort kind"
    DontCare     -> error "cannot quote the dontcare value"
    Fun a (K b)  -> A.Pi Nothing <$> quote a i <*> quote b i
    Fun a f      -> do
      u     <- quote a i
      (x,t) <- quoteFun f i
      return $ A.Pi (Just x) u t
    f            -> do
      (x,e) <- quoteFun f i
      return $ A.Lam x Nothing e

-- | @quoteFun n v@ expects @v@ to be a function and returns and its
--   body as an expression.
-- quoteFun :: Val -> A.SysNameCounter -> EvalM (A.Name, A.Expr)
quoteFun :: (Applicative m, Monad m, MonadEval Head Val Env m) =>
            Val -> A.SysNameCounter -> m (A.Name, A.Expr)
quoteFun f i = do
  let n = boundName f
  let (x, i') = A.nextSysName i n
  v <- f `apply` (var_ x)
  (x,) <$> quote v i'
