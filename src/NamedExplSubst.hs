{- | Values implemented named terms with explicit substitutions.
-}

{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses,
    OverlappingInstances, IncoherentInstances, UndecidableInstances,
    PatternGuards, TupleSections #-}

module NamedExplSubst where

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
import Fresh

-- * Values

type Var = A.UID

-- | Heads are identifiers excluding @A.Def@.
type Head = A.Ident

data Val
  = Ne   Head       Val [Val]        -- ^ @x^a vs^-1 | c^a vs^-1@
  | Df   A.Name Val Val [Val]        -- ^ @d=v^a vs^-1@  a,v are closed!
  | App  Val [Val]                   -- ^ @v vs^-1@ non-canonical
                                     --   last argument first in list!
  | Sort Sort                        -- ^ @s@
  | K    Val                         -- ^ constant function
  | Abs  A.Name Val                  -- ^ @\xv@ abstraction
  | Fun  Val  Val                    -- ^ @Pi a b@
  | Clos Val  Env                    -- ^ @v[rho]@
  | DontCare

-- * Environments (Values for expression (=bound) variables)

type Env = Env.Env Var Val

-- * Application, Substitution

-- | @app f v@  computes the whnf of @f v@ without expanding definitions
app :: Val -> Val -> Val
app f v =
  case f of
    K w                  -> w
    Ne h t vs            -> Ne h t (v:vs)
    Df h w t vs          -> Df h w t (v:vs)
    App w ws             -> rapps w (v:ws) -- evaluate apps
    Abs x w              -> substFree v (A.uid x) w
    Clos (Abs x w) sigma -> substs (Env.update sigma (A.uid x) v) w
    Clos w sigma         -> substs sigma w `app` v

rapps :: Val -> [Val] -> Val
rapps f vs = foldr (flip app) f vs

-- | @substFree v x w = [v/x]w@ single substitution
substFree :: Val -> Var -> Val -> Val
substFree w x = substs (Env.singleton x w)

-- | parallel substitution, computing whnf
substs :: Env -> Val -> Val
substs sigma = subst where
  subst (Ne h@(A.Var y) a vs) = case Env.lookup (A.uid y) sigma of
    Just w  -> rapps w $ map subst vs
    Nothing -> Ne h (subst a) $ map subst vs
  subst (Ne h a vs)    = Ne h a $ map subst vs    -- a is closed
  subst (Df h v a vs)  = Df h v a $ map subst vs  -- a,v are closed
  subst (App v vs)     = rapps (subst v) (map subst vs)
    -- subst (rapps v vs) -- OR: first compute application ?
  subst (Sort s)       = Sort s
  subst (K v)          = K $ subst v
  subst (Abs x v)      = Clos (Abs x v) sigma
  subst (Clos v tau)   = flip substs v $ flip Env.union sigma $ Env.map subst tau
    -- composing two substitutions (first tau, then sigma) :
    --   apply sigma to tau
    --   add all bindings from sigma that are not yet present
    --   thus, we can take sigma and overwrite it with [sigma]tau
  subst (Fun a b)      = Fun (subst a) $ subst b

-- | computing the whnf of a term, pushing a delayed substitution in
whnf :: Val -> Val
whnf (Clos v rho) = substs rho v
whnf (App f vs)   = rapps f vs
whnf v            = v


-- * Smart Constructors for values.

var :: A.Name -> Val -> Val
var x t = Ne (A.Var x) t []

var_ :: A.Name -> Val
var_ x = var x DontCare

con :: A.Name -> Val -> Val
con x t = Ne (A.Con x) t []

def :: A.Name -> Val -> Val -> Val
def x v t = Df x v t []

-- non-computing application
application :: Val -> Val -> Val
application f v =
  case f of
    Ne h   t vs -> Ne h   t (v:vs)
    Df x w t vs -> Df x w t (v:vs)
    App  w   vs -> App  w   (v:vs)
    K w         -> w               -- because K comes from non-dep fun types
    _           -> App f [v]

-- * projections

boundName :: Val -> A.Name
boundName (Abs n _) = n
boundName _ = A.noName

-- * Translation

-- | @translate e rho = v@ where @rho@ is a renaming.
translate :: (Applicative m, Monad m, Signature Val sig, MonadReader sig m, MonadFresh m) =>
  A.Expr -> Renaming -> m Val
translate e rho =
    case e of
      A.Ident (A.Con x) -> con x . symbType . sigLookup' (A.uid x) <$> ask
      A.Ident (A.Def x) -> do
        SigDef t v <- sigLookup' (A.uid x) <$> ask
        return $ def x v t
      A.Ident (A.Var x) -> return $ var_ $ Env.lookupSafe (A.uid x) rho
      A.App f e    -> application <$> (evaluate f rho) <*> (evaluate e rho)
      A.Lam x mt e -> do y <- fresh x
                         Abs y <$> translate e (Env.update rho (A.uid x) y)
      A.Pi mx e e' -> Fun <$> (evaluate e rho) <*> case mx of
                        Just x  -> do
                          y <- fresh x
                          Abs y <$> translate e (Env.update rho (A.uid x) y)
                        Nothing -> K <$> evaluate e' rho
      A.Typ        -> typ

-- * Evaluation monad

instance (Applicative m, Monad m, Signature Val sig, MonadReader sig m, MonadFresh m) => MonadEval Head Val Renaming m where

  typ  = return $ Sort Type
  kind = return $ Sort Kind
  freeVar h t = return $ Ne h t []

  valView v = return $
    case (whnf v) of
      Fun a b -> VPi a b
      Sort s  -> VSort s
      Ne h       t vs -> VNe  h t (reverse vs)
      Df x v t vs     -> VDef (A.Def x) t (reverse vs)
      _               -> VAbs

  apply f v = return $ app f v

  evaluate e rho = error "NYI: NamedExplSubst.evaluate"
--  evaluate e rho = whnf <$> translate e rho

  evaluate' e = whnf <$> (translate e =<< renaming)

  unfold v =
    case v of
      Df x f t vs  -> appsR f vs
      _            -> return v

  unfolds v =
    case v of
      Df x f t vs  -> unfolds =<< appsR f vs -- unfolding application
      _            -> return v

  abstractPi a (n, Ne (A.Var x) _ []) b = return $ Fun a $ Abs x b

  reify v = quote v

-- * Reification

-- quote :: Val -> A.SysNameCounter -> EvalM A.Expr
quote :: (Applicative m, Monad m, MonadFresh m, MonadEval Head Val Renaming m) =>
         Val -> m A.Expr
quote v =
  case v of
    Ne h a vs    -> foldr (flip A.App) (A.Ident h) <$> mapM quote vs
    Df x f a vs  -> foldr (flip A.App) (A.Ident (A.Def x)) <$> mapM quote vs
    App f vs      -> foldr (flip A.App) <$> quote f <*> mapM quote vs
    Sort Type    -> return A.Typ
    Sort Kind    -> error "cannot quote sort kind"
    DontCare     -> error "cannot quote the dontcare value"
    Fun a (K b)  -> A.Pi Nothing <$> quote a <*> quote b
    Fun a f      -> do
      u     <- quote a
      (x,t) <- quoteFun f
      return $ A.Pi (Just x) u t
    f            -> do
      (x,e) <- quoteFun f
      return $ A.Lam x Nothing e

-- | @quoteFun n v@ expects @v@ to be a function and returns and its
--   body as an expression.
-- quoteFun :: Val -> A.SysNameCounter -> EvalM (A.Name, A.Expr)
quoteFun :: (Applicative m, Monad m, MonadFresh m, MonadEval Head Val Renaming m) =>
            Val -> m (A.Name, A.Expr)
quoteFun f = do
  x <- fresh $ boundName f
  v <- f `apply` (var_ x)
  (x,) <$> quote v
