{-# LANGUAGE UndecidableInstances, TupleSections #-}

module OrderedComplex2 where

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
-- import DatastrucImpl.SimplDynArray (DynArray)
import DatastrucImpl.StrictDynArray (DynArray)
import DatastrucImpl.List (List)
-- import DatastrucImpl.DynArrayInstance

----------------------------------------------

-- * ordered terms

type Head = A.Name

data OTm -- names are only used for quoting
  = O
  | OVar A.Name
  | OCon A.Name
  | ODef A.Name
  | OApp OTm Int OTm
  | OLam (Maybe A.Name) [Int] OTm
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

-- type OSubst = DatastrucImpl.SimpleDynArray.DynArray Val
-- type OSubst = DatastrucImpl.StrictDynArray.DynArray Val
type OSubst = [Val]
type Env = Map A.UID Val

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
-- updateEnv :: Env -> UID -> Val -> Env
updateEnv env x v = Map.insert x v env
sgEnv v x = Map.singleton x v
lookupEnv :: A.UID -> Env -> Maybe Val
lookupEnv = Map.lookup

{-
emptyE = Map.empty
updateE env x v = Map.insert x v env
sgE v x = Map.singleton x v
lookupE :: UID -> Env -> Maybe Val
lookupE = Map.lookup
-}

emptyOSubst :: OSubst
emptyOSubst = DS.empty
updateSubst :: OSubst -> Int -> Val-> OSubst
updateSubst osubst k v = DS.insert v k osubst

-- * evaluation

instance (Applicative m, Monad m, Signature Val sig, MonadReader sig m) => MonadEval Head Val Env m where

  typ  = return $ Sort Type
  kind = return $ Sort Kind
  freeVar h t = return $ var h t

  valView v = return $
    case v of
      HVar x t vs         -> VNe x t (reverse vs)
      HCon x t vs         -> VNe x t (reverse vs)
      HDef x v t vs       -> VDef x t (reverse vs)
      CLam _ _ _ _ _      -> VAbs
      Abs _ _ _           -> VAbs
      Sort s              -> VSort s
      Fun a b             -> VPi a b
      -- DontCare         -> error "Cannot view DontCare Value"

  -- apply :: Val -> Val -> m Val
  apply f w =
    case f of
      HVar x t vs                 -> return $ HVar x t (w:vs)
      HCon x t vs                 -> return $ HCon x t (w:vs)
      HDef x v t vs               -> return $ HDef x v t (w:vs)
      CLam mname ks t env osubst  -> evalTerm t env (DS.multiinsert w ks osubst)
      Abs x v env                 -> -- todo: (y, fx) \in env ==> (y, fw) ? no!
                                     -- let xid = uid x
                                     -- in substs (updateEnv (mapM (substs (sgEnv w xid)) env) xid w) v
                                     substs (updateEnv env (uid x) w) v


  -- evaluate  :: Expr -> Env -> m val
  evaluate expr e =
    let t = transform expr
    in evalTerm t e emptyOSubst

  evaluate' = flip evaluate emptyEnv


  abstractPi a (_, HVar x _ []) b = return $ Fun a $ Abs x b emptyEnv
  abstractPi _ _ _                = fail $ "can only abstract a free variable"

  unfold v =
    case v of
      HDef d f t vs   -> appsR f vs
      _               -> return v

  unfolds v =
    case v of
      HDef d f t vs   -> unfolds =<< appsR' f vs
      _               -> return v

  -- reify :: Val -> m Expr
  -- reify v = return $ A.Ident $ A.Con $ Name 0 $ "NYI: reify"
  reify v = quote v A.initSysNameCounter


evalTerm :: (Applicative m, Monad m, Signature Val sig, MonadReader sig m) => OTm -> Env -> OSubst -> m Val
evalTerm t env osubst =
  case t of
    O                       -> return $ DS.get osubst 0
            -- if size osubst == 1 then DS.get osubst 0 else fail "wrong size of osubst when evaluating a single O"
    OVar x                  -> return $ lookupSafe (uid x) env
    OCon x                  -> con x . symbType . sigLookup' (A.uid x) <$> ask
    ODef x                  -> do
                                SigDef t v <- sigLookup' (uid x) <$> ask
                                return $ def x v t
    OApp t1 k t2            -> let
                                 (osubst1, osubst2) = DS.split (size osubst - k) osubst
                               in
                                 Util.appM2 apply (evalTerm t1 env osubst1) (evalTerm t2 env osubst2)
    OLam mname ks t         -> return $ CLam mname ks t env osubst
    OSort s		              -> return $ Sort s
    OPi t1 k t2		          -> let (osubst1, osubst2) = DS.split (size osubst - k) osubst
                               in do
                                 a <- evalTerm t1 env osubst1
                                 b <- evalTerm t2 env osubst2
                                 return $ Fun a b

substs :: (Applicative m, Monad m, MonadEval Head Val Env m) => Env -> Val -> m Val
substs env = subst where
  subst :: (Applicative m, Monad m, MonadEval Head Val Env m) => Val -> m Val
  subst value = case value of
    HVar x t vs                 -> case lookupEnv (uid x) env of
                                      Just w  -> appsR w =<< mapM subst vs
                                      Nothing -> HVar x <$> subst t <*> mapM subst vs
    HCon c t vs                 -> HCon c t <$> mapM subst vs
    HDef d v t vs               -> HDef d v t <$> mapM subst vs
    CLam mname ks t env' osubst -> (CLam mname ks t) <$> (flip Map.union env <$> mapM subst env') <*> (mapMonad subst osubst)
    Abs x v env'                -> Abs x v <$> (flip Map.union env <$> mapM subst env')
    Sort s                      -> return $ Sort s
    Fun a b                     -> Fun <$> subst a <*> subst b


-- * quoting (reify)

quote :: (Applicative m, Monad m, MonadEval Head Val Env m) => Val -> A.SysNameCounter -> m A.Expr
quote v i =
  case v of
    HVar name _ vs                -> foldr (flip App) (Ident $ Var name) <$> mapM (flip quote i) vs
    HCon name _ vs                -> foldr (flip App) (Ident $ Con name) <$> mapM (flip quote i) vs
    HDef name _ _ vs              -> foldr (flip App) (Ident $ Def name) <$> mapM (flip quote i) vs
    -- CLam mname ks otm env osubst  -> see below
    -- Abs name v env                -> see below
    Sort Type                     -> return Typ
    Sort Kind                     -> error "cannot quote sort kind"
    Fun a b                       -> do
                                      u     <- quote a i
                                      (x,t) <- quoteFun b i
                                      return $ Pi (Just x) u t
    DontCare                      -> error "cannot quote dontcare"
    _                             -> do
                                      (x,e) <- quoteFun v i
                                      return $ Lam x Nothing e

-- | @quoteFun n v@ expects @v@ to be a function and returns and its body as an expression.
quoteFun :: (Applicative m, Monad m, MonadEval Head Val Env m) => Val -> A.SysNameCounter -> m (A.Name, A.Expr)
quoteFun f i = do
  let n = case f of
          CLam (Just name) _ _ _ _ -> name
          Abs name _ _             -> name
          _                        -> A.noName
  let (x, i') = A.nextSysName i n
  v <- f `apply` (var_ x)
  (x,) <$> quote v i'


-- * transforming A.Expr to OTm

data LocBoundList name = LBL {lblsize :: Int, bList :: (Map name Int)}
lbl_empty = LBL 0 (Map.empty)

insert_lbl :: (Ord name) => name -> LocBoundList name -> LocBoundList name
insert_lbl n (LBL k m) = LBL (k+1) (Map.insert n k m) -- note that it does NOT matter whether or not n already had been a key before!

type LambdaLists = [[Int]]
incrKaddZero :: Int -> LambdaLists -> LambdaLists
incrKaddZero 0 (l:ll) = (0:l):ll
incrKaddZero k ((i:l):ll) = ((i+1):l) : incrKaddZero (k-1) ll

--type Transform a = LocBoundList Name -> LambdaLists -> (a, LambdaLists)
type Transform = ReaderT (LocBoundList Name) (State LambdaLists)

transform :: A.Expr -> OTm
transform e = snd $ trans e `runReaderT` lbl_empty `evalState` [] where
  trans :: A.Expr -> Transform (Int, OTm)
  trans (Ident ident) = case ident of
    Var x -> do
      lbl <- ask
      case Map.lookup x (bList lbl) of
        Just k -> do
          modify $ incrKaddZero (lblsize lbl - 1 - k)
          return (1, O)
        Nothing -> return (0, OVar x) -- fail $ "variable " ++ show x ++ " is not bound"
    Con x -> return (0, OCon x)
    Def x -> return (0, ODef x)
  trans (App e1 e2) = do
    (i1, t1) <- trans e1
    (i2, t2) <- trans e2
    return (i1 + i2, OApp t1 i2 t2)
  trans (Lam x mty e) =
    do
    modify ((:) [0])
    (i, t)    <- local (insert_lbl x) $ trans e
    (l':ll')  <- Control.Monad.State.get    --
    put $ ll'                               -- these lines are the same as: modify (tail)
    return (i + 1 - (length l'), OLam (Just x) (reverse $ tail l') t)
  trans (Pi mname ty1 ty2) = case mname of
    Just n -> do
      (i1, oty1) <- trans ty1
      (i2, oty2) <- trans $ Lam n Nothing ty2
      return (i1+i2, OPi oty1 i2 oty2)
    Nothing -> do
      (i1, oty1) <- trans ty1
      (i2, oty2) <- trans ty2
      return (i1+i2, OPi oty1 i2 $ OLam Nothing [] oty2)
  trans A.Typ = return (0, OSort Type)



-- * supporting unfolds

appsR' :: (Applicative m, Monad m, MonadEval Head Val Env m) => Val -> [Val] -> m Val
appsR' f vs = foldr (\ v mf -> mf >>= \ f -> apply' f v) (return f) vs

apply' :: (Applicative m, Monad m, MonadEval Head Val Env m) => Val -> Val -> m Val
apply' f v =
    case f of
      HDef d w t []   -> apply' w v
      HDef d w t ws   -> appsR' w (v:ws)
      _               -> apply f v
