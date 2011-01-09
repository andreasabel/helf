{-# LANGUAGE OverlappingInstances, IncoherentInstances, UndecidableInstances,
    PatternGuards, TupleSections #-}
    
module OrderedSimple where

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
  | OLam (Maybe A.Name) [Int] (Maybe OTm) OTm -- maybe type annotation; here: \y.xyz <> \[1].ooo instead of \[1,1].ooo
  | OSort Value.Sort
  | OPi OTm Int OTm -- OPi t k (OLam ...)
  

-- * values

data Val
  = HVar A.Name Val [Val]       -- }
  | HCon A.Name Val [Val]       -- }-> Head
  | HDef A.Name Val Val [Val]   -- }
  | CLam (Maybe A.Name) [Int] OTm OSubst
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
  freeVar x t = HVar x t []

  valView v =
    case v of
      HVar x t vs         -> VNe x t (reverse vs)
      HCon x t vs         -> VNe x t (reverse vs)
      HDef x v t vs       -> VDef x t (reverse vs)
      CLam _ _ _ _        -> VAbs
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
      HVar x t vs             -> return $ HVar x t (w:vs)
      HCon x t vs             -> return $ HCon x t (w:vs)
      HDef x v t vs           -> return $ HDef x v t (w:vs)
      CLam mname ks t osubst  -> eval' t (DS.multiinsert w ks osubst)
      Abs x v env             -> subst (updateEnv env (A.uid x) v) w
      Sort _                  -> fail $ "Cannot apply a Sort to anything"
      Fun a b                 -> apply b w -- probably not correct and even if, w :: a should be checked first ?!
      DontCare                -> fail $ "Cannot apply DontCare to anything"

  evaluate expr = eval' (transform expr)
  
  evaluate' expr = evaluate expr DS.empty
  
  unfold v = 
    case v of
      HDef x f t vs -> appsR f vs
      _             -> return v


{-
                          currently missing: 
  
  unfolds   :: val -> m val         -- ^ unfold definition until constructor
  abstractPi:: val -> (Name, val) -> val -> m val -- ^ abstractPi a x b = pi x:a.b
  reify     :: val -> m Expr        -- ^ quote value as expression
-}


-- eval' :: OTm -> OSubst -> m Val -- (Applicative m, Monad m, Signature Val sig, MonadReader sig m) => OTm -> OSubst -> m Val
-- eval' :: OTm -> OSubst -> m Val
eval' :: (Applicative m, Monad m, Signature Val sig, MonadReader sig m) => OTm -> OSubst -> m Val
eval' t osubst = case t of
  O | size osubst == 1    -> return $ DS.get osubst 0
  O                       -> fail $ "tried to evaluate O with more or less than one substitution"
  OVar _                  -> fail $ "found a free variable in an OTm"
  OCon x                  -> con x . symbType . sigLookup' (A.uid x) <$> ask
  ODef x                  -> -- ((symbType, symbDef) . sigLookup' (A.uid x) <$> ask) >>= \ t d -> return $ def x d t
                            do
                              SigDef t v <- sigLookup' (A.uid x) <$> ask
                              return $ def x v t
  OApp t1 k t2            -> let (osubst1, osubst2) = DS.split (size osubst - k) osubst
                             in Util.appM2 apply (eval' t1 osubst1) (eval' t2 osubst2)
  OLam mname ks mty t     -> return $ CLam mname ks t osubst -- todo: the information mty is wasted ?!
  OSort s                 -> return $ Sort s  -- When I forgot the '$' sign, a _very_ strange error message occured... (*)
  OPi t1 k t2             -> let (osubst1, osubst2) = DS.split (size osubst - k) osubst
                             in do
                                a <- eval' t1 osubst1
                                b <- eval' t2 osubst2
                                return $ Fun a b


subst :: (Applicative m, Monad m, MonadEval Val OSubst m) => Env -> Val -> m Val
subst env v = case v of
  HVar x t vs             -> case lookupEnv (A.uid x) env of
                              Just a  -> appsR a =<< (mapM (subst env) vs)
                              Nothing -> HVar x <$> (subst env t) <*> mapM (subst env) vs
  HCon c t vs             -> HCon c t <$> mapM (subst env) vs
  HDef d w t vs           -> HDef d w t <$> mapM (subst env) vs
  CLam mname ks t osubst  -> CLam mname ks t <$> mapMonad (subst env) osubst
  Abs x w env'            -> Abs x w . flip Map.union env <$> mapM (subst env) env'
      -- composing two substitutions is done in the same way as it is done in ClosVal
  Sort s                  -> return $ Sort s
  Fun a b                 -> Fun <$> subst env a <*> subst env b
  DontCare                -> return DontCare


-- test.
-- instance Signature Val Sort where
-- the above "fixed" the problem produced at (*)



-- * transforming A.Expr to OTm

data (Ord name) => LocBoundList name = LBL {lblsize :: Int, bList :: (Map name Int)}
lbl_empty = LBL 0 (Map.empty)

insert_lbl :: (Ord name) => name -> LocBoundList name -> LocBoundList name
insert_lbl n (LBL k m) = LBL (k+1) (Map.insert n k m) -- note that it does NOT matter whether or not n already had been a key before!

type LambdaLists = [[Int]]
incrKaddZero :: Int -> LambdaLists -> LambdaLists
incrKaddZero 0 (l:ll) = (0:l):ll
incrKaddZero (k+1) ((i:l):ll) = ((i+1):l) : incrKaddZero k ll

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
        Nothing -> fail $ "variable " ++ show x ++ " is not bound" -- or OVar ?
    Con x -> return (0, OCon x)
    Def x -> return (0, ODef x)  
  trans (App e1 e2) = do
    (i1, t1) <- trans e1
    (i2, t2) <- trans e2
    return (i1 + i2, OApp t1 i2 t2)
  trans (Lam x mty e) = 
    let moty = case mty of
                Just ty -> Just $ transform ty -- can ty depend on bound variables? e.g., is \x:A. \y:B(x). xy possible?
                Nothing -> Nothing
    in
    do 
    modify ((:) [0]) 
    (i, t)    <- local (insert_lbl x) $ trans e
    (l':ll')  <- Control.Monad.State.get    --
    put $ ll'                               -- these lines are the same as: modify (tail)
    return (i + 1 - (length l'), OLam (Just x) (reverse $ tail l') moty t) 
  trans (Pi name ty1 ty2) = case name of
    Just n -> do
      (i1, oty1) <- trans ty1
      (i2, oty2) <- trans $ Lam n Nothing ty2 -- Nothing?
      return (i1+i2, OPi oty1 i2 oty2)
    Nothing -> do
      (i1, oty1) <- trans ty1
      (i2, oty2) <- trans ty2
      return (i1+i2, OPi oty1 i2 $ OLam Nothing [] Nothing oty2)
  trans A.Typ = return (0, OSort Type)

