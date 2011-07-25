{-# LANGUAGE TupleSections, UndecidableInstances #-}

module TermGraph where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Traversable as Trav

import Debug.Trace

import ORef
import qualified Abstract as A
import Signature
import Util
import Value

(<.>) :: Applicative m => (b -> c) -> (a -> m b) -> a -> m c
(<.>) f g x = f <$> g x  

{- Term graphs -}

-- | Heads are identifiers excluding @A.Def@.
type Head = A.Ident 

type Term = ORef Term'
type Type = Term
type Var  = Term
type Spine = [Term]

-- instance Show Term where
--   show _ = "*"

data Term' 
  = Atom  Head Type      Spine -- ^ typed free variable or constant
  | Def A.Name Type Term Spine -- ^ typed definition and its value
  | Var A.Name                 -- ^ bound variable
  | Abs Var  Term              -- ^ lambda abstraction
  | K   Term                   -- ^ constant function
  | Fun Type Type              -- ^ function type (second type a K or Abs)
  | Sort Sort
  -- not whnf:
  | App Term Spine             -- ^ application, not normal; spine not empty
  | DontCare                   -- ^ for unannotated free variables
    deriving Show

show1 :: MonadORef m => Term -> m String
show1 t = show <$> readORef t

-- * Predefined terms

data TGCxt = TGCxt { pType :: Term, pKind :: Term, pDontCare :: Term }

-- * Term graph monad

class MonadORef m => MonadTG m where
  predefType :: m Term
  predefKind :: m Term
  dontCare   :: m Term

newtype TGM a = TGM { runTGM :: ReaderT TGCxt ORefM a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadORef)

evalTGM :: TGM a -> ORefM a
evalTGM (TGM cont) = do
  dc <- newORef $ DontCare   -- prints as r0
  ty <- newORef $ Sort Type
  ki <- newORef $ Sort Kind
  runReaderT cont $ TGCxt { pType = ty, pKind = ki, pDontCare = dc }

instance MonadTG TGM where
  predefType = TGM $ asks pType
  predefKind = TGM $ asks pKind
  dontCare   = TGM $ asks pDontCare

instance MonadTG m => MonadTG (ReaderT r m) where
  predefType = lift $ predefType
  predefKind = lift $ predefKind
  dontCare   = lift $ dontCare

instance MonadTG m => MonadTG (StateT s m) where
  predefType = lift $ predefType
  predefKind = lift $ predefKind
  dontCare   = lift $ dontCare

instance (Error e, MonadTG m) => MonadTG (ErrorT e m) where
  predefType = lift $ predefType
  predefKind = lift $ predefKind
  dontCare   = lift $ dontCare

{-
instance MonadIO TGM where
  liftIO = TGM . liftIO

instance MonadORef TGM where
  newORef   a   = TGM $ newORef a
  readORef  r   = TGM $ readORef r
  writeORef r a = TGM $ writeORef r a
-}

-- * Value instance

type Env = Map A.Name Term

instance (Signature Term sig, MonadReader sig m, MonadTG m) => 
  MonadEval Head Term Env m where

  typ         = predefType
  kind        = predefKind
  freeVar h t = newORef $ Atom h t []

  valView r = do
    u <- whnf' r
    case u of
      Atom h t sp  -> return $ VNe h t sp
      Def h t v sp -> return $ VDef (A.Def h) t sp 
      Sort s       -> return $ VSort s
      Fun u t      -> return $ VPi u t
      K{}          -> return $ VAbs
      Abs{}        -> return $ VAbs
      _            -> fail $ "not a whnf " ++ show u

  evaluate e rho = trans rho e
  evaluate'      = trans Map.empty

  apply f r = app f [r]

  unfold r = do
    u <- whnf' r
    case u of
      Def h t v sp -> app v sp
      _            -> return r

  unfolds r = do
      u <- whnf' r 
      case u of
        Def h t v sp -> unfolds =<< app v sp
        _            -> return r

{-
  unfolds r = loop r =<< whnf r where
    loop r u =
      case u of
        Def h t v sp -> do
          u <- app v sp
          r <- newORef u
          loop r u
        _            -> return r
-}

  reify r = quote r A.initSysNameCounter 

  abstractPi a x b = error "TermGraph.abstractPi: NYI"

-- * Translation from expressions

type TDict = Map A.Expr Term
type TransM = StateT TDict ORefM
type TransT = StateT TDict

addPredefs :: MonadTG m => TDict -> m TDict
addPredefs dict = do
  ty <- predefType
  return $ Map.insert A.Typ ty dict

trans ::  (Signature Term sig, MonadReader sig m, MonadTG m) => 
  Env -> A.Expr -> m Term
trans rho e = -- trace ("translating " ++ show e) $ do
  evalStateT (transT e) =<< addPredefs 
                     (Map.mapKeysMonotonic (A.Ident . A.Var) rho)

transT ::  (Signature Term sig, MonadReader sig m, MonadTG m) => 
  A.Expr -> TransT m Term
transT e = do
  dict <- get
  case Map.lookup e dict of  -- TODO compare upto alpha!
    Just r  -> do
      -- traceM $ return ("found translation for " ++ show e)
      return r
    Nothing -> do
      r <- newORef =<< transT' e
      -- traceM $ return ("adding translation for " ++ show e) 
      put $ Map.insert e r dict
      return r

{-
con :: MonadORef m => A.Name -> Term -> m Term 
con x t = newORef $ Atom (A.Con x) t []

def :: MonadORef m => A.Name -> Term -> Term -> m Term
def x v t = newORef $ Def x v t []
-}
{-
var_ :: A.Name -> Term' 
var_ x = var x (error "var_: type annotation not given")
-}

var :: A.Name -> Type -> Term' 
var x t = Atom (A.Var x) t []

con :: A.Name -> Type -> Term' 
con x t = Atom (A.Con x) t []

def :: A.Name -> Type -> Term -> Term'
def x t v = Def x t v []

transT' ::  (Signature Term sig, MonadReader sig m, MonadTG m) => 
  A.Expr -> TransT m Term'
transT' e = do
  let (f, rsp) = A.revAppView e
  h <- case f of
         A.Ident (A.Con x) -> con x . symbType . sigLookup' (A.uid x) <$> ask
         A.Ident (A.Def x) -> do 
            SigDef t v <- sigLookup' (A.uid x) <$> ask
            return $ def x t v 
         A.Ident (A.Var n) -> 
--           fail ("transT': unbound variable " ++ A.suggestion n) 
--           trace ("transT': unbound variable " ++ A.suggestion n) 
           return $ Var n -- only for binding in Lam and Pi
         -- A.Typ             -> predefType  -- impossible case
         A.Lam n _ e       -> Abs <$> transT (A.Ident $ A.Var n) <*> transT e
         A.Pi Nothing  a b -> Fun <$> transT a <*> (newORef =<< K <$> transT b)
         A.Pi (Just n) a b -> Fun <$> transT a <*> transT (A.Lam n Nothing b)
  if null rsp then return h else do
    r <- newORef h
    App r <$> (mapM transT $ reverse rsp)

-- * Evaluation

-- Example: (\x. x x) (\y. y) --> x x [x := \y.y] --> y [y := clone(\y.y)]

type Whnf  = Term
type Whnf' = Term'

whnf :: MonadORef m => Term -> m Whnf
whnf r = do
  t <- readORef r
  case t of
    App u us -> assign r =<< app u us
    _        -> return r


whnf' :: MonadORef m => Term -> m Whnf'
whnf' r = do
  t <- readORef r
  case t of
    App u us -> writeORef r =<< readORef =<< app u us
    _        -> return t

app :: MonadORef m => Term -> Spine -> m Whnf
app r [] = whnf r
app r sp@(u:us) = do
  t <- whnf' r
  case t of
    Abs x t'      -> flip app us =<< subst t' x u
    K   t'        -> app t' us
    App x ts      -> newORef $ App x (ts ++ sp)
    Atom h ty ts  -> newORef $ Atom h ty (ts ++ sp)
    Def h ty v ts -> newORef $ Def h ty v (ts ++ sp)
    _             -> fail $ "cannot apply " ++ show t

{-
type Whnf' = Term'

whnf :: MonadORef m => Term -> m Whnf'
whnf r = do
  t <- readORef r
  case t of
    App u us -> writeORef r =<< app u us
    _        -> return t

app :: MonadORef m => Term -> Spine -> m Whnf'
app r []        = whnf r
app r sp@(u:us) = do
  t <- whnf r
  case t of
    Abs x t'      -> flip app us =<< subst t' x u
    K   t'        -> app t' us
    App x ts      -> return $ App x (ts ++ sp)
    Atom h ty ts  -> return $ Atom h ty (ts ++ sp)
    Def h ty v ts -> return $ Def h ty v (ts ++ sp)
    _             -> fail $ "cannot apply " ++ show t
      --return $ App r sp
-}

-- * Substituition

type Subst = Map Term (Maybe Term) -- Just = dirty, Nothing = clean

-- t[x<-u]
subst :: MonadORef m => Term -> Var -> Term -> m Term
subst t x u = do
  [st,sx,su] <- mapM show1 [t,x,u]
  traceM $ return ("substituting  " ++ su ++ " for " ++ sx ++ " in " ++ st)
  maybe (return t) return =<< (evalStateT (substT t) $ Map.singleton x (Just u))
  
type SubstM = StateT Subst ORefM
type SubstT = StateT Subst

substT :: MonadORef m => Term -> SubstT m (Maybe Term)
substT r = do
  dict <- get
  case Map.lookup r dict of
    Just (Just r') -> do
      s  <- show1 r 
      s' <- show1 r'
      traceM $ return ("fire subst of " ++ s' ++ " for " ++ s)
      return $ Just r'
    Just (Nothing) -> return Nothing
    Nothing  -> do
      mt <- substT' =<< readORef r
      mr <- Trav.mapM newORef mt
      -- mr <- maybe (return Nothing) (Just <.> newORef) mt 
      put $ Map.insert r mr dict
      return mr

-- | if no path applied the substitution, we are "clean" (Nothing)
--   and can keep the old value.
substT' :: MonadORef m => Term' -> SubstT m (Maybe Term')
substT' t = do
  case t of
    Abs x u -> do
      modify $ Map.insert x Nothing -- bound variable, do not change!
      (fmap $ Abs x) <$> substT u
    K   u   -> (fmap K) <$> substT u
    Fun u v -> do
      mu <- substT u
      mv <- substT v
      return $ maybeMap2 Fun mu mv u v
    App u us -> do
      mu  <- substT u
      mus <- maybeUpd us <$> mapM substT us
      return $ maybeMap2 App mu mus u us
      -- do not subst in atoms and constants:
    Atom h ty  sp -> (fmap $ Atom h ty)  <$> (maybeUpd sp <$> mapM substT sp)
    Def h ty v sp -> (fmap $ Def h ty v) <$> (maybeUpd sp <$> mapM substT sp)
    Sort{} -> return Nothing 
    Var n -> do
      dict <- get
      fail $ "substT': unbound variable " ++ show n ++ " keys: " ++ show (Map.keys dict)
    -- return Nothing -- bound variables: impossible case?

maybeMap2 :: (a -> b -> c) -> Maybe a -> Maybe b -> a -> b -> Maybe c
maybeMap2 f Nothing  Nothing  a b = Nothing
maybeMap2 f (Just a) Nothing  _ b = Just $ f a b
maybeMap2 f Nothing  (Just b) a _ = Just $ f a b
maybeMap2 f (Just a) (Just b) _ _ = Just $ f a b

maybeUpd :: [a] -> [Maybe a] -> Maybe [a]
maybeUpd as mas = if all isNothing mas then Nothing else Just $
  zipWith (flip maybe id) as mas



-- * Reification (TODO: memoizing!)

-- quote :: Val -> A.SysNameCounter -> EvalM A.Expr
quote :: (MonadEval Head Term Env m, MonadTG m) =>
         Term -> A.SysNameCounter -> m A.Expr
quote r i = do
  v <- readORef r
  case v of
    Var n        -> return $ A.Ident (A.Var n)
    App f vs     -> foldr (flip A.App) <$> quote f i <*> mapM (flip quote i) vs
    Atom h a vs  -> foldr (flip A.App) (A.Ident h) <$> mapM (flip quote i) vs
    Def x a f vs -> foldr (flip A.App) (A.Ident (A.Def x)) <$> mapM (flip quote i) vs
    Sort Type    -> return A.Typ
    Sort Kind    -> error "cannot quote sort kind"
--    DontCare     -> error "cannot quote the dontcare value"
--    Fun a (K b)  -> A.Pi Nothing <$> quote a i <*> quote b i
    Fun a f      -> do
      u     <- quote a i
      b     <- readORef f
      case b of
        K b' -> do
          t <- quote b' i
          return $ A.Pi Nothing u t
        _    -> do
          (x,t) <- quoteFun f i
          return $ A.Pi (Just x) u t
    f            -> do
      (x,e) <- quoteFun r i
      return $ A.Lam x Nothing e

-- | @quoteFun n v@ expects @v@ to be a function and returns and its
--   body as an expression.
-- quoteFun :: Val -> A.SysNameCounter -> EvalM (A.Name, A.Expr)
quoteFun :: (MonadEval Head Term Env m, MonadTG m) =>
            Term -> A.SysNameCounter -> m (A.Name, A.Expr)
quoteFun f i = do
  n <- boundName f
  let (x, i') = A.nextSysName i n
  vx <- newORef . var x =<< dontCare
  v  <- f `apply` vx
  (x,) <$> quote v i'

boundName f = do
  u <- readORef f
  case u of
    Abs x _ -> getVarName x
    _       -> return $ A.noName

getVarName x = do
  u <- readORef x
  case u of
    Var n -> return n
    _     -> return A.noName

{-
-- * Copying everything

clone :: Term -> ORefM Term
clone r = evalStateT (cloneT r) Map.empty 

type Dict = Map Term Term
type CloneM = StateT Dict ORefM

cloneT :: Term -> CloneM Term
cloneT r = do
  dict <- get
  case Map.lookup r dict of
    Just r  -> return r
    Nothing -> do
      r' <- newORef =<< cloneT' =<< readORef r
      put $ Map.insert r r' dict
      return r' 

cloneT' :: Term' -> CloneM Term'
cloneT' t = do
  case t of
    App u sp -> App <$> cloneT u <*> mapM cloneT sp
    Abs x u  -> Abs <$> cloneT x <*> cloneT u
    K   u    -> K   <$> cloneT u
    Fun u v  -> Fun <$> cloneT u <*> cloneT v
    _        -> return t
-}