-- A type checker instance with 
-- * values as explicit closures and 
-- * environments as finite maps

{-# LANGUAGE OverlappingInstances, IncoherentInstances, 
    PatternGuards, TupleSections #-}

module Closures where

import Prelude hiding (pi,abs,mapM)

import Control.Applicative
import Control.Monad ((<=<))
import Control.Monad.Error  hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.State  hiding (mapM)

import Data.Map (Map)
import qualified Data.Map as Map
-- import qualified Data.Maybe as Maybe
import Data.Traversable

import qualified Abstract as A
import Context
import PrettyM
-- import Scoping
-- import ScopeMonad
import Signature
import TypeCheck hiding (app)
import Util
import Value

import Data.Char -- testing
-- import Text.PrettyPrint

-- * Values

type Var = A.UID
{-
data Head 
  = HVar Var      -- (typed) variable 
  | HCon A.Name   -- (typed) constructor
  deriving (Eq,Ord,Show)
-}

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
{-
  tyView v =
    case v of
      Fun a b -> VPi a b
      Sort s  -> VSort s
      _       -> VBase
 
  tmView v =
    case v of
      Ne h       t vs -> VNe  h t (reverse vs)
--      Ne h@Def{} t vs -> VDef h t (reverse vs)
      Df x v t vs     -> VDef (A.Def x) t (reverse vs)
      _               -> VVal
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

type Env = Map Var Val

update :: Env -> Var -> Val -> Env
update rho x e = Map.insert x e rho 

-- * Substitutions (Values for value (=free) variables)

type Subst = Map Var Val

emptySubst = Map.empty
sgSubst v x = Map.singleton x v
updateSubst sigma x v = Map.insert x v sigma
lookupSubst = Map.lookup
-- updateSubsts = 

-- * Evaluation

type EvalM = Reader (MapSig Val)

-- | @substFree v x w = [v/x]w@
substFree :: Val -> Var -> Val -> EvalM Val
substFree w x = substs (sgSubst w x)
{-
substFree w x = subst where
  subst (Ne (HVar y) a vs) 
              | x == y = appsR w =<< mapM subst vs
  subst (Ne h a vs)    = Ne h <$> subst a <*> mapM subst vs
  subst (Sort s)       = return (Sort s)
  subst (CLam y e rho) = CLam y e <$> mapM subst rho
  subst (K v)          = K <$> subst v
  subst (Abs{})        = fail $ "substFree: giving up"
  subst (Fun a b)      = Fun <$> subst a <*> subst b
-}
substs :: Subst -> Val -> EvalM Val
substs sigma = subst where
  subst (Ne h@(A.Var y) a vs) = case lookupSubst (A.uid y) sigma of
    Just w  -> appsR w =<< mapM subst vs
    Nothing -> Ne h <$> subst a <*> mapM subst vs
  subst (Ne h a vs)    = Ne h a <$> mapM subst vs    -- a is closed
  subst (Df h v a vs)  = Df h v a <$> mapM subst vs  -- a,v are closed
  subst (Sort s)       = return (Sort s)
  subst (CLam y e rho) = CLam y e <$> mapM subst rho
  subst (K v)          = K <$> subst v
  subst (Abs x v tau)  = Abs x v . flip Map.union sigma <$> mapM subst tau
    -- composing two substitutions (first tau, then sigma) :
    --   apply sigma to tau
    --   add all bindings from sigma that are not yet present
    --   thus, we can take sigma and overwrite it with [sigma]tau
  subst (Fun a b)      = Fun <$> subst a <*> subst b

{-
apps :: Val -> [Val] -> EvalM Val
apps f vs = foldl (\ mf v -> mf >>= \ f -> apply f v) (return f) vs
-}
appsR :: Val -> [Val] -> EvalM Val
appsR f vs = foldr (\ v mf -> mf >>= \ f -> apply f v) (return f) vs

-- | Unfold head definitions during applying.
appsR' :: Val -> [Val] -> EvalM Val
appsR' f vs = foldr (\ v mf -> mf >>= \ f -> apply' f v) (return f) vs

apply' :: Val -> Val -> EvalM Val 
apply' f v =
    case f of
      K w               -> return $ w
      Ne h t vs         -> return $ Ne h t (v:vs)
      Df h w t []       -> apply' w v
      Df h w t vs       -> appsR' w (v:vs)
      CLam x e rho      -> evaluate e (update rho (A.uid x) v)
      Abs x w sigma     -> substs (updateSubst sigma (A.uid x) v) w
  
instance MonadEval Val Env EvalM where

  apply f v =
    case f of
      K w               -> return $ w
      Ne h t vs         -> return $ Ne h t (v:vs)
      Df h w t vs       -> return $ Df h w t (v:vs)
      CLam x e rho      -> evaluate e (update rho (A.uid x) v)
      Abs x w sigma     -> substs (updateSubst sigma (A.uid x) v) w
  
  evaluate e rho =
    case e of
      A.Ident (A.Con x) -> con x . symbType . sigLookup' (A.uid x) <$> ask
--      A.Ident (A.Def x) -> symbDef . sigLookup' x  <$> ask
      A.Ident (A.Def x) -> do 
        SigDef t v <- sigLookup' (A.uid x) <$> ask
        return $ def x v t 
      A.Ident (A.Var x) -> return $ lookupSafe (A.uid x) rho
      A.App f e    -> Util.appM2 apply (evaluate f rho) (evaluate e rho)
      A.Lam x mt e -> return $ CLam x e rho
      A.Pi mx e e' -> Fun <$> (evaluate e rho) <*> case mx of
                        Just x  -> return $ CLam x e' rho
                        Nothing -> K <$> evaluate e' rho 
      A.Typ        -> return typ

  evaluate' e = evaluate e Map.empty

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

-- * Reification

quote :: Val -> A.SysNameCounter -> EvalM A.Expr
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
quoteFun :: Val -> A.SysNameCounter -> EvalM (A.Name, A.Expr)
quoteFun f i = do
  let n = boundName f
  let (x, i') = A.nextSysName i n
  v <- f `apply` (var_ x)
  (x,) <$> quote v i'

instance PrettyM EvalM Val where
  prettyM = prettyM <=< reify

-- * Context monad

data Context = Context
  { level  :: Int
  , tyEnv  :: Env
  , valEnv :: Env
  }

emptyContext :: Context
emptyContext = Context 0 Map.empty Map.empty

type ContextM = Reader Context

instance MonadCxt Val Env ContextM where

  addLocal n@(A.Name x _) t cont = do
    l <- asks level
    let xv = Ne (A.Var $ n { A.uid = l }) t []
    local (\ (Context l gamma rho) -> 
             Context (l + 1) (Map.insert x t gamma) (Map.insert x xv rho)) 
          (cont xv)

  lookupLocal x = do 
    gamma <- asks tyEnv
    return $ lookupSafe (A.uid x) gamma

  getEnv = asks valEnv

-- * Type checking monad

data SigCxt = SigCxt { globals :: MapSig Val, locals :: Context }

type Err = Either String
type CheckExprM = ReaderT SigCxt Err

instance MonadCxt Val Env CheckExprM where

  addLocal n@(A.Name x _) t cont = do
    Context l gamma rho <- asks locals
    let xv  = Ne (A.Var $ n { A.uid = l }) t []
    let cxt = Context (l + 1) (Map.insert x t gamma) (Map.insert x xv rho) 
    local (\ sc -> sc { locals = cxt }) $ cont xv

  lookupLocal n@(A.Name x _) = do 
    gamma <- asks $ tyEnv . locals
    return $ lookupSafe x gamma

  getEnv = asks $ valEnv . locals

instance MonadCheckExpr Val Env EvalM CheckExprM where  

  doEval comp    = runReader comp <$> asks globals

  typeError err  = failDoc $ prettyM err 
  newError err k = k `catchError` (const $ typeError err)
  -- handleError k k' = catchError k (const k')

  typeTrace tr   = -- traceM (showM tr) .
    (enterDoc $ prettyM tr)

  lookupGlobal x = symbType . sigLookup' (A.uid x) <$> asks globals

--  lookupGlobal x = ReaderT $ \ sig -> return $ lookupSafe x sig
    
{-
  addBind x a cont = do
    Context level tyEnv valEnv <- ask
    let xv   = freeVar level a
    let cxt' = Context 
                 (level + 1)
                 (Map.insert x a tyEnv)
                 (Map.insert x xv valEnv)
    local (const cxt') (cont xv)

  addBind' _ a cont = do
    l <- asks level
    let xv = freeVar l a
    local (\ cxt -> cxt { level = level cxt + 1 }) (cont xv)

  lookupVar x = do
    gamma <- asks tyEnv
    case Map.lookup x gamma of
      Just t  -> return t
      Nothing -> fail $ "unbound variable " ++ x 
-}

instance PrettyM CheckExprM Val where
  prettyM = doEval . prettyM 

checkTySig :: A.Expr -> A.Type -> CheckExprM ()
checkTySig e t = do
  -- checkType t
  t <- eval t
  check e t

runCheck :: A.Expr -> A.Type -> Err ()
runCheck e t = runReaderT (checkTySig e t) $ SigCxt Map.empty emptyContext

-- * Declarations

-- type CheckDeclM = StateT (MapSig Val) (ReaderT ScopeState (ErrorT String IO))
type CheckDeclM = StateT (MapSig Val) (ErrorT String IO)

instance MonadCheckDecl Val Env EvalM CheckExprM CheckDeclM where
{-
  doCheckExpr cont = do
    sig <- get 
    case runReaderT cont $ SigCxt sig emptyContext of
      Left err -> fail err
      Right a  -> return a
-}

  doCheckExpr cont = either throwError return . runReaderT cont . sigCxt =<< get where
     sigCxt sig = SigCxt sig emptyContext

--  doCheckExpr cont = (\ sig -> runReaderT cont $ SigCxt sig emptyContext) <$> get

instance PrettyM CheckDeclM Val where
  prettyM = doCheckExpr . prettyM 

checkDeclaration :: A.Declaration -> CheckDeclM ()
checkDeclaration d = do
  liftIO . putStrLn =<< showM d
  -- liftIO . putStrLn . show $ d -- debugging
  -- enter (show d) $  -- debugging
  checkDecl d

checkDeclarations :: A.Declarations -> CheckDeclM ()
checkDeclarations = mapM_ checkDeclaration . A.declarations

runCheckDecls :: A.Declarations -> IO (Err ())
runCheckDecls ds = runErrorT $ evalStateT (checkDeclarations ds) Map.empty

{-
runCheckDecls :: ScopeState -> A.Declarations -> IO (Err ())
runCheckDecls st ds = runErrorT $ runReaderT (evalStateT (checkDeclarations ds) Map.empty) st
-}

-- * Testing

-- polymorphic identity
hashString = fromIntegral . foldr f 0
      where f c m = ord c + (m * 128) `rem` 1500007

hash :: String -> A.Name 
hash s = A.Name (hashString s) s

var' x   = A.Ident $ A.Var $ hash x
abs x e = A.Lam (hash x) Nothing e
app     = A.App
ty = A.Typ
pi x = A.Pi (Just $ hash x)

eid = abs "A" $ abs "x" $ var' "x"
tid = pi "A" ty $ pi "x" (var' "A") $ var' "A"

arrow a b = A.Pi Nothing a b

tnat = pi "A" ty $ 
         pi "zero" (var' "A") $ 
         pi "suc"  (var' "A" `arrow` var' "A") $
           var' "A" 

ezero  = abs "A" $ abs "zero" $ abs "suc" $ var' "zero"
-- problem: esuc is not a nf
esuc n = abs "A" $ abs "zero" $ abs "suc" $ var' "suc" `app` 
          (n `app` var' "A" `app` var' "zero" `app` var' "suc")  

enat e =  abs "A" $ abs "zero" $ abs "suc" $ e
enats = map enat $ iterate (app (var' "suc")) (var' "zero")
e2 = enats !! 2

rid = runCheck eid tid

success = [(eid,tid),(ezero,tnat),(e2,tnat)] -- ,(Sort Type,Sort Kind)]
failure = [(tid,tid)]

runsuccs = map (uncurry runCheck) success
runtests = map (uncurry runCheck) (success ++ failure)

