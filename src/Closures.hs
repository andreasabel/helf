-- A type checker instance with 
-- * values as explicit closures and 
-- * environments as finite maps

{-# LANGUAGE OverlappingInstances, IncoherentInstances #-}

module Closures where

import Prelude hiding (pi,abs)

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map
-- import qualified Data.Maybe as Maybe

import qualified Abstract as A
import Context
import Signature
import TypeCheck hiding (app)
import Util
import Value

import Data.Char -- testing
-- import Text.PrettyPrint

-- Values

type Var = Int

data Head 
  = HVar Var      -- (typed) variable 
  | HCon A.Name   -- (typed) constructor
  deriving (Eq,Ord,Show)

data Val 
  = Ne   Head Val [Val]     -- x^a vs^-1 | c^a vs^-1   last argument first in list!
  | Sort Sort               -- s
  | CLam A.Name A.Expr Env  -- (\xe) rho
  | K    Val                -- constant function
  | Fun  Val  Val           -- Pi a ((\xe)rho)

instance Value Head Val where
  typ  = Sort Type 
  kind = Sort Kind
  freeVar h t = Ne h t []

  tyView v =
    case v of
      Fun a b -> VPi a b
      Sort s  -> VSort s
      _       -> VBase
 
  tmView v =
    case v of
      Ne h t vs -> VNe h t (reverse vs)
      _         -> VVal

con :: A.Name -> Val -> Val
con x t = Ne (HCon x) t []

-- Environments

type Env = Map A.Name Val

update :: Env -> A.Name -> Val -> Env
update rho x e = Map.insert x e rho 

-- Evaluation

type EvalM = Reader (MapSig Val)

instance MonadEval Val Env EvalM where

  apply f v =
    case f of
      K w          -> return $ w
      Ne h t vs    -> return $ Ne h t (v:vs)
      CLam x e rho -> evaluate e (update rho x v)
  
  evaluate e rho =
    case e of
      A.Ident (A.Con x) -> con x . symbType . sigLookup' x <$> ask
      A.Ident (A.Def x) -> symbDef . sigLookup' x  <$> ask
      A.Ident (A.Var x) -> return $ lookupSafe x rho
      A.App f e    -> Util.appM2 apply (evaluate f rho) (evaluate e rho)
      A.Lam x mt e -> return $ CLam x e rho
      A.Pi mx e e' -> Fun <$> (evaluate e rho) <*> case mx of
                        Just x  -> return $ CLam x e' rho
                        Nothing -> K <$> evaluate e' rho 
      A.Typ        -> return typ

  evaluate' e = evaluate e Map.empty

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

  addLocal x t cont = do
    l <- asks level
    let xv = Ne (HVar l) t []
    local (\ (Context l gamma rho) -> 
             Context (l + 1) (Map.insert x t gamma) (Map.insert x xv rho)) 
          (cont xv)

  lookupLocal x = do 
    gamma <- asks tyEnv
    return $ lookupSafe x gamma

  getEnv = asks valEnv

-- * Type checking monad

data SigCxt = SigCxt { globals :: MapSig Val, locals :: Context }

type Err = Either String
type CheckExprM = ReaderT SigCxt Err

instance MonadCxt Val Env CheckExprM where

  addLocal x t cont = do
    Context l gamma rho <- asks locals
    let xv  = Ne (HVar l) t []
    let cxt = Context (l + 1) (Map.insert x t gamma) (Map.insert x xv rho) 
    local (\ sc -> sc { locals = cxt }) $ cont xv

  lookupLocal x = do 
    gamma <- asks $ tyEnv . locals
    return $ lookupSafe x gamma

  getEnv = asks $ valEnv . locals

instance MonadCheckExpr Val Env EvalM CheckExprM where  

  doEval comp = runReader comp <$> asks globals

  lookupGlobal x = symbType . sigLookup' x <$> asks globals

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

checkTySig :: A.Expr -> A.Type -> CheckExprM ()
checkTySig e t = do
  -- checkType t
  t <- eval t
  check e t

runCheck :: A.Expr -> A.Type -> Err ()
runCheck e t = runReaderT (checkTySig e t) $ SigCxt Map.empty emptyContext

-- * Declarations

type CheckDeclM = StateT (MapSig Val) Err 

instance Field (MapSig v) (MapSig v) where
  getF = id
  setF = const
  modF = id

instance MonadCheckDecl Val Env EvalM CheckExprM CheckDeclM where
{-
  doCheckExpr cont = do
    sig <- get 
    case runReaderT cont $ SigCxt sig emptyContext of
      Left err -> fail err
      Right a  -> return a
-}

  doCheckExpr cont = either fail return . runReaderT cont . sigCxt =<< get where
     sigCxt sig = SigCxt sig emptyContext

--  doCheckExpr cont = (\ sig -> runReaderT cont $ SigCxt sig emptyContext) <$> get

checkDeclaration :: A.Declaration -> CheckDeclM ()
checkDeclaration = checkDecl

checkDeclarations :: A.Declarations -> CheckDeclM ()
checkDeclarations = mapM_ checkDeclaration . A.declarations

runCheckDecls :: A.Declarations -> Err ()
runCheckDecls ds = evalStateT (checkDeclarations ds) Map.empty

-- * Testing

-- polymorphic identity
hashString = fromIntegral . foldr f 0
      where f c m = ord c + (m * 128) `rem` 1500007

var x   = A.Ident $ A.Var $ hashString x
abs x e = A.Lam (hashString x) Nothing e
app     = A.App
ty = A.Typ
pi x = A.Pi (Just $ hashString x)

eid = abs "A" $ abs "x" $ var "x"
tid = pi "A" ty $ pi "x" (var "A") $ var "A"

arrow a b = A.Pi Nothing a b

tnat = pi "A" ty $ 
         pi "zero" (var "A") $ 
         pi "suc"  (var "A" `arrow` var "A") $
           var "A" 

ezero  = abs "A" $ abs "zero" $ abs "suc" $ var "zero"
-- problem: esuc is not a nf
esuc n = abs "A" $ abs "zero" $ abs "suc" $ var "suc" `app` 
          (n `app` var "A" `app` var "zero" `app` var "suc")  

enat e =  abs "A" $ abs "zero" $ abs "suc" $ e
enats = map enat $ iterate (app (var "suc")) (var "zero")
e2 = enats !! 2

rid = runCheck eid tid

success = [(eid,tid),(ezero,tnat),(e2,tnat)] -- ,(Sort Type,Sort Kind)]
failure = [(tid,tid)]

runsuccs = map (uncurry runCheck) success
runtests = map (uncurry runCheck) (success ++ failure)

