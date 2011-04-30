-- A type checker instance with 
-- * values as explicit closures and 
-- * environments as finite maps

{-# LANGUAGE OverlappingInstances, IncoherentInstances, UndecidableInstances,
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
import ClosVal
import qualified ListEnv as Env
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

-- * Evaluation monad

type EvalM = Reader (MapSig Val)

instance PrettyM EvalM Val where
  prettyM = prettyM <=< reify

-- * Context monad

data Context = Context
  { level  :: Int
  , tyEnv  :: Env
  , valEnv :: Env
  }

emptyContext :: Context
emptyContext = Context 0 Env.empty Env.empty

type ContextM = Reader Context

instance MonadCxt Val Env ContextM where

  addLocal n@(A.Name x _) t cont = do
    l <- asks level
    let xv = Ne (A.Var $ n { A.uid = l }) t []
    local (\ (Context l gamma rho) -> 
             Context (l + 1) (Env.insert x t gamma) (Env.insert x xv rho)) 
          (cont xv)

  lookupLocal x = do 
    gamma <- asks tyEnv
    return $ Env.lookupSafe (A.uid x) gamma

  getEnv = asks valEnv

-- * Type checking monad

data SigCxt = SigCxt { globals :: MapSig Val, locals :: Context }

type Err = Either String
type CheckExprM = ReaderT SigCxt Err

instance MonadCxt Val Env CheckExprM where

  addLocal n@(A.Name x _) t cont = do
    Context l gamma rho <- asks locals
    let xv  = Ne (A.Var $ n { A.uid = l }) t []
    let cxt = Context (l + 1) (Env.insert x t gamma) (Env.insert x xv rho) 
    local (\ sc -> sc { locals = cxt }) $ cont xv

  lookupLocal n@(A.Name x _) = do 
    gamma <- asks $ tyEnv . locals
    return $ Env.lookupSafe x gamma

  getEnv = asks $ valEnv . locals

instance MonadCheckExpr Head Val Env EvalM CheckExprM where  

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

instance MonadCheckDecl Head Val Env EvalM CheckExprM CheckDeclM where
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
  -- liftIO . putStrLn =<< showM d  -- UNCOMMENT FOR PRINTING

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

