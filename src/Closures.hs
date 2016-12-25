-- | A type checker instance with
-- - values as explicit closures and
-- - environments as finite maps.

{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses,
    OverlappingInstances, IncoherentInstances, UndecidableInstances,
    PatternGuards, TupleSections #-}

module Closures where

import Control.Applicative
import Control.Monad ((<=<))
import Control.Monad.Except  hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.State  hiding (mapM)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable

import qualified Abstract as A
import ClosVal
import qualified ListEnv as Env
import Context
import PrettyM
import Signature
import TypeCheck hiding (app)
import Util
import Value

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

type CheckDeclM = StateT (MapSig Val) (ExceptT String IO)

instance MonadCheckDecl Head Val Env EvalM CheckExprM CheckDeclM where
  doCheckExpr cont = either throwError return . runReaderT cont . sigCxt =<< get
    where
    sigCxt sig = SigCxt sig emptyContext

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
runCheckDecls ds = runExceptT $ evalStateT (checkDeclarations ds) Map.empty
