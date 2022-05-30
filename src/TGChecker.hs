-- | A type checker instance with term graphs.

{-# LANGUAGE FlexibleContexts, FlexibleInstances,
    OverlappingInstances, IncoherentInstances, UndecidableInstances,
    PatternGuards, TupleSections, TypeSynonymInstances, MultiParamTypeClasses #-}

module TGChecker where

import Control.Applicative
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Except  hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.State  hiding (mapM)

import Data.Map (Map)
import qualified Data.Map as Map
-- import qualified Data.Maybe as Maybe
import Data.Traversable

import Debug.Trace

import qualified Abstract as A
import ORef
import TermGraph
import qualified MapEnv as Env
import Context
import PrettyM
import Signature
import TypeCheck hiding (app)
import Util
import Value

-- | Evaluation monad

type EvalM = ReaderT (MapSig Val) TGM

type Val = Term

instance PrettyM EvalM Val where
  prettyM = prettyM <=< reify

-- | Context

data Context = Context
  { level  :: Int
  , tyEnv  :: Env
  , valEnv :: Env
  }

emptyContext :: Context
emptyContext = Context 0 Env.empty Env.empty

-- | Type checking monad

type CheckExprM = ReaderT SigCxt (ExceptT String TGM)

data SigCxt = SigCxt { globals :: MapSig Val, locals :: Context }

type Err = Either String

instance MonadCxt Val Env CheckExprM where

  addLocal n t cont = do
    Context l gamma rho <- asks locals
    xv <- newORef $ var n t
    let cxt = Context (l + 1) (Env.insert n t gamma) (Env.insert n xv rho)
    local (\ sc -> sc { locals = cxt }) $ cont xv

{-
  addLocal n@(A.Name x _) t cont = do
    Context l gamma rho <- asks locals
    xv <- newORef $ var (n { A.uid = l }) t
    let cxt = Context (l + 1) (Env.insert n t gamma) (Env.insert n xv rho)
    local (\ sc -> sc { locals = cxt }) $ cont xv
-}

  lookupLocal n@(A.Name x _) = do
    gamma <- asks $ tyEnv . locals
    return $ Env.lookupSafe n gamma

  getEnv = asks $ valEnv . locals

instance MonadCheckExpr Head Val Env EvalM CheckExprM where

  doEval comp    = do
    sig <- asks globals
    lift $ lift $ runReaderT comp sig

  unlessId r1 r2 cont = if (r1 /= r2) then cont else do
    unlessM (atomic r1) $ do
      traceDoc $ text "==> ref identity fired for" <+> prettyM r1
    return ()

-- SKIP equate
--  equate r1 r2 = trace ("equate fired!") $ assign r1 r2 >> return ()

  typeError err  = failDoc $ prettyM err
  newError err k = k `catchError` (const $ typeError err)
  -- handleError k k' = catchError k (const k')

  typeTrace tr cont  = -- traceM (showM tr) >>
    enterDoc (prettyM tr) cont

{-
  traceEval m = do
    v <- m
    traceM (showTG v)
    return v
-}

  lookupGlobal x = symbType . sigLookup' (A.uid x) <$> asks globals

instance PrettyM CheckExprM Val where
  prettyM = doEval . prettyM

checkTySig :: A.Expr -> A.Type -> CheckExprM ()
checkTySig e t = do
  -- checkType t
  t <- eval t
  check e t

{-
runCheck :: A.Expr -> A.Type -> Err ()
runCheck e t = runReaderT (checkTySig e t) $ SigCxt Map.empty emptyContext
-}

-- * Declarations

-- type CheckDeclM = StateT (MapSig Val) (ReaderT ScopeState (ExceptT String IO))

type CheckDeclM = StateT (MapSig Val) (ExceptT String TGM)

instance MonadCheckDecl Head Val Env EvalM CheckExprM CheckDeclM where

  doCheckExpr cont = do
    sig <- get
    res <- lift $ lift $ runExceptT $ runReaderT cont $ SigCxt sig emptyContext
    case res of
      Left err -> throwError err
      Right a  -> return a

{-
  doCheckExpr cont = do
     res <- runExceptT . runReaderT cont . sigCxt =<< get
     either throwError return res
    where
     sigCxt sig = SigCxt sig emptyContext
-}
--  doCheckExpr cont = (\ sig -> runReaderT cont $ SigCxt sig emptyContext) <$> get

instance PrettyM CheckDeclM Val where
  prettyM = doCheckExpr . prettyM

checkDeclaration :: A.Declaration -> CheckDeclM ()
checkDeclaration d = do
  -- traceM $ return ("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
  liftIO . putStrLn =<< showM d  -- UNCOMMENT FOR PRINTING
  -- traceM $ return ("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")

  -- liftIO . putStrLn . show $ d -- debugging
  -- enter (show d) $  -- debugging
  checkDecl d

checkDeclarations :: A.Declarations -> CheckDeclM ()
checkDeclarations = mapM_ checkDeclaration . A.declarations

runCheckDecls :: A.Declarations -> IO (Err ())
runCheckDecls ds = evalORef $ evalTGM $ runExceptT $ evalStateT (checkDeclarations ds) Map.empty

{-
runCheckDecls :: ScopeState -> A.Declarations -> IO (Err ())
runCheckDecls st ds = runExceptT $ runReaderT (evalStateT (checkDeclarations ds) Map.empty) st
-}

-- * Testing
-- see Test.hs
