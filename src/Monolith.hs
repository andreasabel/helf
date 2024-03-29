{- | Monolithic monad for everything. -}

-- A type checker instance with
-- (*) values as explicit closures and
-- (*) environments as finite maps

{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses,
    OverlappingInstances, IncoherentInstances, UndecidableInstances,
    PatternGuards, TupleSections #-}

module Monolith where

import Prelude hiding (pi,abs,mapM)

import Control.Applicative
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad ((<=<))
import Control.Monad.Except  hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.State  hiding (mapM)

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Traversable

import qualified Abstract as A
import MonoVal
import qualified ListEnv as Env
import Context
import PrettyM
import Signature
import TypeCheck hiding (app)
import Util
import Value

import Data.Char -- testing


-- | Just one monad for everything.

type TCM = ReaderT Context (StateT (MapSig Val) (ExceptT String IO))

-- | Evaluation monad

type EvalM = TCM

instance PrettyM EvalM Val where
  prettyM = prettyM <=< reify

-- * Context

data Context = Context
  { level  :: Int
  , tyEnv  :: Env
  , valEnv :: Env
  }

emptyContext :: Context
emptyContext = Context 0 Env.empty Env.empty

-- * Type checking monad

data SigCxt = SigCxt { globals :: MapSig Val, locals :: Context }

type Err = Either String
type CheckExprM = TCM -- ReaderT SigCxt Err

instance MonadCxt Val Env CheckExprM where

  addLocal n@(A.Name x _) t cont = do
    Context l gamma rho <- ask
    let xv  = Ne (A.Var $ n { A.uid = l }) t []
    let cxt = Context (l + 1) (Env.insert x t gamma) (Env.insert x xv rho)
    local (\ sc -> cxt) $ cont xv

  lookupLocal n@(A.Name x _) = do
    gamma <- asks $ tyEnv
    return $ Env.lookupSafe x gamma

  getEnv = asks $ valEnv

instance MonadCheckExpr Head Val Env EvalM CheckExprM where

  doEval comp    = comp -- runReader comp <$> asks globals

  typeError err  = failDoc $ prettyM err
  newError err k = k `catchError` (const $ typeError err)
  -- handleError k k' = catchError k (const k')

  typeTrace tr   = -- traceM (showM tr) .
    (enterDoc $ prettyM tr)

  lookupGlobal x = symbType . sigLookup' (A.uid x) <$> get

--  lookupGlobal x = ReaderT $ \ sig -> return $ lookupSafe x sig

{-
instance PrettyM CheckExprM Val where
  prettyM = doEval . prettyM
-}

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
type CheckDeclM = TCM -- StateT (MapSig Val) (ExceptT String IO)

instance MonadCheckDecl Head Val Env EvalM CheckExprM CheckDeclM where
{-
  doCheckExpr cont = do
    sig <- get
    case runReaderT cont $ SigCxt sig emptyContext of
      Left err -> fail err
      Right a  -> return a
-}

  doCheckExpr cont = cont
{- either throwError return . runReaderT cont . sigCxt =<< get where
     sigCxt sig = SigCxt sig emptyContext
-}
--  doCheckExpr cont = (\ sig -> runReaderT cont $ SigCxt sig emptyContext) <$> get

{-
instance PrettyM CheckDeclM Val where
  prettyM = doCheckExpr . prettyM
-}

checkDeclaration :: A.Declaration -> CheckDeclM ()
checkDeclaration d = do
  liftIO . putStrLn =<< showM d
  -- liftIO . putStrLn . show $ d -- debugging
  -- enter (show d) $  -- debugging
  checkDecl d

checkDeclarations :: A.Declarations -> CheckDeclM ()
checkDeclarations = mapM_ checkDeclaration . A.declarations

runCheckDecls :: A.Declarations -> IO (Err ())
runCheckDecls ds = runExceptT $ evalStateT (checkDeclarations ds `runReaderT` emptyContext) Map.empty

{-
runCheckDecls :: ScopeState -> A.Declarations -> IO (Err ())
runCheckDecls st ds = runExceptT $ runReaderT (evalStateT (checkDeclarations ds) Map.empty) st
-}

-- * Testing

-- polymorphic identity
hashString = fromIntegral . foldr f 0
      where f c m = ord c + (m * 128) `rem` 1500007

hash :: String -> A.Name
hash s = A.Name (hashString s) (Text.pack s)

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

{-
rid = runCheck eid tid

success = [(eid,tid),(ezero,tnat),(e2,tnat)] -- ,(Sort Type,Sort Kind)]
failure = [(tid,tid)]

runsuccs = map (uncurry runCheck) success
runtests = map (uncurry runCheck) (success ++ failure)

-}
