{-# LANGUAGE OverlappingInstances, IncoherentInstances, UndecidableInstances,
    PatternGuards, TupleSections #-}

module HeredNormal where

import Prelude hiding (pi,abs,mapM)

import Control.Applicative
import Control.Monad ((<=<))
import Control.Monad.Except  hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.State  hiding (mapM)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable

import qualified Abstract as A
import Context
import Signature
import TypeCheck hiding (app)
import Util hiding (lookupSafe)
import Value
import MapEnv as M

import PrettyM
import Data.Char

import HeredNormVal


-- * Evaluation monad

type EvalM = Reader (MapSig NVal)

instance PrettyM EvalM NVal where
  prettyM = prettyM <=< reify

-- * Context monad

data Context = Context
  { level  :: Int
  , tyEnv  :: Env'
  , valEnv :: Env'
  }

emptyContext :: Context
emptyContext = Context (-1) M.empty M.empty

type ContextM = Reader Context

instance MonadCxt NVal Env' ContextM where

  addLocal n@(A.Name x _) t cont = do
    l <- asks level
    let xv = NVar (n { A.uid = l }) t []
    local (\ (Context l gamma rho) ->
             Context (l - 1) (Map.insert x t gamma) (Map.insert x xv rho))
          (cont xv)

  lookupLocal x = do
    gamma <- asks tyEnv
    return $ lookupSafe (A.uid x) gamma

  getEnv = asks valEnv


-- * Type checking monad

data SigCxt = SigCxt { globals :: MapSig NVal, locals :: Context }

type Err = Either String
type CheckExprM = ReaderT SigCxt Err

instance MonadCxt NVal Env' CheckExprM where

  addLocal n@(A.Name x _) t cont = do
    Context l gamma rho <- asks locals
    let xv  = NVar (n { A.uid = l }) t []
    let cxt = Context (l - 1) (Map.insert x t gamma) (Map.insert x xv rho)
    local (\ sc -> sc { locals = cxt }) $ cont xv

  lookupLocal n@(A.Name x _) = do
    gamma <- asks $ tyEnv . locals
    return $ lookupSafe x gamma

  getEnv = asks $ valEnv . locals



instance MonadCheckExpr NVal Env' EvalM CheckExprM where

  doEval comp    = runReader comp <$> asks globals

  -- TODO ?
  typeError err  = failDoc $ prettyM err
  newError err k = k `catchError` (const $ typeError err)

  typeTrace tr   = (enterDoc $ prettyM tr)

  lookupGlobal x = symbType . sigLookup' (A.uid x) <$> asks globals


instance PrettyM CheckExprM NVal where
  prettyM = doEval . prettyM

checkTySig :: A.Expr -> A.Type -> CheckExprM ()
checkTySig e t = do
  -- checkType t
  t <- eval t
  check e t

runCheck :: A.Expr -> A.Type -> Err ()
runCheck e t = runReaderT (checkTySig e t) $ SigCxt M.empty emptyContext


-- * Declarations

type CheckDeclM = StateT (MapSig NVal) (ExceptT String IO)

instance MonadCheckDecl NVal Env' EvalM CheckExprM CheckDeclM where

  doCheckExpr cont = either throwError return . runReaderT cont . sigCxt =<< get where
     sigCxt sig = SigCxt sig emptyContext

instance PrettyM CheckDeclM NVal where
  prettyM = doCheckExpr . prettyM

checkDeclaration :: A.Declaration -> CheckDeclM ()
checkDeclaration d = do
  liftIO . putStrLn =<< showM d
  checkDecl d

checkDeclarations :: A.Declarations -> CheckDeclM ()
checkDeclarations = mapM_ checkDeclaration . A.declarations

runCheckDecls :: A.Declarations -> IO (Err ())
runCheckDecls ds = runExceptT $ evalStateT (checkDeclarations ds) Map.empty


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
