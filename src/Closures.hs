-- A type checker instance with 
-- * values as explicit closures and 
-- * environments as finite maps

module Closures where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import TypeCheck

-- Values

type Var = Int

data Head 
  = HVar Var Val     -- typed variable 
  | HType 
  | HKind

data Val 
  = Ne   Head [Val]  -- x vs^-1 | c vs^-1   last argument first in list!
  | Clos Expr Env    -- (\xe) rho
  | Fun  Val  Val    -- Pi a ((\xe)rho)

instance Value Int Val where
  typ  = Ne HType []
  kind = Ne HKind []
  freeVar x t = Ne (HVar x t) []

  tyView v =
    case v of
      Fun a b    -> VPi a b
      Ne HType _ -> VType
      Ne HKind _ -> VKind
      _          -> VBase
 
  tmView v =
    case v of
      Ne (HVar x t) vs -> VNe x t (reverse vs)
      _                -> VVal

-- Environments

type Env = Map Name Val

update :: Env -> Name -> Val -> Env
update rho x e = Map.insert x e rho 

-- Evaluation

apply :: Val -> Val -> Val
apply f v =
  case f of
    Ne h vs            -> Ne h (v:vs)
    Clos (Abs x e) rho -> evaluate e (update rho x v)

evaluate :: Expr -> Env -> Val
evaluate e rho =
  case e of
    Var x     -> Maybe.fromJust $ Map.lookup x rho
    App f e   -> evaluate f rho `apply` evaluate e rho
    Abs{}     -> Clos e rho
    Pi x e e' -> Fun (evaluate e rho) $ Clos (Abs x e') rho
    Type      -> typ
    Kind      -> kind

-- Type checking monad

data Context = Context
  { level  :: Int
  , tyEnv  :: Env
  , valEnv :: Env
  }

emptyContext :: Context
emptyContext = Context 0 Map.empty Map.empty

type Err = Either String
type TCM = ReaderT Context Err

instance TypeCheck Val TCM where  

  app f v = return $ apply f v
  
  eval e = evaluate e <$> asks valEnv 
    
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

checkTySig :: Expr -> Expr -> TCM ()
checkTySig e t = do
  -- checkType t
  t <- eval t
  check e t

runCheck :: Expr -> Expr -> Err ()
runCheck e t = runReaderT (checkTySig e t) emptyContext

-- Testing

eid = Abs "A" $ Abs "x" $ Var "x"
tid = Pi "A" Type $ Pi "x" (Var "A") $ Var "A"

rid = runCheck eid tid

tests = [(eid,tid),(tid,tid)]

runtests = map (uncurry runCheck) tests
