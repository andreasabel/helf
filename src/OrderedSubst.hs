module OrderedSubst where

import Prelude hiding (pi)

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader

import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Abstract
import TypeCheck

import DataStructure
import DatastrucImplementations.DS_SimpleDynArray (DynArray)
import DatastrucImplementations.DS_List (List)

-- "ordered" Expressions

-- type OName = String
type OType = OExpr

data OExpr
--  = OVarFree OName
  = OCon Name
  | ODef Name
  | O
  | OApp OExpr Int OExpr
  | OAbs [Int] OExpr
  | OPi  OType OType
  | OType
--  | OKind -- only internally
    deriving (Eq,Ord,Show)

-- Values

type Var = Int

data Head 
  = HVar Var Val     -- typed variable 
  | HSort Sort
  | HConst Name

data Val 
  = Ne   Head [Val]   -- x vs^-1 | c vs^-1   last argument first in list!
  | Clos OExpr OSubst -- (\xe) rho
   -- | K    Val          -- constant function
  | Fun  Val  Val     -- Pi a ((\x e) rho)


instance Value Var Val where
  typ  = Ne (HSort Type) []
  kind = Ne (HSort Kind) []
  freeVar x t = Ne (HVar x t) []

  tyView v =
    case v of
      Fun a b        -> VPi a b
      Ne (HSort s) _ -> VSort s
      -- K _            -> VBase -- correct?
      _              -> VBase
 
  tmView v =
    case v of
      Ne (HVar x t) vs -> VNe x t (reverse vs)
      _                -> VVal


---------------

-- ordered substitution

type OSubst = DynArray (Int, Val) 

-- Evaluation

apply :: Val -> Int -> Val -> Val
-- apply (K w) _ _ = w
apply (Ne h vs) i v = Ne h (v:vs)
apply (Clos (OAbs klist oe) osubst) i v = evaluate oe (multiinsert (i, v) klist osubst)

evaluate :: OExpr -> OSubst -> Val
evaluate e osubst = case e of
    OVarFree x     -> hConst x
    O              -> snd $ DataStructure.get osubst 0
    OApp oe1 k oe2 -> let (osubst1, osubst2) = split k osubst in apply (evaluate oe1 osubst1) k (evaluate oe2 osubst2)
    OAbs _ _       -> Clos e osubst
    OPi ty1 ty2    -> Fun (evaluate ty1 osubst) (Clos ty2 osubst)
    OType          -> typ
    OKind          -> kind
    
    
hConst x = Ne (HConst x) []





{-- Closures:
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

-}


--------------------------------------------------------------------------------------
---- any possibility to hide this? (it just serves the transformation below)
data (Ord name) => LocBoundList name = LBL {lblsize :: Int, bList :: (Map name Int)}
lbl_empty = LBL 0 (Map.empty)

--lblsize :: (Ord name) => LocBoundList name -> Int
--lblsize (LBL k name) = k

--bList :: (Ord name) => LocBoundList name -> Map name Int
--bList (LBL _ m) = m

insert :: (Ord name) => name -> LocBoundList name -> LocBoundList name
insert n (LBL k m) = LBL (k+1) (Map.insert n k m) -- note that it does NOT matter whether or not n already had been a key before!

type LambdaLists = [[Int]]
incrKaddZero :: Int -> LambdaLists -> LambdaLists
incrKaddZero 0 (l:ll) = (0:l):ll
incrKaddZero (k+1) ((i:l):ll) = ((i+1):l) : incrKaddZero k ll


-- transform (monadic version)
--type Transform a = LocBoundList Name -> LambdaLists -> (a, LambdaLists)
type Transform = ReaderT (LocBoundList Name) (State LambdaLists)

transform :: Expr -> OExpr
transform e = snd $ trans e `runReaderT` lbl_empty `evalState` [] where
  
  trans :: Expr -> Transform (Int, OExpr)
  trans (Var x) = do
    lbl <- ask
    case Map.lookup x (bList lbl) of
      Just k -> do
  {-
        ll <- get
        put $ incrKaddZero (lblsize lbl - 1 - k) ll
  -}
        modify $ incrKaddZero (lblsize lbl - 1 - k)
        return (1, O)
      Nothing -> return (0, OVarFree x)
  
  trans (App e1 e2) = do
    (i1, oexpr1) <- trans e1
    (i2, oexpr2) <- trans e2
    return (i1 + i2, OApp oexpr1 i2 oexpr2)
  
  trans (Abs x e) = do
    modify ((:) [0]) 
    (i, oexpr) <- local (OrderedSubst.insert x) $ trans e
    (l':ll')   <- Control.Monad.State.get
    put $ ll'
    return (i + 1 - (length l'), OAbs (reverse $ tail l') oexpr) 
  
  trans (Pi name ty1 ty2) = case name of
    Just n -> do
      (i1, oexpr1) <- trans ty1
      (i2, oexpr2) <- trans $ (Abs n ty2)
      return (i1+i2, OPi oexpr1 oexpr2)
    Nothing -> do
      (i1, oexpr1) <- trans ty1
      (i2, oexpr2) <- trans ty2
      return (i1+i2, OPi oexpr1 $ OAbs [] oexpr2)
  
  trans (Sort Type) = return (0, OType)
  trans (Sort Kind) = return (0, OKind)


-- transform (original version)
transform0 :: Expr -> OExpr
transform0 e =
  let 
  
  -- takes: an expression e, the list containing bound variable names which is valid for e, the binding lists of the lambdas 'above' e).
  -- returns the build expression's number of "not-yet-bound" bound variables (i.e. number of O which are not bound in the returned 'subexpression') and, after that, all the modified information in the same order as it was taken
  trans :: Expr -> (LocBoundList Name) -> LambdaLists -> (Int, OExpr, LambdaLists) -- why would we need the new LocBoundList ??
  trans (Var x) lbl ll = case Map.lookup x (bList lbl) of
    Just k  -> (1, O, incrKaddZero ((lblsize lbl) - 1 - k) ll)
    Nothing -> (0, OVarFree x, ll)
  trans (App e1 e2) lbl ll = 
    let
    (i1, oexpr1, ll1) = trans e1 lbl ll
    (i2, oexpr2, ll2) = trans e2 lbl ll1
    in 
    (i1+i2, OApp oexpr1 i2 oexpr2, ll2)
  trans (Abs x e) lbl ll = 
    let
    (i, oexpr, l':ll') = trans e (OrderedSubst.insert x lbl) ([0]:ll) 
    l'' = reverse (tail l') -- TODO: check whether reversing is really necessary!
    ibound = length l''
    in
    (i - ibound, OAbs l'' oexpr, ll')

  -- TODO: check whether the following is correct!

  trans (Pi name ty1 ty2) lbl ll = 
    case name of
      Just n -> 
        let
        (i1, oexpr1, ll1) = trans ty1 lbl ll
        (i2, oexpr2, ll2) = trans (Abs n ty2) lbl ll1 -- TODO: correct??
        in
        (i1+i2, OPi oexpr1 oexpr2, ll2)
      Nothing ->
        let
        (i1, oexpr1, ll1) = trans ty1 lbl ll
        (i2, oexpr2, ll2) = trans ty2 lbl ll1 -- same as above
        oexpr2' = OAbs [] oexpr2
        in
        (i1+i2, OPi oexpr1 oexpr2', ll2)
  trans (Sort Type) lbl ll = (0, OType, ll)
  trans (Sort Kind) lbl ll = (0, OKind, ll)
  
  (i, oexpr, ll) = trans e lbl_empty []
  in 
  oexpr

-----------------------------------------------------------------------------
-- Tests

test = Abs "1" $ Abs "2" $ Abs "3" $ Abs "4" $ Abs "5" $ Abs "6" $ Var "3"

ty = Sort Type
pi x = Pi (Just x)

eid = Abs "A" $ Abs "x" $ Var "x"
tid = pi "A" ty $ pi "x" (Var "A") $ Var "A"

arrow a b = Pi Nothing a b

tnat = pi "A" ty $ 
         pi "zero" (Var "A") $ 
         pi "suc"  (Var "A" `arrow` Var "A") $
           Var "A" 

ezero  = Abs "A" $ Abs "zero" $ Abs "suc" $ Var "zero"
-- problem: esuc is not a nf
esuc n = Abs "A" $ Abs "zero" $ Abs "suc" $ Var "suc" `App` 
          (n `App` Var "A" `App` Var "zero" `App` Var "suc")  

enat e =  Abs "A" $ Abs "zero" $ Abs "suc" $ e
enats = map enat $ iterate (App (Var "suc")) (Var "zero")
e2 = enats !! 2

transTest = transform test -- etc
