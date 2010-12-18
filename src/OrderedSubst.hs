module OrderedSubst where

import Prelude hiding (pi)

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader

import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Abstract as A
import Value
import TypeCheck
-- import Context
import Signature
import Util

import DataStructure as DS
import DatastrucImpl.SimpleDynArray (DynArray)
import DatastrucImpl.List (List)
import DatastrucImpl.DynArrayInstance

-- "ordered" Expressions

-- type OName = String
type OType = OExpr


data OExpr
  = OCon Name
  | ODef Name
  | O
  | OApp OExpr Int OExpr -- NOTE: the Int-value specifies the number of O's in the second expression (i.e. the 'argument', not the 'function'), but there is no real reason for this choice. In fact, this should be changed as it does not match the specification of 'split'
  | OAbs [Int] OExpr
  | OPi  OType Int OType -- similar to OApp
  | OType
--  | OKind -- only internally
    deriving (Eq,Ord,Show)

-- Values

type Var = Int


data Head 
  = HVar Var      -- (typed) variable 
  | HCon A.Name   -- (typed) constructor
  deriving (Eq,Ord,Show)

data Val 
  = Ne   Head Val [Val]     -- x^a vs^-1 | c^a vs^-1   last argument first in list!
  | Sort Sort               -- s
  | Clos OExpr OSubst       -- (\xe) rho
  | Fun  Val  Val           -- Pi a ((\xe)rho)


instance Value Head Val where
  typ  = Sort Type
  kind = Sort Kind
  freeVar h t = Ne h t []

  tyView (Fun a b) = VPi a b
  tyView (Sort s)  = VSort s
  tyView _         = VBase
 
  tmView (Ne h t vs) = VNe h t (reverse vs) -- see above: last argument first in list!
  tmView _           = VVal
  

---------------

-- ordered substitution

type OSubst = DynArray Val

type EvalM = Reader (MapSig Val)


-- Evaluation
apply :: Val -> Val -> EvalM Val
-- apply (K w) _ _ = w
apply (Ne head t vs) v = return $ Ne head t (v:vs)
apply (Clos (OAbs klist oe) osubst) v = OrderedSubst.evaluate oe (multiinsert v klist osubst)

evaluate :: OExpr -> OSubst -> EvalM Val
evaluate e osubst = case e of
    OCon x         -> symbType . sigLookup' x <$> ask
    ODef x         -> symbDef  . sigLookup' x <$> ask
    O              -> return $ DS.get osubst 0
    OApp oe1 k oe2 -> let (osubst1, osubst2) = split ((DS.size osubst) - k) osubst 
                      in Util.appM2 OrderedSubst.apply (OrderedSubst.evaluate oe1 osubst1) (OrderedSubst.evaluate oe2 osubst2)
    OAbs _ _       -> return $ Clos e osubst
    -- OPi ty1 ty2    -> Fun (OrderedSubst.evaluate ty1 osubst) (Clos ty2 osubst) -- wrong.
    OPi ty1 k ty2  -> --return $ Sort Type
                      let (osubst1, osubst2) = split ((DS.size osubst) - k) osubst
                      in 
                      -- this is not correct - but at least, it can be compiled:
                      -- (OrderedSubst.evaluate ty2 osubst2) >>= ((return $ Sort Type)               >>= ( \a -> \b -> return (Fun a b) )  )
                      -- this is correct but cannot be compiled:
                      -- (OrderedSubst.evaluate ty2 osubst2) >>= ((OrderedSubst.evaluate ty1 osubst1) >>= ( \a -> \b -> return (Fun a b) )  )
                      -- okay, this also does not work:
                      -- (return $ Sort Type)               >>= ((OrderedSubst.evaluate ty2 osubst2) >>= ( \a -> \b -> return (Fun a b) )  )
                      -- finally, this works:
                      
                      do
                      mty1 <- OrderedSubst.evaluate ty1 osubst1
                      mty2 <- OrderedSubst.evaluate ty2 osubst2
                      return $ Fun mty1 mty2
                      
                      -- (OrderedSubst.evaluate ty1 osubst1) >>= (\ a -> (OrderedSubst.evaluate ty2 osubst2) >>= (\ b ->  return $ Fun a b))
                      --Util.appM2 (return Fun) (OrderedSubst.evaluate ty1 osubst1) (OrderedSubst.evaluate ty2 osubst2)
                      
    OType          -> return $ typ
    -- OKind       -> kind





instance MonadEval Val OSubst EvalM where
  apply = OrderedSubst.apply
  evaluate  e = OrderedSubst.evaluate (transform e)
  evaluate' e = OrderedSubst.evaluate (transform e) DS.empty
  
  -- Here we get a problem: In this file, values are always closed. As b is a value, we can only form a "fake"-dependent type (which is not really dependent).
  -- TODO !!
  abstractPi a _ b = do
                                      b' <- OrderedSubst.apply (Clos (OAbs [0] O) DS.empty) b
                                      return $ Fun a b'
  
  

-- hConst x = Ne (HConst x) []
-- hConst :: A.Name -> Val -> Val
-- hConst x t = Ne (HCon x) t []




{-- Closures:

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
  trans (Ident ident) = case ident of
    Var x -> do
      lbl <- ask
      case Map.lookup x (bList lbl) of
        Just k -> do
          {- ll <- get
          put $ incrKaddZero (lblsize lbl - 1 - k) ll  -}
          modify $ incrKaddZero (lblsize lbl - 1 - k)
          return (1, O)
        Nothing -> fail $ "variable " ++ show x ++ " is not bound"
    Con x -> return (0, OCon x)
    Def x -> return (0, ODef x)
  
  {-
    trans (Apps (e:es)) = 
    let
    trShortApp :: Expr -> Expr -> OExpr
    trShortApp e1 e2 = do
      (i1, oexpr1) <- trans e1
      (i2, oexpr2) <- trans e2
      return (i1 + i2, OApp oexpr1 i2 oexpr2)
    in
    foldl trShortApp e es
  -}
  
  trans (App e1 e2) = do
    (i1, oexpr1) <- trans e1
    (i2, oexpr2) <- trans e2
    return (i1 + i2, OApp oexpr1 i2 oexpr2)
  
  trans (Lam x mty e) = do -- !! Maybe Type !!
    modify ((:) [0]) 
    (i, oexpr) <- local (OrderedSubst.insert x) $ trans e
    (l':ll')   <- Control.Monad.State.get
    put $ ll'
    return (i + 1 - (length l'), OAbs (reverse $ tail l') oexpr) 
  
  trans (Pi name ty1 ty2) = case name of
    Just n -> do
      (i1, oexpr1) <- trans ty1
      (i2, oexpr2) <- trans $ Lam n Nothing ty2 -- Nothing?
      return (i1+i2, OPi oexpr1 i2 oexpr2)
    Nothing -> do
      (i1, oexpr1) <- trans ty1
      (i2, oexpr2) <- trans ty2
      return (i1+i2, OPi oexpr1 i2 $ OAbs [] oexpr2)
  
  -- trans (Sort Type) = return (0, OType)
  -- trans (Sort Kind) = return (0, OKind)

{- TODO delete this
---
type Type = Expr
data Expr
  = Ident Ident
  | Typ                           -- ^ type
  | Pi    (Maybe Name) Type Type  -- ^ A -> B or {x:A} B
  | Lam   Name (Maybe Type) Expr  -- ^ [x:A] E or [x]E
  | App   Expr Expr               -- ^ E1 E2 
  deriving (Show)

data Ident 
  = Var { name :: Name }          -- ^ locally bound identifier
  | Con { name :: Name }          -- ^ declared constant
  | Def { name :: Name }          -- ^ defined identifier
  deriving (Show)
---
-}



-- transform (original version, not updated)
{-
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
-}

-----------------------------------------------------------------------------
-- Tests (not updated yet)

{-
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
-}