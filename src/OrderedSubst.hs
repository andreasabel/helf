module OrderedSubst where


import Prelude hiding (pi)

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import DynArray
import TypeCheck

-- "ordered" Expressions

type OName = String
type OType = OExpr

data OExpr
  = OVarFree OName
  | O
  | OApp OExpr Int OExpr
  | OAbs [Int] OExpr
  | OPi  OType OType
  | OType
  | OKind -- only internally
    deriving (Eq,Ord,Show)

-- Values

type Var = Int

data Head 
  = HVar Var Val     -- typed variable 
  | HSort Sort

data Val 
  = Ne   Head [Val]  -- x vs^-1 | c vs^-1   last argument first in list!
  | Clos OExpr OSubst    -- (\xe) rho
  | K    Val         -- constant function
  | Fun  Val  Val    -- Pi a ((\x e) rho)


instance Value Var Val where
  typ  = Ne (HSort Type) []
  kind = Ne (HSort Kind) []
  freeVar x t = Ne (HVar x t) []

  tyView v =
    case v of
      Fun a b        -> VPi a b
      Ne (HSort s) _ -> VSort s
      K _            -> VBase -- correct?
      _              -> VBase
 
  tmView v =
    case v of
      Ne (HVar x t) vs -> VNe x t (reverse vs)
      _                -> VVal


---------------

-- ordered substitution

type OSubst = DynArray (Int, Val) 

-- maybe not the best implementation... O(k*(log n)^2), n = size of the substitution, k = number of the position where a value has to be injected
-- injects a given value at the given places
update :: OSubst -> [Int] -> (Int, Val) -> OSubst
update osubst [] _ = osubst
update osubst (k:klist) ival = 
  let
  (left, right) = split osubst k
  left' = left `DynArray.join` (DynArray 1 (Repeat ival))
  osubst' = update right klist ival
  in
  left' `DynArray.join` osubst'

-- old:
--update osubst (k:klist) ival = update (arrayInsert ival k osubst) klist ival


---------------
---------------

---- any possibility to hide this? (it just serves the transformation below)
data (Ord name) => LocBoundList name = LBL {size :: Int, bList :: (Map name Int)}
lbl_empty = LBL 0 (Map.empty)

--size :: (Ord name) => LocBoundList name -> Int
--size (LBL k name) = k

--bList :: (Ord name) => LocBoundList name -> Map name Int
--bList (LBL _ m) = m

insert :: (Ord name) => name -> LocBoundList name -> LocBoundList name
insert n (LBL k m) = LBL (k+1) (Map.insert n k m) -- note that it does NOT matter whether or not n already had been a key before!

type LambdaLists = [[Int]]
incrKaddZero :: Int -> LambdaLists -> LambdaLists
incrKaddZero 0 (l:ll) = (0:l):ll
incrKaddZero (k+1) ((i:l):ll) = ((i+1):l) : incrKaddZero k ll
----


transform :: Expr -> OExpr
transform e =
  let 
  
  -- takes: an expression e, the list containing bound variable names which is valid for e, the binding lists of the lambdas 'above' e).
  -- returns the build expression's number of "not-yet-bound" bound variables (i.e. number of O which are not bound in the returned 'subexpression') and, after that, all the modified information in the same order as it was taken
  trans :: Expr -> (LocBoundList Name) -> LambdaLists -> (Int, OExpr, LambdaLists) -- why would we need the new LocBoundList ??
  trans (Var x) lbl ll = case Map.lookup x (bList lbl) of
    Just k  -> (1, O, incrKaddZero ((size lbl) - 1 - k) ll)
    Nothing -> (0, OVarFree x, ll)
  trans (App e1 e2) lbl ll = 
    let
    (i1, oexpr1, ll1) = trans e1 lbl ll
    (i2, oexpr2, ll2) = trans e2 lbl ll1
    in 
    (i1+i2, OApp oexpr1 i2 oexpr2, ll2)
  trans (Abs x e) lbl ll = 
    let
    (i, oexpr, l':ll') = trans e (insert x lbl) ([0]:ll) 
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
