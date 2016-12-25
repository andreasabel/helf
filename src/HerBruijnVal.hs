{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts,
    OverlappingInstances, IncoherentInstances, UndecidableInstances,
    PatternGuards, TupleSections, MultiParamTypeClasses #-}

module HerBruijnVal where

import Prelude hiding (pi,abs,mapM,lookup)

import Control.Monad.Reader hiding (mapM)
import Control.Applicative hiding (empty)
import Control.Monad.Except hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.State hiding (mapM)

import qualified Data.List as List
import Data.Traversable
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Abstract as A
--import Concrete (Name as CName)
import Value
import LocallyNamelessSyntax
import TypeCheck
-- import Context
import Signature
import Util hiding (lookupSafe)
import Value
import MapEnv as M hiding (mapM)

import Data.Set (Set, empty, notMember, insert)

-- * heads

data Head
  = HdBound Int A.Name  -- bound variable: de Bruijn index
  | HdFree A.Name   -- free variable: name
  | HdCon A.Name
  | HdDef A.Name
    deriving (Ord, Show)

instance Eq Head where
  HdBound i _ == HdBound i' _ = i == i'
  HdFree x == HdFree x' = x == x'
  HdCon x == HdCon x' = x == x'
  HdDef x == HdDef x' = x == x'
  _ == _ = False

-- * beta normal values

data HVal
  = HBound Int A.Name [HVal]       -- }   (bound head variable)
  | HVar A.Name HVal  [HVal]       -- }
  | HCon A.Name HVal  [HVal]       -- }-> Head
  | HDef A.Name HVal HVal [HVal]   -- }
  | HLam A.Name HVal
  | HK HVal                        -- constant Lambda, not binding anything and therefore not counted by de Bruijn indices
  | HSort Value.Sort
  | HFun HVal HVal
  | HDontCare
    deriving (Show)

{-
instance Value Head HVal where
  typ = HSort Type
  kind = HSort Kind
  freeVar (HdFree x) = var x
  valView v =
    case v of
      HBound k name vs    -> VNe (HdBound k name) HDontCare (reverse vs)
      HVar x t vs         -> VNe (HdFree x) t (reverse vs)
      HCon x t vs         -> VNe (HdCon x) t (reverse vs)
      HDef x v t vs       -> VDef (HdDef x) t (reverse vs)
      HLam _ _            -> VAbs
      HK _                -> VAbs
      HSort s             -> VSort s
      HFun a b            -> VPi a b
      -- HDontCare        -> error "Cannot view DontCare Value"
-}

-- * smart constructors

var :: A.Name -> HVal -> HVal
var x t = HVar x t []

var_ :: A.Name -> HVal
var_ x = var x HDontCare

con :: A.Name -> HVal -> HVal
con x t = HCon x t []

def :: A.Name -> HVal -> HVal -> HVal
def x v t = HDef x v t []


-- * environment handling
-- see MapEnv.hs
type Env' = Env UID HVal

lookupVal :: HVal -> Env' -> HVal
lookupVal v@(HVar x _ _) env = case lookup (uid x) env of
      Just w  -> w
      _       -> v


instance (Applicative m, Monad m, Signature HVal sig, MonadReader sig m) => MonadEval Head HVal Env' m where

  typ                = return $ HSort Type
  kind               = return $ HSort Kind
  freeVar (HdFree x) = return . var x
  valView v          = return $
    case v of
      HBound k name vs    -> VNe (HdBound k name) HDontCare (reverse vs)
      HVar x t vs         -> VNe (HdFree x) t (reverse vs)
      HCon x t vs         -> VNe (HdCon x) t (reverse vs)
      HDef x v t vs       -> VDef (HdDef x) t (reverse vs)
      HLam _ _            -> VAbs
      HK _                -> VAbs
      HSort s             -> VSort s
      HFun a b            -> VPi a b
      -- HDontCare        -> error "Cannot view DontCare Value"

  -- apply :: HVal-> HVal -> m HVal
  apply f w =
    case f of
      HBound k name vs            -> return $ HBound k name (w:vs)
      HVar x t vs                 -> return $ HVar x t (w:vs)
      HCon x t vs                 -> return $ HCon x t (w:vs)
      HDef x v t vs               -> return $ HDef x v t (w:vs)
      HLam x v                    -> subst v w --here?!
      HK v                        -> return v

{- ONLY FOR DEBUGGING
  -- apply :: HVal-> HVal -> m HVal
  apply f w =
    let result = appl' f w
    in
      (\correctVal -> if checkForPseudofree correctVal then return correctVal else fail $ "APPLY produced a value without unique names") =<< result
    where appl' :: HVal -> HVal -> m HVal
          appl' f w =
            if (checkForPseudofree f) && (checkForPseudofree w) then
              case f of
                HBound k name vs            -> return $ HBound k name (w:vs)
                HVar x t vs                 -> return $ HVar x t (w:vs)
                HCon x t vs                 -> return $ HCon x t (w:vs)
                HDef x v t vs               -> return $ HDef x v t (w:vs)
                HLam x v                    -> subst v w
                HK v                        -> return v
            else fail $ "APPLY found a value without unique names"
-}

  evaluate = evaluate2 -- we have evaluate1 and evaluate2, the latter uses a transformation bevor the evaluation is actually started.

{-
evaluate expr env =
    eval expr [] where
      eval expr bvars = case expr of
        Ident (Var x) ->
          case M.lookup (A.uid x) env of
            Just v  -> return v
            Nothing -> case List.elemIndex x bvars of
              Just i -> return $ HBound i x []
              Nothing -> fail "unbound variable"
        Ident (Con x) -> con x . symbType . sigLookup' (uid x) <$> ask
        Ident (Def x) -> do
                      SigDef t v <- sigLookup' (uid x) <$> ask
                      return $ def x v t
        App e1 e2 -> Util.appM2 apply (eval e1 bvars) (eval e2 bvars)
        Lam x _ e -> HLam x <$> eval e (x : bvars)
        Typ       -> return $ HSort Type
        Pi Nothing a b ->
          HFun <$> eval a bvars <*> (HK <$> eval b bvars)
        Pi (Just x) a b ->
          HFun <$> eval a bvars <*> (HLam x <$> eval b (x : bvars))
-}


{- THIS IS ONLY FOR DEBUGGING -}
{-  -- evaluate :: Expr -> Env' -> m HVal
  evaluate expr env =
    let expr' = toLocallyNameless expr
        result' = evaluate' expr' env
    in
    (\correctVal -> if checkForPseudofree correctVal then return correctVal else fail $ "EVALUATE did not bind all variables correctly") =<< result'
    where
      -- evaluate' :: BTm -> Env' -> m HVal
      evaluate' btm env = case btm of
        B k n       -> return $ HBound k n []
        BVar x      -> return $ lookupVal (var_ x) env
        -- BCon x      -> con x . symbType . sigLookup' (uid x) <$> ask
        -- debugging (above is the "normal" code):
        BCon x      -> do
                        result <- con x . symbType . sigLookup' (uid x) <$> ask
                        if checkForPseudofree result then return result else fail $ "EVALUATE: pseudofree variables after BCon"
        BDef x      -> do
                      SigDef t v <- sigLookup' (uid x) <$> ask
                      return $ def x v t
        BApp t1 t2  -> Util.appM2 apply (evaluate' t1 env) (evaluate' t2 env)
        BLam x t    -> (\z -> return $ HLam x z) =<< (evaluate' t env)
        BConstLam t -> (\z -> return $ HK z) =<< (evaluate' t env)
        BSort sort  -> return $ HSort sort
        BPi a b     -> Util.appM2 (\a' b' -> return $ HFun a' b') (evaluate' a env) (evaluate' b env)
-}

  -- evaluate' :: Expr -> m HVal
  evaluate' = flip evaluate M.empty

  abstractPi a (_, HVar x _ []) b = return $ HFun a $ HLam x $ bindx 0 b where
    -- INVARIANT in bindx k v: k is bigger than any index in k
    --   (if k is increased whenever stepping under a lambda)
    bindx :: Int -> HVal -> HVal
    -- debugging
    -- bindx k (HBound i n vs) = if i<k then HBound i n $ map (bindx k) vs else error "unbound HBound detected"
    bindx k (HBound i n vs) = HBound i n $ map (bindx k) vs
    bindx k (HVar y t vs)   = (if  x==y
                                then HBound k x
                                else HVar y $ bindx k t)
                                $ map (bindx k) vs
    bindx k (HCon c t vs)   = HCon c t $ map (bindx k) vs
    bindx k (HDef y t d vs) = HDef y t d $ map (bindx k) vs
    bindx k (HLam y t)      = HLam y $ bindx (k+1) t
    bindx k (HK t)          = HK $ bindx k t
    bindx k (HFun a b)      = HFun (bindx k a) (bindx k b)
    bindx k v@(HSort{})     = v
    bindx k v@HDontCare     = v
  abstractPi _ _ _          = fail $ "can only abstract a free variable"

  unfold v =
    case v of
      HDef d f t vs   -> appsR f vs
      _               -> return v

  unfolds v =
    case v of
      HDef d f t vs   -> unfolds =<< appsR' f vs
      _               -> return v

  -- reify v = fail $ "not implemented yet"
  reify v = quote v A.initSysNameCounter


-- we have two implementations of evaluate:
-- evaluate :: Expr -> Env' -> m HVal
evaluate1 expr env = -- (\ w -> assertClosed w (return w)) =<<
  eval expr where
    eval expr = case expr of
      Ident (Var x)   -> return $ maybe (var_ x) id $ M.lookup (A.uid x) env
      Ident (Con x)   -> con x . symbType . sigLookup' (uid x) <$> ask
      Ident (Def x)   -> do
                        SigDef t v <- sigLookup' (uid x) <$> ask
                        return $ def x v t
      App e1 e2       -> Util.appM2 apply (eval e1) (eval e2)
      Lam x _ e       -> HLam x . bind x <$> eval e -- inefficient?
      Typ             -> return $ HSort Type
      Pi Nothing a b  -> HFun <$> eval a <*> (HK <$> eval b)
      Pi (Just x) a b -> HFun <$> eval a <*> (HLam x . bind x <$> eval b)

-- old, uses toLocallyNameless:
evaluate2 expr env =
  let expr' = toLocallyNameless expr
  in evaluate' expr' env
  where
    -- evaluate' :: BTm -> Env' -> m HVal
    evaluate' btm env = case btm of
      B (DBIndex k n) -> return $ HBound k n []
      BVar x      -> return $ lookupVal (var_ x) env
      BCon x      -> con x . symbType . sigLookup' (uid x) <$> ask
      BDef x      -> do
                    SigDef t v <- sigLookup' (uid x) <$> ask
                    return $ def x v t
      BApp t1 t2  -> Util.appM2 apply (evaluate' t1 env) (evaluate' t2 env)
      BLam (Annotation x) t    -> (\z -> return $ HLam x z) =<< (evaluate' t env)
                     -- HLam x <$> evaluate' t env
      BConstLam t -> (\z -> return $ HK z) =<< (evaluate' t env)
      BSort sort  -> return $ HSort sort
      BPi a b     -> Util.appM2 (\a' b' -> return $ HFun a' b') (evaluate' a env) (evaluate' b env)



bind :: Name -> HVal -> HVal
bind x v = bindx 0 v where
    -- INVARIANT in bindx k v: k is bigger than any index in k
    --   (if k is increased whenever stepping under a lambda)
    bindx :: Int -> HVal -> HVal
    -- debugging
    -- bindx k (HBound i n vs) = if i<k then HBound i n $ map (bindx k) vs else error "unbound HBound detected"
    bindx k (HBound i n vs) = HBound i n $ map (bindx k) vs
    bindx k (HVar y t vs)   = (if  x==y
                                then HBound k x
                                else HVar y $ bindx k t)
                                $ map (bindx k) vs
    bindx k (HCon c t vs)   = HCon c t $ map (bindx k) vs
    bindx k (HDef y t d vs) = HDef y t d $ map (bindx k) vs
    bindx k (HLam y t)      = HLam y $ bindx (k+1) t -- wrong: if x==y then HLam y t else HLam y $ bindx (k+1) t
    bindx k (HK t)          = HK $ bindx k t
    bindx k (HFun a b)      = HFun (bindx k a) (bindx k b)
    bindx k v@(HSort{})     = v
    bindx k v@HDontCare     = v




-- for debugging only:
-- checks if a value contains a variable that actually should be bound. "True" means that everything is okay.
checkForPseudofree :: HVal -> Bool
checkForPseudofree = check Data.Set.empty where
  check :: Set A.Name -> HVal -> Bool
  check set v = case v of
    HBound _ _ vs -> foldr (&&) True $ map (check set) vs
    HVar x t vs   -> (notMember x set) && (check set t) && (foldr (&&) True $ map (check set) vs)
    HCon x t vs   -> foldr (&&) True $ map (check set) vs
    HDef x t d vs -> foldr (&&) True $ map (check set) vs
    HLam x t      -> check (Data.Set.insert x set) t
    HK t          -> check set t
    HFun a b      -> (check set a) && (check set b)
    _             -> True


checkClosed :: Int -> HVal -> Bool
checkClosed k v = -- check whether all indices in v are < k
  case v of
    HBound i n vs -> i < k && all (checkClosed k) vs
    HVar x a vs   -> checkClosed 0 a && all (checkClosed k) vs
    HCon c a vs   -> checkClosed 0 a && all (checkClosed k) vs
    HDef c a w vs -> checkClosed 0 a && checkClosed 0 w && all (checkClosed k) vs
    HLam x t      -> checkClosed (k+1) t
    HK t          -> checkClosed k t
    HFun a b      -> checkClosed k a && checkClosed k b
    _             -> True

assert :: Monad m => String -> Bool -> m a -> m a
assert s True  cont = cont
assert s False cont = fail s

assertClosed w = assert (show w ++ " not closed") (checkClosed 0 w)

{-
subst :: (Applicative m, Monad m, Signature HVal sig, MonadReader sig m) => HVal -> HVal -> m HVal
subst t w = assertClosed w $
 sub 0 t where
  sub :: (Applicative m, Monad m, Signature HVal sig, MonadReader sig m) => Int -> HVal -> m HVal
  sub k t = case t of
    HBound i n vs   -> if k==i
                        then appsR w =<< mapM (sub k) vs
                        else HBound i n <$> mapM (sub k) vs
    HVar x a vs     -> HVar x a <$> mapM (sub k) vs
    HCon c a vs     -> HCon c a <$> mapM (sub k) vs
    HDef x a v vs   -> HDef x a v <$> mapM (sub k) vs
    HLam x v        -> HLam x <$> sub (k+1) v   -- BUGGY!
    HK v            -> HK <$> sub k v
    HFun a b        -> HFun <$> sub k a <*> sub k b
    anything        -> return anything
-}


subst :: (Applicative m, Monad m, Signature HVal sig, MonadReader sig m) => HVal -> HVal -> m HVal
subst t w = --assertClosed w $
 sub 0 w t where
  sub :: (Applicative m, Monad m, Signature HVal sig, MonadReader sig m) => Int -> HVal -> HVal -> m HVal
  sub k w t = --if True then fail "subst aufgerufen" else
    case t of   -- t[w/k]
    HBound i n vs   -> if k==i -- i.e., i is to be replaced
                        then appsR w =<< mapM (sub k w) vs
                        else if k < i -- i.e, i is bound to a \ which stands to the left of the eliminated \
                          then HBound (i-1) n <$> mapM (sub k w) vs
                          else HBound i n <$> mapM (sub k w) vs
    HVar x a vs     -> HVar x a <$> mapM (sub k w) vs
    HCon c a vs     -> HCon c a <$> mapM (sub k w) vs
    HDef x a v vs   -> HDef x a v <$> mapM (sub k w) vs
    HLam x v        -> HLam x <$> sub (k+1) (lifting w) v
    HK v            -> HK <$> sub k w v
    HFun a b        -> HFun <$> sub k w a <*> sub k w b
    v@HSort{}       -> return v
    v@HDontCare     -> return v


-- raise the index of bound, but locally unbound variables by one
lifting :: HVal -> HVal
lifting w = lift' 0 w where
  lift' :: Int -> HVal -> HVal
  lift' k w = case w of -- k is the counter for lambdas in w
    HBound i n vs -> (if i < k  -- i.e. if i is bound
                      then HBound i n
                      else HBound (i+1) n) $ map (lift' k) vs
    HVar x a vs   -> HVar x a $ map (lift' k) vs
    HCon c a vs   -> HCon c a $ map (lift' k) vs
    HDef x a v vs -> HDef x a v $ map (lift' k) vs
    HLam x v      -> HLam x $ lift' (k+1) v
    HK v          -> HK $ lift' k v
    HFun a b      -> (HFun $ lift' k a) $ lift' k b
    anything      -> anything



{-

  [ K / x] (\ y. x y)
= [(\ \ 1) / 0] (\ 1 0)
= \ [(\ \ 1) / 1] (1 0)
= \ (\ \ 1) @ 0
= \ [0/0] (\ 1)
= \ \ [0/1] 1    <-- problematic step (substituting a non-closed term under lambda)
= \ \ 0
= K*

-}

-- * supporting unfolds

appsR' :: (Applicative m, Monad m, MonadEval Head HVal Env' m) => HVal -> [HVal] -> m HVal
appsR' f vs = foldr (\ v mf -> mf >>= \ f -> apply' f v) (return f) vs

apply' :: (Applicative m, Monad m, MonadEval Head HVal Env' m) => HVal -> HVal -> m HVal
apply' f v =
    case f of
      HDef d w t []   -> apply' w v
      HDef d w t ws   -> appsR' w (v:ws)
      _               -> apply f v


-- * Reification

-- quote :: NVal -> A.SysNameCounter -> EvalM A.Expr
quote :: (Applicative m, Monad m, MonadEval Head HVal Env' m) => HVal -> A.SysNameCounter -> m A.Expr
-- INVARIANT: i is negative, decreased for every lambda
quote v i =
  case v of
    HBound k n vs       -> foldr (flip A.App) (A.Ident $ A.Var (n {uid = k+i+1})) <$> mapM (flip quote i) vs
    HVar x _ vs         -> foldr (flip A.App) (A.Ident $ A.Var x) <$> mapM (flip quote i) vs
    HCon x _ vs         -> foldr (flip A.App) (A.Ident $ A.Con x) <$> mapM (flip quote i) vs
    HDef x _ _ vs       -> foldr (flip A.App) (A.Ident $ A.Def x) <$> mapM (flip quote i) vs
    --HLam x w            -> see below
    --HK _                -> see below
    HSort Type          -> return A.Typ
    HSort Kind          -> error "cannot quote sort kind"
    HDontCare           -> error "cannot quote the dontcare value"
    HFun a (HK b)       -> A.Pi Nothing <$> quote a i <*> quote b i
    HFun a f            -> do
                            u     <- quote a i
                            (x,t) <- quoteFun f i
                            return $ A.Pi (Just x) u t
    f                   -> do
                            (x,e) <- quoteFun f i
                            return $ A.Lam x Nothing e


-- quoteFun :: HVal -> A.SysNameCounter -> EvalM (A.Name, A.Expr)
quoteFun :: (Applicative m, Monad m, MonadEval Head HVal Env' m) =>
            HVal -> A.SysNameCounter -> m (A.Name, A.Expr)
quoteFun f i = do
  let n = case f of
          HLam x _  -> x
          HK w      -> A.noName
  let (x, i') = A.nextSysName i n
  v <- f `apply` (var_ x)
  (x,) <$> quote v i'
