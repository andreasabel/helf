{-# LANGUAGE OverlappingInstances, IncoherentInstances, UndecidableInstances,
    PatternGuards, TupleSections #-}

module HerBruijnVal where

import Prelude hiding (pi,abs,mapM,lookup)

import Control.Monad.Reader hiding (mapM)
import Control.Applicative hiding (empty)
import Control.Monad.Error hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.State hiding (mapM)

import Data.Traversable
import Data.Map (Map) 
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Abstract as A
--import Concrete (Name as CName)
import Value
import TypeCheck
-- import Context
import Signature
import Util hiding (lookupSafe)
import Value
import MapEnv as M hiding (mapM)

import Data.Set (Set, empty, notMember, insert)

-- * de Bruijn Terms

data BTm -- names are only used for quoting
  = B Int A.Name
  | BVar A.Name
  | BCon A.Name
  | BDef A.Name
  | BApp BTm BTm
  | BLam A.Name BTm
  | BConstLam BTm -- these are lambdas, but not counted
  | BSort Value.Sort
  | BPi BTm BTm


-- * beta normal values

data HVal
  = HBound Int A.Name [HVal]       -- }   (bound head variable)
  | HVar A.Name HVal  [HVal]       -- }
  | HCon A.Name HVal  [HVal]       -- }-> Head
  | HDef A.Name HVal HVal [HVal]   -- }
  | HLam A.Name HVal
  | HK HVal                        -- constant Lambda, not binding anything and therefor not counted by de Bruijn indices 
  | HSort Value.Sort
  | HFun HVal HVal
  | HDontCare

instance Value A.Name HVal where
  typ = HSort Type
  kind = HSort Kind
  freeVar = var
  valView v =
    case v of
      HBound k name vs    -> VNe name HDontCare (reverse vs) 
      HVar x t vs         -> VNe x t (reverse vs)
      HCon x t vs         -> VNe x t (reverse vs)
      HDef x v t vs       -> VDef x t (reverse vs)
      HLam _ _            -> VAbs
      HK _                -> VAbs
      HSort s             -> VSort s
      HFun a b            -> VPi a b
      -- HDontCare        -> error "Cannot view DontCare Value"
      
      
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


instance (Applicative m, Monad m, Signature HVal sig, MonadReader sig m) => MonadEval HVal Env' m where

{- THIS IS THE CORRECT CODE -}
  -- apply :: HVal-> HVal -> m HVal
  apply f w =
    case f of
      HBound k name vs            -> return $ HBound k name (w:vs)
      HVar x t vs                 -> return $ HVar x t (w:vs)
      HCon x t vs                 -> return $ HCon x t (w:vs)
      HDef x v t vs               -> return $ HDef x v t (w:vs)
      HLam x v                    -> subst v w
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
            
{- THIS IS THE CORRECT CODE -}
  -- evaluate :: Expr -> Env' -> m HVal
  evaluate expr env =
    let expr' = transform expr
    in evaluate' expr' env
    where
      -- evaluate' :: BTm -> Env' -> m HVal
      evaluate' btm env = case btm of
        B k n       -> return $ HBound k n []
        BVar x      -> return $ lookupVal (var_ x) env
        BCon x      -> con x . symbType . sigLookup' (uid x) <$> ask
        BDef x      -> do
                      SigDef t v <- sigLookup' (uid x) <$> ask
                      return $ def x v t
        BApp t1 t2  -> Util.appM2 apply (evaluate' t1 env) (evaluate' t2 env)
        BLam x t    -> (\z -> return $ HLam x z) =<< (evaluate' t env)
        BConstLam t -> (\z -> return $ HK z) =<< (evaluate' t env)
        BSort sort  -> return $ HSort sort
        BPi a b     -> Util.appM2 (\a' b' -> return $ HFun a' b') (evaluate' a env) (evaluate' b env)
        

{- THIS IS ONLY FOR DEBUGGING -}
{-  -- evaluate :: Expr -> Env' -> m HVal
  evaluate expr env =
    let expr' = transform expr
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
        
  -- evaluate' :: Expr -> m HVal
  evaluate' = flip evaluate M.empty
-}

  abstractPi a (_, HVar x _ []) b = return $ HFun a $ HLam x $ bindx 0 b where
    bindx :: Int -> HVal -> HVal
    -- debugging
    bindx k (HBound i n vs) = if i<k then HBound i n $ map (bindx k) vs else error "unbound HBound detected"
    bindx k (HVar y t vs)   = (if  x==y 
                                then HBound k x 
                                else HVar y $ bindx k t) 
                                $ map (bindx k) vs
    bindx k (HCon c t vs)   = HCon c t $ map (bindx k) vs
    bindx k (HDef y t d vs) = HDef y t d $ map (bindx k) vs
    bindx k (HLam y t)      = HLam y $ bindx (k+1) t -- wrong: if x==y then HLam y t else HLam y $ bindx (k+1) t
    bindx k (HK t)          = HK $ bindx k t
    bindx k (HFun a b)      = HFun (bindx k a) (bindx k b)
    bindx _ anything        = anything -- Sort, DontCare
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
    






subst :: (Applicative m, Monad m, Signature HVal sig, MonadReader sig m) => HVal -> HVal -> m HVal
subst t w = sub 0 t where
  sub :: (Applicative m, Monad m, Signature HVal sig, MonadReader sig m) => Int -> HVal -> m HVal
  sub k t = case t of
    HBound i n vs   -> if k==i 
                        then appsR w =<< mapM (sub k) vs
                        else HBound i n <$> mapM (sub k) vs
    HVar x a vs     -> HVar x a <$> mapM (sub k) vs
    HCon c a vs     -> HCon c a <$> mapM (sub k) vs
    HDef x a v vs   -> HDef x a v <$> mapM (sub k) vs
    HLam x v        -> HLam x <$> sub (k+1) v
    HK v            -> HK <$> sub k v
    HFun a b        -> HFun <$> sub k a <*> sub k b
    anything        -> return anything

    
-- * supporting unfolds

appsR' :: (Applicative m, Monad m, MonadEval HVal Env' m) => HVal -> [HVal] -> m HVal 
appsR' f vs = foldr (\ v mf -> mf >>= \ f -> apply' f v) (return f) vs

apply' :: (Applicative m, Monad m, MonadEval HVal Env' m) => HVal -> HVal -> m HVal
apply' f v =
    case f of
      HDef d w t []   -> apply' w v
      HDef d w t ws   -> appsR' w (v:ws)
      _               -> apply f v
      

-- * Reification

-- quote :: NVal -> A.SysNameCounter -> EvalM A.Expr
quote :: (Applicative m, Monad m, MonadEval HVal Env' m) => HVal -> A.SysNameCounter -> m A.Expr
quote v i =
  case v of
    HBound k n vs       -> foldr (flip A.App) (A.Ident $ A.Var n) <$> mapM (flip quote i) vs
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
quoteFun :: (Applicative m, Monad m, MonadEval HVal Env' m) =>
            HVal -> A.SysNameCounter -> m (A.Name, A.Expr)
quoteFun f i = do
  let n = case f of
          HLam x _  -> x
          HK w      -> A.noName
  let (x, i') = A.nextSysName i n
  v <- f `apply` (var_ x)
  (x,) <$> quote v i'


-- * transformation

-- maybe this should be transferred to Util.hs
data (Ord name) => LocBoundList name = LBL {lblsize :: Int, bList :: (Map name Int)}
lbl_empty = LBL 0 (Map.empty)

insert_lbl :: (Ord name) => name -> LocBoundList name -> LocBoundList name
insert_lbl n (LBL k m) = LBL (k+1) (Map.insert n k m) -- note that it does NOT matter whether or not n already had been a key before!

-- fakeinsert :: (Ord name) => LocBoundList name -> LocBoundList name
-- fakeinsert (LBL k m) = LBL k+1 m

lookup_lbl :: (Ord name) => name -> LocBoundList name -> Maybe Int
lookup_lbl x (LBL _ m)= Map.lookup x m


transform :: A.Expr -> BTm
transform = trans lbl_empty where
  trans :: LocBoundList A.Name -> A.Expr -> BTm
  trans lbl (Ident ident) = case ident of
        Var x -> case lookup_lbl x lbl of 
                        Just k  -> B (lblsize lbl - 1 - k) x
                        Nothing -> BVar x
        Con x -> BCon x
        Def x -> BDef x
  trans lbl (App e1 e2) = BApp (trans lbl e1) (trans lbl e2)
  trans lbl (Lam name _ e) = BLam name $ trans (insert_lbl name lbl) e
  trans _ Typ = BSort Type
  trans lbl (Pi mname a b) = case mname of
    Just n -> 
      let
        a' = trans lbl a
        b' = trans lbl $ Lam n Nothing b
      in 
        BPi a' b'
    Nothing ->
      let
        a' = trans lbl a
        b' = BConstLam $ trans lbl b
      in
        BPi a' b'


