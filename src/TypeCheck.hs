-- A generic bidirectional type-checker for LF

module TypeCheck where

import Abstract
import Context
import PrettyM
import Scoping (prettyM)
import qualified Scoping
import Signature 
import Value
import Util

{-  
type Name = String
type Type = Expr

data Expr 
  = Var Name
  | App Expr Expr
  | Abs Name Expr
  | Pi  (Maybe Name) Type Type
  | Sort Sort
    deriving (Eq,Ord,Show)
-}

-- * Typechecking expressions.

class (Monad m, -- Scoping.Scope m, 
       MonadEval val env me, 
       MonadCxt val env m) =>
  MonadCheckExpr val env me m | m -> me where

  doEval       :: me a -> m a      -- ^ run evaluation
  app          :: val -> val -> m val
  app f v       = doEval $ apply f v
  eval         :: Expr -> m val
  eval e        = getEnv >>= \ rho -> doEval $ evaluate e rho
  abstrPi      :: val -> val -> val -> m val  -- ^ pi a x b
  abstrPi a x b = doEval $ abstractPi a x b
{-
  doCxt        :: mx a -> m a
  addLocal     :: A.Name -> val -> (val -> m a) -> m a
  addLocal x t  = doCxt . addBind x t cont
-}
  addLocal'    :: val -> val -> (val -> m a) -> m a
  addLocal' b   = addLocal systemGeneratedName

  lookupGlobal :: Name -> m val -- ^ lookup in signature
  lookupIdent  :: Ident -> m val 
  lookupIdent (Var x) = lookupLocal x
  lookupIdent id      = lookupGlobal $ name id


{-
class (Monad m, Scoping.Scope m) => TypeCheck val m | m -> val where
  app       :: val -> val -> m val
  eval      :: Expr -> m val  
  addLocal   :: Name -> val -> (val -> m a) -> m a
  addLocal'  :: val -> val -> (val -> m a) -> m a
  lookupIdent :: Ident -> m val
--  lookupVar :: Name -> m val
-}

{-  Type checking   Gamma |- e <=: t

   Gamma |- e' :=> type   Gamma |- [|e'|] = a   Gamma, x:a |- e <=: b x    
   --------------------------------------------------------------------    
   Gamma |- \x:e'.e <=: Pi a b 

   Gamma, x:a |- e <=: b x    Gamma |- e :=> t' 
   -----------------------    ----------------- Gamma |- t = t'
   Gamma |- \xe <=: Pi a b    Gamma |- e <=: t
-}
check :: (Value fvar tyVal, MonadCheckExpr tyVal env me m) => Expr -> tyVal -> m ()  
check e t =
  case e of
--    Abs x e -> 
    Lam x mt e -> do
      whenMaybe mt $ \ te -> do
        checkType te
        equalType t =<< eval te
      case tyView t of
        VPi a b -> addLocal x a $ \ xv -> check e =<< (b `app` xv) 
        _       -> fail $ "not a function type"
    e -> equalType t =<< infer e

{-  Type inference   Gamma |- e :=> t 

                              Gamma |- f :=> Pi a b    Gamma |- e <=: a
   -----------------------    -----------------------------------------
   Gamma |- x :=> Gamma(x)    Gamma |- f e :=> b [|e|]

                              Gamma |- e :=> type   Gamma, x:[|e|] |- e' :=> s
   ----------------------     ------------------------------------------------
   Gamma |- type :=> kind     Gamma |- Pi x:e. e' :=> s  
-}
infer :: (Value fvar tyVal, MonadCheckExpr tyVal env me m) => Expr -> m tyVal
infer e =
  case e of
--    Var x -> lookupVar x
    Ident x -> lookupIdent x
    Lam x (Just t) e -> do
      checkType t
      a <- eval t
      addLocal x a $ \ xv -> do
        bx <- infer e
        abstrPi a xv bx        
    App f e -> do
      t <- infer f
      case tyView t of
        VPi a b -> do
          check e a
          app b =<< eval e
        _ -> fail $ "not a function type"
    Typ -> return $ kind
{-
    Sort Type -> return $ kind
    Sort Kind -> fail $ "internal error: infer Kind"
-}
    Pi mx e e' -> do
      checkType e
      a <- eval e
      case mx of 
        Nothing -> infer e'
        Just x  -> addLocal x a $ \ xv -> infer e'
    _ -> fail $ "cannot infer type"
--    _ -> failDoc $ text "cannot infer type of" <+> prettyM e
        
checkType :: (Value fvar tyVal, MonadCheckExpr tyVal env me m) => Expr -> m ()
checkType e = isType =<< infer e

inferType :: (Value fvar tyVal, MonadCheckExpr tyVal env me m) => Expr -> m Sort
inferType e = do
  t <- infer e
  case tyView t of
    VSort s -> return s
    _       -> fail "neither a type nor a kind"

isType :: (Value fvar val, MonadCheckExpr val env me m) => val -> m ()
isType t = 
  case tyView t of
    VSort Type -> return ()
    _          -> fail $ "not a type"
 
equalType ::  (Value fvar val, MonadCheckExpr val env me m) => val -> val -> m ()
equalType t1 t2 = 
  case (tyView t1, tyView t2) of
    (VSort s1, VSort s2) | s1 == s2 -> return ()
    (VPi a1 b1, VPi a2 b2) -> do
      equalType a1 a2
      addLocal' b1 a1 $ \ xv -> do
        b1' <- app b1 xv
        b2' <- app b2 xv
        equalType b1' b2' 
    (VBase, VBase) -> equalBase t1 t2 
    _ -> fail $ "types unequal"

equalBase :: (Value fvar val, MonadCheckExpr val env me m) => val -> val -> m ()
equalBase v1 v2 = 
  case (tmView v1, tmView v2) of
    (VNe x1 t1 vs1, VNe x2 t2 vs2) -> 
      if x1 == x2 then equalApp t1 vs1 vs2 >> return ()
       else fail $ "head mismatch"

equalApp :: (Value fvar val, MonadCheckExpr val env me m) => val -> [val] -> [val] -> m val
equalApp t vs1 vs2 =
  case (vs1, vs2) of
    ([], []) -> return t
    (v1:vs1, v2:vs2) -> case tyView t of
      VPi a b -> do
        equalTm a v1 v2
        b' <- app b v1
        equalApp b' vs1 vs2
    _ -> fail $ "unequal length of argument vector"

equalTm :: (Value fvar val, MonadCheckExpr val env me m) => val -> val -> val -> m ()
equalTm t v1 v2 =
  case tyView t of
    VPi a b -> addLocal' b a $ \ xv -> do
      v1' <- app v1 xv
      v2' <- app v2 xv
      b'  <- app b  xv
      equalTm b' v1' v2'
    _ -> equalBase v1 v2 >> return ()

-- * Typechecking declarations.

class (Monad m, -- Scoping.Scope m, 
       MonadSig  val m, 
       MonadEval val env me, 
       MonadCheckExpr val env me mc) => 
  MonadCheckDecl val env me mc m | m -> mc, m -> me where

  doCheckExpr :: mc a -> m a
{-
  checkExpr   :: Value fvar val => Expr -> val -> m ()
  checkExpr e t = doCheckExpr $ check e t
--  checkType   :: Expr -> m ()         -- ^ is it a well-formed type or kind?
--  checkType t  = doCheckExpr $ infer t >> return ()
-}
  evalExpr    :: Expr -> m val
  evalExpr     = doCheckExpr . doEval . evaluate' 

checkDecl :: (Value fvar val, MonadCheckDecl val env me mc m) => Declaration -> m ()
checkDecl d = 
  case d of
    TypeSig n t -> do
      doCheckExpr $ inferType t
      t <- evalExpr t
      addCon n t
    Defn n Nothing e -> do
      t <- doCheckExpr $ infer e
      e <- evalExpr e
      addDef n t e
    Defn n (Just t) e -> do
      doCheckExpr $ inferType t
      t <- evalExpr t
      doCheckExpr $ check e t
      e <- evalExpr e
      addDef n t e