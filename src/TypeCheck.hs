-- A generic bidirectional type-checker for LF

module TypeCheck where

import Abstract -- as A
import Context
import PrettyM
-- import Scoping (prettyM)
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

-- * Type errors

data TypeError val 
  = NotFunType val       -- ^ not a function type
  | NotInferable Expr    -- ^ cannot infer type
  | NotSort val          -- ^ neither "type" nor "kind"
  | NotType val          -- ^ not "type"
  | NotAType Expr        -- ^ not a type
  | UnequalTypes val val -- ^ types unequal
  | UnequalHeads val val -- ^ neutral terms unequal
  | UnequalSpines [val] [val] -- ^ spines differ in length

instance PrettyM m val => PrettyM m (TypeError val) where
  prettyM err = 
    case err of
      NotFunType t      -> prettyM t <+> text "is not a function type"
      NotInferable e    -> text "cannot infer type of" <+> prettyM e
      NotSort s         -> prettyM s <+> text "is not a valid sort"
      NotType s         -> text "expected" <+> prettyM s <+> text "to be 'type'"
      NotAType e        -> text "expected" <+> prettyM e <+> text "to be a type"
      UnequalTypes t t' -> text "type mismatch" <+> prettyM t <+> text "!=" <+> prettyM t'
      UnequalHeads v v' -> text "head mismatch" <+> prettyM v <+> text "!=" <+> prettyM v'
      UnequalSpines vs vs' -> text "value mismatch, spines differ in length"

-- * Traces 

data TypeTrace val
  = Check Expr val
  | Infer Expr
  | EqualType val val

instance PrettyM m val => PrettyM m (TypeTrace val) where
  prettyM tr = 
    case tr of
      Check e t -> text "checking" <+> prettyM e <+> text "against" <+> prettyM t
      Infer e   -> text "inferring type of" <+> prettyM e
      EqualType t1 t2 -> text "checking wether" <+> prettyM t1 <+> text "equals" <+> prettyM t2

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
  abstrPi      :: val -> (Name, val) -> val -> m val  -- ^ pi a x b
  abstrPi a x b = doEval $ abstractPi a x b
{-
  doCxt        :: mx a -> m a
  addLocal     :: Name -> val -> (val -> m a) -> m a
  addLocal x t  = doCxt . addBind x t cont
-}
  addLocal'    :: val -> val -> (val -> m a) -> m a
  addLocal' b   = addLocal systemGeneratedName

  lookupGlobal :: Name -> m val -- ^ lookup in signature
  lookupIdent  :: Ident -> m val 
  lookupIdent (Var x) = lookupLocal x
  lookupIdent id      = lookupGlobal $ name id

  typeError    :: TypeError val -> m a
  newError     :: TypeError val -> m a -> m a
  typeTrace    :: TypeTrace val -> m a -> m a


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
check e t = typeTrace (Check e t) $
  case e of
--    Abs x e -> 
    Lam x mt e ->
      case tyView t of
        VPi a b -> do
          whenMaybe mt $ \ t -> do
            checkType t
            equalType a =<< eval t
          addLocal x a $ \ xv -> check e =<< (b `app` xv) 
        _       -> typeError $ NotFunType t
    e -> flip equalType t =<< infer e

{-  Type inference   Gamma |- e :=> t 

                              Gamma |- f :=> Pi a b    Gamma |- e <=: a
   -----------------------    -----------------------------------------
   Gamma |- x :=> Gamma(x)    Gamma |- f e :=> b [|e|]

                              Gamma |- e :=> type   Gamma, x:[|e|] |- e' :=> s
   ----------------------     ------------------------------------------------
   Gamma |- type :=> kind     Gamma |- Pi x:e. e' :=> s  
-}
infer :: (Value fvar tyVal, MonadCheckExpr tyVal env me m) => Expr -> m tyVal
infer e = typeTrace (Infer e) $ 
  case e of
--    Var x -> lookupVar x
    Ident x -> lookupIdent x
    Lam x (Just t) e -> do
      checkType t
      a <- eval t
      addLocal x a $ \ xv -> do
        bx <- infer e
        abstrPi a (x,xv) bx        
    App f e -> do
      t <- infer f
      case tyView t of
        VPi a b -> do
          check e a
          app b =<< eval e
        _ -> typeError $ NotFunType t
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
    _ -> typeError $ NotInferable e
--    _ -> failDoc $ text "cannot infer type of" <+> prettyM e
        
checkType :: (Value fvar tyVal, MonadCheckExpr tyVal env me m) => Expr -> m ()
checkType e = newError (NotAType e) $ isType =<< infer e

inferType :: (Value fvar tyVal, MonadCheckExpr tyVal env me m) => Expr -> m Sort
inferType e = do
  t <- infer e
  case tyView t of
    VSort s -> return s
    _       -> typeError $ NotSort t

isType :: (Value fvar val, MonadCheckExpr val env me m) => val -> m ()
isType t = 
  case tyView t of
    VSort Type -> return ()
    _          -> typeError $ NotType t
 
equalType ::  (Value fvar val, MonadCheckExpr val env me m) => val -> val -> m ()
equalType t1 t2 = typeTrace (EqualType t1 t2) $
  case (tyView t1, tyView t2) of
    (VSort s1, VSort s2) | s1 == s2 -> return ()
    (VPi a1 b1, VPi a2 b2) -> do
      equalType a1 a2
      addLocal' b1 a1 $ \ xv -> appM2 equalType (b1 `app` xv) (b2 `app` xv)
    (VBase, VBase) -> equalBase t1 t2 
    _ -> typeError $ UnequalTypes t1 t2

equalBase :: (Value fvar val, MonadCheckExpr val env me m) => val -> val -> m ()
equalBase v1 v2 = 
  case (tmView v1, tmView v2) of
    (VNe x1 t1 vs1, VNe x2 t2 vs2) -> 
      if x1 == x2 then equalApp vs1 vs2 t1 >> return ()
       else typeError $ UnequalHeads v1 v2

equalApp :: (Value fvar val, MonadCheckExpr val env me m) => [val] -> [val] -> val -> m val
equalApp vs1 vs2 t =
  case (vs1, vs2) of
    ([], []) -> return t
    (v1:vs1, v2:vs2) -> case tyView t of
      VPi a b -> do
        equalTm v1 v2 a
        equalApp vs1 vs2 =<< app b v1
    _ -> typeError $ UnequalSpines vs1 vs2

equalTm :: (Value fvar val, MonadCheckExpr val env me m) => val -> val -> val -> m ()
equalTm v1 v2 t =
  case tyView t of
    VPi a b -> addLocal' b a $ \ xv -> 
      appM3 equalTm (v1 `app` xv) (v2 `app` xv) (b `app` xv)
    _ -> equalBase v1 v2 

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