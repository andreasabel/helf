{-# LANGUAGE TupleSections, UndecidableInstances, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, FunctionalDependencies #-}

-- | A generic bidirectional type-checker for LF.

module TypeCheck where

import Control.Applicative

import Abstract -- as A
import Context
import PrettyM
import Signature
import Value
import Util

-- * Type errors

data TypeError val
  = NotFunType val            -- ^ not a function type
  | NotInferable Expr         -- ^ cannot infer type
  | NotSort val               -- ^ neither "type" nor "kind"
  | NotType val               -- ^ not "type"
  | NotAType Expr             -- ^ not a type
  | UnequalTypes val val      -- ^ types unequal
  | UnequalHeads val val      -- ^ neutral terms unequal
  | UnequalSpines [val] [val] -- ^ spines differ in length

instance PrettyM m val => PrettyM m (TypeError val) where
  prettyM err =
    case err of
      NotFunType t      -> prettyM t <+> text "is not a function type"
      NotInferable e    -> text "cannot infer type of" <+> prettyM e
      NotSort s         -> prettyM s <+> text "is not a valid sort"
      NotType s         -> text "expected" <+> prettyM s <+> text "to be 'type'"
      NotAType e        -> text "expected" <+> prettyM e <+> text "to be a type"
      UnequalTypes t t' -> text "mismatch" <+> prettyM t <+> text "!=" <+> prettyM t'
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
--
-- Needs only read-only access to signature.

class (Monad m, -- Scoping.Scope m,
       Alternative m,
       MonadEval fvar val env me,
       MonadCxt val env m) =>
  MonadCheckExpr fvar val env me m | m -> me where

  -- evaluation stuff
  doEval       :: me a -> m a      -- ^ run evaluation
  -- {-# INLINE doEval #-}
  app          :: val -> val -> m val
  app f v       = doEval $ apply f v
  eval         :: Expr -> m val
  eval e        = getEnv >>= \ rho -> doEval $ evaluate e rho
  force        :: val -> m val
  force v       = doEval $ unfolds v
  unfold1      :: val -> m val
  unfold1 v     = doEval $ unfold v
  abstrPi      :: val -> (Name, val) -> val -> m val  -- ^ pi a x b
  abstrPi a x b = doEval $ abstractPi a x b
  view         :: val -> m (ValView fvar val)
  view          = doEval . valView
  -- | Shortcut check for identity.  Default: no shortcut.
  unlessId     :: val -> val -> m () -> m ()
  unlessId _ _ cont = cont
  -- | Assigning equal things to same ref.
  equate       :: val -> val -> m ()
  equate _ _    = return ()
{-
  doCxt        :: mx a -> m a
  addLocal     :: Name -> val -> (val -> m a) -> m a
  addLocal x t  = doCxt . addBind x t cont
-}

  -- context and signature
  addLocal'    :: val -> val -> (val -> m a) -> m a
  addLocal' b   = addLocal systemGeneratedName

  lookupGlobal :: Name -> m val -- ^ lookup in signature
  lookupIdent  :: Ident -> m val
  lookupIdent (Var x) = lookupLocal x
  lookupIdent id      = lookupGlobal $ name id

  -- error and debugging
  typeError    :: TypeError val -> m a
  newError     :: TypeError val -> m a -> m a
  -- handleError  :: m a -> (TypeError val -> m a) -> m a
  typeTrace    :: TypeTrace val -> m a -> m a
  typeTrace tr cont = cont   -- default: do not trace type checker
  traceEval    :: m val -> m val
  traceEval cont = cont      -- default: do not print evaluated term

  -- local (lazily checked) let
  addLet       :: Name -> Expr -> m a -> m a
  addLet       = error "NYI: addLet"

  checkLet     :: Name -> val  -> (Expr -> val -> m ()) ->
                                  (val  -> val -> m ()) -> m ()
  checkLet     = error "NYI: checkLet"

  inferLet     :: Name -> (Expr -> m val) -> m val
  inferLet     = error "NYI: inferLet"

    -- check let bound var x.
    -- 1st continuation: If it does not have a type yet, call type checker.
    -- 2nd continuation: If it has already a type, call equality checker.

{-  Type checking   Gamma |- e <=: t

   Gamma |- e' :=> type   Gamma |- [|e'|] = a   Gamma, x:a |- e <=: b x
   --------------------------------------------------------------------
   Gamma |- \x:e'.e <=: Pi a b

   Gamma, x:a |- e <=: b x    Gamma |- e :=> t'
   -----------------------    ----------------- Gamma |- t = t'
   Gamma |- \xe <=: Pi a b    Gamma |- e <=: t
-}
check :: (MonadCheckExpr fvar tyVal env me m) => Expr -> tyVal -> m ()
check e t = typeTrace (Check e t) $
  case e of
    Ident (Let x) -> checkLet x t check equalType
    LLet x e e' -> addLet x e $ check e' t
    Lam x mt e -> do
      t' <- view =<< force t       -- if t is a definition unfold it to expose Pi
      case t' of
        VPi a b -> do
          whenMaybe mt $ \ t -> do
            checkType t
            equalType a =<< eval t
          addLocal x a $ \ xv -> check e =<< (b `app` xv)
        _       -> typeError $ NotFunType t
    e -> flip equalType t =<< infer e

{-  Type inference   Gamma |- e :=> t

   -----------------------
   Gamma |- c :=> Sigma(c)

                              Gamma |- f :=> Pi a b    Gamma |- e <=: a
   -----------------------    -----------------------------------------
   Gamma |- x :=> Gamma(x)    Gamma |- f e :=> b [|e|]


   Gamma |- e :=> type   a =[|e|]   Gamma, x:a |- e' :=> b
   -------------------------------------------------------
   Gamma |- \x:e.e' :=> Pi a \x.b

                              Gamma |- e :=> type   Gamma, x:[|e|] |- e' :=> s
   ----------------------     ------------------------------------------------
   Gamma |- type :=> kind     Gamma |- Pi x:e. e' :=> s
-}
infer :: (MonadCheckExpr fvar tyVal env me m) => Expr -> m tyVal
infer e = typeTrace (Infer e) $
  case e of
    Ident (Let x) -> inferLet x infer
    LLet x e e'    -> addLet x e $ infer e'
    Ident x -> lookupIdent x
    Lam x (Just t) e -> do
      checkType t
      a <- eval t
      addLocal x a $ \ xv -> do
        bx <- infer e
        abstrPi a (x,xv) bx
    App f e -> do
      t  <- infer f
      t' <- view =<< force t
      case t' of
        VPi a b -> do
          check e a
          app b =<< eval e
        _ -> typeError $ NotFunType t
    Typ -> doEval $ kind
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

checkType :: (MonadCheckExpr fvar tyVal env me m) => Expr -> m ()
checkType e = newError (NotAType e) $ isType =<< infer e

inferType :: (MonadCheckExpr fvar tyVal env me m) => Expr -> m Sort
inferType e = do
  t  <- infer e
  t' <- view =<< force t
  case t' of
    VSort s -> return s
    _       -> typeError $ NotSort t

isType :: (MonadCheckExpr fvar val env me m) => val -> m ()
isType t = do
  t' <- view =<< force t
  case t' of
    VSort Type -> return ()
    _          -> typeError $ NotType t

-- | Equality of types.
equalType ::  (MonadCheckExpr fvar val env me m) => val -> val -> m ()
equalType t1 t2 = typeTrace (EqualType t1 t2) $ unlessId t1 t2 $ do
  equalBase t1 t2
  equate t1 t2

-- | Equality at base type/sort.
equalBase ::  (MonadCheckExpr fvar val env me m) => val -> val -> m ()
equalBase v1 v2 = unlessId v1 v2 $ do
  let ret = equate v1 v2
  w1 <- view v1
  w2 <- view v2
  case (w1, w2) of
    (VSort s1, VSort s2) | s1 == s2 -> ret
    (VPi a1 b1, VPi a2 b2) -> do
      equalBase a1 a2
      addLocal' b1 a1 $ \ xv -> appM2 equalBase (b1 `app` xv) (b2 `app` xv)
      ret
    (VNe x1 t1 vs1, VNe x2 t2 vs2) ->
      if x1 == x2 then equalApp vs1 vs2 t1 >> ret
       else typeError $ UnequalHeads v1 v2
    (VDef x1 t1 vs1, VDef x2 t2 vs2) ->
      case compare x1 x2 of
        EQ -> (equalApp vs1 vs2 t1 >> ret)
              <|> (appM2 equalBase (unfold1 v1) (unfold1 v2) >> ret)
        -- unfold newer definition first (Coq heuristics)
        GT -> equalBase v1 =<< unfold1 v2
        LT -> unfold1 v1 >>= \ v1 -> equalBase v1 v2
    (VDef{}, _) -> unfold1 v1 >>= \ v1 -> equalBase v1 v2 >> ret
    (_, VDef{}) -> (equalBase v1 =<< unfold1 v2) >> ret
    _ -> typeError $ UnequalTypes v1 v2

-- | Pointwise equality of spines.
equalApp :: (MonadCheckExpr fvar val env me m) => [val] -> [val] -> val -> m val
equalApp vs1 vs2 t =
  case (vs1, vs2) of
    ([], []) -> return t
    (v1:vs1, v2:vs2) -> do
      t <- view =<< force t
      case t of
        VPi a b -> do
          equalTm v1 v2 a
          equalApp vs1 vs2 =<< app b v1
    _ -> typeError $ UnequalSpines vs1 vs2

-- | Type directed equality of terms.
equalTm :: (MonadCheckExpr fvar val env me m) => val -> val -> val -> m ()
equalTm v1 v2 t = unlessId v1 v2 $ do
  w <- view =<< force t
  case w of
    VPi a b -> addLocal' b a $ \ xv ->
      appM3 equalTm (v1 `app` xv) (v2 `app` xv) (b `app` xv)
    _ -> equalBase v1 v2

-- * Typechecking declarations.
--
-- Needs read and write access to signature Sigma.

class (Monad m, -- Scoping.Scope m,
       MonadSig  val m,
       MonadEval fvar val env me,
       MonadCheckExpr fvar val env me mc) =>
  MonadCheckDecl fvar val env me mc m | m -> mc, m -> me where

  doCheckExpr :: mc a -> m a -- ^ monad lifting
  -- {-# INLINE doCheckExpr #-}

  evalExpr    :: Expr -> m val
  evalExpr     = doCheckExpr . traceEval . doEval . evaluate'

{- Rules for checking declarations

  Sigma; . |- t :=> s
  ------------------------------------
  Sigma |- (c : t) => (Sigma, c:[|t|])

  Sigma; . |- e :=> a
  ----------------------------------------------
  Sigma |- (d = e) => (Sigma, d:a=[|e|])

  Sigma; . |- t :=> s    Sigma; . |- e <=: [|t|]
  ----------------------------------------------
  Sigma |- (d : t = e) => (Sigma, d:[|t|]=[|e|])

-}
checkDecl :: (MonadCheckDecl fvar val env me mc m) => Declaration -> m ()
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
