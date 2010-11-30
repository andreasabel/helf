-- A generic bidirectional type-checker for LF

module TypeCheck where
  
type Name = String
type Type = Expr

data Sort 
  = Type
  | Kind -- only internally
    deriving (Eq, Ord, Show)

data Expr 
  = Var Name
  | App Expr Expr
  | Abs Name Expr
  | Pi  (Maybe Name) Type Type
  | Sort Sort
    deriving (Eq,Ord,Show)

data TyView val
  = VPi val val
  | VSort Sort
  | VBase 

data TmView fvar val
  = VNe  fvar val [val]  -- x^A vs
  | VVal                 -- Abs, Pi, Type, ...

class Eq fvar => Value fvar val | val -> fvar where
  typ     :: val
  kind    :: val
  freeVar :: fvar -> val -> val      -- typed free variable
  tyView  :: val -> TyView val
  tmView  :: val -> TmView fvar val

class Monad m => TypeCheck val m | m -> val where
  app       :: val -> val -> m val
  eval      :: Expr -> m val  
  addBind   :: Name -> val -> (val -> m a) -> m a
  addBind'  :: val -> val -> (val -> m a) -> m a
  lookupVar :: Name -> m val

{-  Type checking   Gamma |- e <=: t

   Gamma, x:a |- e <=: b x    Gamma |- e :=> t' 
   -----------------------    ----------------- Gamma |- t = t'
   Gamma |- \xe <=: Pi a b    Gamma |- e <=: t
-}
check :: (Value fvar tyVal, TypeCheck tyVal m) => Expr -> tyVal -> m ()  
check e t =
  case e of
    Abs x e -> 
      case tyView t of
        VPi a b -> addBind x a $ \ xv -> check e =<< (b `app` xv) 
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
infer :: (Value fvar tyVal, TypeCheck tyVal m) => Expr -> m tyVal
infer e =
  case e of
    Var x -> lookupVar x
    App f e -> do
      t <- infer f
      case tyView t of
        VPi a b -> do
          check e a
          app b =<< eval e
        _ -> fail $ "not a function type"
    Sort Type -> return $ kind
    Sort Kind -> fail $ "internal error: infer Kind"
    Pi mx e e' -> do
      checkType e
      a <- eval e
      case mx of 
        Nothing -> infer e'
        Just x  -> addBind x a $ \ xv -> infer e'
    _ -> fail $ "cannot infer type of " ++ show e
        
checkType :: (Value fvar tyVal, TypeCheck tyVal m) => Expr -> m ()
checkType e = isType =<< infer e

inferType :: (Value fvar tyVal, TypeCheck tyVal m) => Expr -> m Sort
inferType e = do
  t <- infer e
  case tyView t of
    VSort s -> return s
    _       -> fail "neither a type nor a kind"

isType :: (Value fvar val, TypeCheck val m) => val -> m ()
isType t = 
  case tyView t of
    VSort Type -> return ()
    _          -> fail $ "not a type"
 
equalType ::  (Value fvar val, TypeCheck val m) => val -> val -> m ()
equalType t1 t2 = 
  case (tyView t1, tyView t2) of
    (VSort s1, VSort s2) | s1 == s2 -> return ()
    (VPi a1 b1, VPi a2 b2) -> do
      equalType a1 a2
      addBind' b1 a1 $ \ xv -> do
        b1' <- app b1 xv
        b2' <- app b2 xv
        equalType b1' b2' 
    (VBase, VBase) -> equalBase t1 t2 
    _ -> fail $ "types unequal"

equalBase :: (Value fvar val, TypeCheck val m) => val -> val -> m ()
equalBase v1 v2 = 
  case (tmView v1, tmView v2) of
    (VNe x1 t1 vs1, VNe x2 t2 vs2) -> 
      if x1 == x2 then equalApp t1 vs1 vs2 >> return ()
       else fail $ "head mismatch"

equalApp :: (Value fvar val, TypeCheck val m) => val -> [val] -> [val] -> m val
equalApp t vs1 vs2 =
  case (vs1, vs2) of
    ([], []) -> return t
    (v1:vs1, v2:vs2) -> case tyView t of
      VPi a b -> do
        equalTm a v1 v2
        b' <- app b v1
        equalApp b' vs1 vs2
    _ -> fail $ "unequal length of argument vector"

equalTm :: (Value fvar val, TypeCheck val m) => val -> val -> val -> m ()
equalTm t v1 v2 =
  case tyView t of
    VPi a b -> addBind' b a $ \ xv -> do
      v1' <- app v1 xv
      v2' <- app v2 xv
      b'  <- app b  xv
      equalTm b' v1' v2'
    _ -> equalBase v1 v2 >> return ()

