{- | Translation from Concrete to Abstract and back -}

{-# LANGUAGE UndecidableInstances #-}

module Scoping where

import Prelude hiding (mapM,print)

import Control.Applicative
import Control.Monad.Error  hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.State  hiding (mapM)

import Data.Traversable
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Concrete as C
import qualified Abstract as A
import qualified OperatorPrecedenceParser as O

-- * abstract scoping monad

type ParseError = O.ParseError

class (Applicative m, Monad m) => Scope m where
  addGlobal  :: (A.Name -> A.Ident) -> C.Name -> m A.Name
  addCon     :: C.Name -> m A.Name
  addCon      = addGlobal A.Con
  addDef     :: C.Name -> m A.Name
  addDef      = addGlobal A.Def 
  addFixity  :: C.Name -> C.Fixity -> m ()
  addVar     :: C.Name -> (A.Name -> m a) -> m a
  getName    :: A.Name -> m C.Name
--  getIdentAndFixity :: C.Name -> m (A.Ident, C.Fixity)
  getFixity  :: C.Name -> m (Maybe C.Fixity)
  getIdent   :: C.Name -> m A.Ident
--  getIdent n  = fst <$> getIdentAndFixity n
  parseError :: ParseError -> m a

-- * implementation of scoping monad by state/error

{- Scoping need the following components

- a map from C.Name to A.Name
- a map from A.Name to (A.Ident, C.Fixity)

  this resolves the parsed concrete names into unique identifiers
  with their fixity 

  My view: A fixity can only be set before the name is first used.
  Twelf  : A fixity can be set and reset at any time.
           Thus, a fixity is attached to the concrete name and not 
           the abstract name.

  I assume the set of fixity declarations to be small, thus, it does
  not matter if there are separate lookups for fixity and abstract name.

- a map from A.Name to 

-}
data ScopeState = ScopeState
  { counter   :: A.Name
  , fixities  :: Map C.Name C.Fixity  -- assuming there are few fix.decls.
  , renaming  :: Map C.Name A.Ident
  , naming    :: Map A.Name C.Name    -- name suggestion
  }

initScopeState = ScopeState
  { counter = 0
  , fixities = Map.empty
  , renaming = Map.empty
  , naming   = Map.empty
  }

{-
data ScopeEntry = ScopeEntry
  { ident   :: A.Ident -- ^ to increase sharing, keep the whole ident here
  , fixity :: C.Fixity
  , name   :: C.Name -- ^ needs to be changed if shadowed 
  }
-}
data ScopeContext = ScopeContext
  { localRen :: Map C.Name A.Ident
--  , localNam :: Map A.Name C.Name
  }

initScopeContext = ScopeContext
  { localRen = Map.empty
--  , localNam = Map.empty
  }

instance (Applicative m, MonadReader ScopeContext m, MonadState ScopeState m, MonadError O.ParseError m) => Scope m where

  addGlobal ident n = do
    st@ScopeState { counter = x, renaming = ren, naming = nam } <- get
    let mx   = Map.lookup n ren -- TODO: shadowing!!
        ren' = Map.insert n (ident x) ren
        nam' = Map.insert x n nam -- TODO: shadowing!
    put $ st { counter = (x + 1), renaming = ren', naming = nam' }
    return x
{-
  addCon n = do
    ScopeState { counter = x, renaming = ren, naming = nam } <- get
    let mx   = Map.lookup n ren -- TODO: shadowing!!
        ren' = Map.insert n x ren
        it   = ScopeEntry 
                { ident = A.Con x 
                , fixity = O.Nofix
                , name = n -- TODO: shadowing!
                }
        nam' = Map.insert x it nam
    put $ ScopeState (x + 1) ren' nam'
    return x
-} 

  addFixity n fx = modify $ \ st -> st { fixities = Map.insert n fx (fixities st) } 

  addVar n cont = do
    st <- get
    let x = counter st
    put $ st { counter = x + 1 , naming = Map.insert x n (naming st) }
    local (\ cxt -> cxt 
      { localRen = Map.insert n (A.Var x) (localRen cxt)
--      , localNam = Map.insert x n (localNam cxt) -- TODO: shadowing!
      }) $ cont x

{-
  addVar n cont = do
    st <- get
    let x = counter st
    put $ st { counter = x + 1 }
    local (\ cxt -> cxt 
      { localRen = Map.insert n (A.Var x) (localRen cxt)
      , localNam = Map.insert x n (localNam cxt) -- TODO: shadowing!
      }) $ cont x
-}

  getName x = do
{-
    nam <- asks localNam
    case Map.lookup x nam of
      Just n -> return n
      Nothing -> do
-}
        nam <- gets naming
        case Map.lookup x nam of
          Just n -> return n
          Nothing -> fail $ "error unbound abstract identifier " ++ show x

  getFixity n = Map.lookup n <$> gets fixities

  getIdent n = do
    ren <- asks localRen
    (\ cont -> maybe cont return $ Map.lookup n ren) $ do
    ren <- gets renaming
    (\ cont -> maybe cont return $ Map.lookup n ren) $ do
    fail $ "error unbound concrete identifier " ++ show n

  parseError = throwError
  
-- * parsing

class Parse c a where 
  parse :: Scope m => c -> m a

instance Parse c a => Parse (Maybe c) (Maybe a) where
  parse = mapM parse

instance Parse C.Declarations A.Declarations where
  parse (C.Declarations cdecls) = A.Declarations . concat <$> mapM parse cdecls

instance Parse C.Declaration [A.Declaration] where
  parse cdecl = 
    case cdecl of
      C.TypeSig n t -> return <$> (A.TypeSig <$> addCon n <*> parse t)
      C.Defn n mt e -> return <$> (A.Defn <$> addDef n <*> parse mt <*> parse e)
      C.Fixity n fx -> const [] <$> addFixity n fx

{-
instance Parse C.Atom A.Atom where
  parse a = 
    case a of
      C.Typ     -> return $ A.Typ
      C.Ident n -> getAtom n
-}

instance Parse C.Expr A.Expr where
  parse cexpr =
    case cexpr of
      C.Typ         -> return $ A.Typ
      C.Ident n     -> A.Ident <$> getIdent n
--      C.Atom a     -> A.Atom <$> parse a
      C.Fun t1 t2  -> A.Pi Nothing <$> parse t1 <*> parse t2
      C.Pi x t1 t2 -> do
        t1 <- parse t1
        addVar x $ \ x -> A.Pi (Just x) t1 <$> parse t2
      C.Lam x mt e -> do
        mt <- parse mt
        addVar x $ \ x -> A.Lam x mt <$> parse e
      C.Apps es -> parseApplication =<< mapM parse es

-- applications [C.Expr] are parsed into list of Stack items 
-- which is then resolved into an A.Expr

type Item = O.Item Int A.Expr

{- 
instance Parse C.Atom Item where
  parse catom = 
    case catom of
      C.Typ     -> return $ O.Atom (A.Atom A.Typ)
      C.Ident n -> do
        (a, fx) <- getAtomAndFixity n
        return $ case fx of
          O.Nofix   -> O.Atom $ A.Atom a
          O.Infix{} -> O.Op fx (\ [x,y] -> A.Atom a `A.App` x `A.App` y)
          _         -> O.Op fx (\ [x]   -> A.Atom a `A.App` x)
-}
{-
instance Parse C.Ident Item where
  parse n = do
        (a, fx) <- getIdentAndFixity n
        return $ case fx of
          O.Nofix   -> O.Ident $ A.Ident a
          O.Infix{} -> O.Op fx (\ [x,y] -> A.Ident a `A.App` x `A.App` y)
          _         -> O.Op fx (\ [x]   -> A.Ident a `A.App` x)
-}

instance Parse C.Expr Item where
  parse cexpr =
    case cexpr of
      C.Ident n -> do
        a  <- getIdent n
        mfx <- getFixity n
        return $ case mfx of
          Nothing           -> O.Atom $ A.Ident a
          Just fx@O.Infix{} -> O.Op fx (\ [x,y] -> A.Ident a `A.App` x `A.App` y)
          Just fx           -> O.Op fx (\ [x]   -> A.Ident a `A.App` x)
      _         -> O.Atom <$> parse cexpr

instance O.Juxtaposition A.Expr where
  juxtaposition = A.App

parseApplication :: Scope m => [Item] -> m A.Expr
parseApplication is =  
  case O.parseApplication is of
    Left err -> parseError err 
    Right e  -> return e

-- * unparsing

class Unparse c a where
  unparse :: Scope m => a -> m c 

instance Unparse c a => Unparse (Maybe c) (Maybe a) where
  unparse = mapM unparse

instance Unparse C.Declarations A.Declarations where
  unparse (A.Declarations adecls) = C.Declarations <$> mapM unparse adecls

instance Unparse C.Declaration A.Declaration where
  unparse adecl =
    case adecl of
      A.TypeSig n t -> C.TypeSig <$> getName n <*> unparse t
      A.Defn n mt e -> C.Defn <$> getName n <*> unparse mt <*> unparse e

instance Unparse C.Expr A.Expr where
  unparse aexpr = 
    case aexpr of
      A.Ident a           -> C.Ident <$> unparse a
      A.Typ               -> return $ C.Typ
      A.Pi Nothing  t1 t2 -> C.Fun <$> unparse t1 <*> unparse t2
      A.Pi (Just x) t1 t2 -> C.Pi <$> getName x <*> unparse t1 <*> unparse t2
      A.Lam x mt e        -> C.Lam <$> getName x <*> unparse mt <*> unparse e
      A.App{}             -> C.Apps <$> unparseApplication aexpr

instance Unparse C.Name A.Ident where
  unparse id = getName (A.name id)

unparseApplication :: Scope m => A.Expr -> m [C.Expr] 
unparseApplication (A.App f a) = mapM unparse [f,a] -- TODO!