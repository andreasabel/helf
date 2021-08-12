{- | An instance of the Scope monad constructed from monad transformers -}

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, UndecidableInstances #-}
{-# LANGUAGE NondecreasingIndentation #-}

module ScopeMonad where

import Control.Applicative
import Control.Monad.Except  hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.State  hiding (mapM)

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Concrete as C
import qualified Abstract as A

import Scoping
{-
import Scoping hiding (ParseError)
import qualified Scoping
type ParseError = Scoping.ParseError
-}
import Util

-- * implementation of scoping monad by reader/state/error

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

- a map from A.Name to C.Name

-}

-- * scope reader

data ScopeState = ScopeState
  { counter   :: A.UID
  , fixities  :: Map C.Name C.Fixity  -- assuming there are few fix.decls.
  , renaming  :: Map C.Name A.Ident
  , naming    :: Map A.UID C.Name    -- name suggestion
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

instance Field ScopeState ScopeState where
  getF = id
  setF = const
  modF = id

{-
instance ( Applicative m
         , Field ScopeState st
         , MonadReader st m    ) => ScopeReader m where

  askName x = maybe (internalError ["unbound abstract identifier", show x]) id .
                Map.lookup (A.uid x) . naming   . getF <$> ask

  askFixity x = Map.lookup x . fixities . getF <$> ask
-}

{-
instance ( Applicative m
         , MonadState ScopeState m    ) => ScopeReader m where

  getName x = maybe (internalError ["unbound abstract identifier", show x]) id .
                Map.lookup x . naming   <$> get

  getFixity x = Map.lookup x . fixities <$> get
-}

-- * full scope monad

data ScopeContext = ScopeContext
  { localRen :: Map C.Name A.Ident
--  , localNam :: Map A.Name C.Name
  }

initScopeContext = ScopeContext
  { localRen = Map.empty
--  , localNam = Map.empty
  }

instance ( Applicative m
         , Monad m
         , MonadReader ScopeContext m
         , MonadState  ScopeState   m
         , MonadError  ParseError   m ) => Scope m where

  addGlobal ident n = do
    st@ScopeState { counter = i, renaming = ren, naming = nam } <- get
    let mx   = Map.lookup n ren -- TODO: shadowing!!
        x    = A.Name i n
        ren' = Map.insert n (ident x) ren
        nam' = Map.insert i n nam -- TODO: shadowing!
    put $ st { counter = (i + 1), renaming = ren', naming = nam' }
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

  addLocal mkId n cont = do
    st <- get
    let i = counter st
    let x = A.Name i n
    put $ st { counter = i + 1 , naming = Map.insert i n (naming st) }
    local (\ cxt -> cxt
      { localRen = Map.insert n (mkId x) (localRen cxt)
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
        case Map.lookup (A.uid x) nam of
          Just n -> return n
          Nothing -> error $ "internal error: unbound abstract identifier " ++ show x

  getFixity n = Map.lookup n <$> gets fixities

  getIdent n = do
    ren <- asks localRen
    (\ cont -> maybe cont return $ Map.lookup n ren) $ do
    ren <- gets renaming
    (\ cont -> maybe cont return $ Map.lookup n ren) $ do
    genericError $ "Unbound concrete identifier " ++ show n

  parseError = throwError
