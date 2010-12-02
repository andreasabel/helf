{- | Translation from Concrete to Abstract and back -}

module Scoping where

import Prelude hiding (mapM,print)

import Control.Applicative
import Data.Traversable

import qualified Concrete as C
import qualified Abstract as A
import qualified OperatorPrecedenceParser as O

-- * abstract scoping monad

class (Applicative m, Monad m) => Scope m where
  addCon    :: C.Name -> m A.Name
  addDef    :: C.Name -> m A.Name 
  addFixity :: C.Name -> C.Fixity -> m ()
  addVar    :: C.Name -> (A.Name -> m a) -> m a
  getName   :: A.Name -> m C.Name
  getAtomAndFixity :: C.Name -> m (A.Atom, C.Fixity)
  getAtom   :: C.Name -> m A.Atom
  getAtom n  = fst <$> getAtomAndFixity n
  parseError :: O.ParseError -> m a

-- * implementation of scoping monad by state/error

{- Scoping need the following components

- a map from C.Name to A.Name
- a map from A.Name to (A.Atom, C.Fixity)

  this resolves the parsed concrete names into unique identifiers
  with their fixity 

  My view: A fixity can only be set before the name is first used.
  Twelf  : A fixity can be set and reset at any time.

- a map from A.Name to 

-}
data ScopeState = ScopeState
  { 
  }

-- * parsing
class Parse c a where 
  parse :: Scope m => c -> m a

instance Parse c a => Parse (Maybe c) (Maybe a) where
  parse = mapM parse

instance Parse C.Declaration [A.Declaration] where
  parse cdecl = 
    case cdecl of
      C.TypeSig n t -> return <$> (A.TypeSig <$> addCon n <*> parse t)
      C.Defn n mt e -> return <$> (A.Defn <$> addDef n <*> parse mt <*> parse e)
      C.Fixity n fx -> const [] <$> addFixity n fx

instance Parse C.Atom A.Atom where
  parse a = 
    case a of
      C.Typ     -> return $ A.Typ
      C.Ident n -> getAtom n

instance Parse C.Expr A.Expr where
  parse cexpr =
    case cexpr of
      C.Atom a     -> A.Atom <$> parse a
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

instance Parse C.Atom Item where
  parse catom = 
    case catom of
      C.Typ     -> return $ O.Atom (A.Atom A.Typ)
      C.Ident n -> do
        (a, fx) <- getAtomAndFixity n
        return $ case fx of
          O.Infix{} -> O.Op fx (\ [x,y] -> A.Atom a `A.App` x `A.App` y)
          _         -> O.Op fx (\ [x]   -> A.Atom a `A.App` x)

instance Parse C.Expr Item where
  parse cexpr =
    case cexpr of
      C.Atom a -> parse a
      _        -> O.Atom <$> parse cexpr

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

instance Unparse C.Declaration A.Declaration where
  unparse adecl =
    case adecl of
      A.TypeSig n t -> C.TypeSig <$> getName n <*> unparse t
      A.Defn n mt e -> C.Defn <$> getName n <*> unparse mt <*> unparse e

instance Unparse C.Expr A.Expr where
  unparse aexpr = 
    case aexpr of
      A.Atom a            -> C.Atom <$> unparse a
      A.Pi Nothing  t1 t2 -> C.Fun <$> unparse t1 <*> unparse t2
      A.Pi (Just x) t1 t2 -> C.Pi <$> getName x <*> unparse t1 <*> unparse t2
      A.Lam x mt e        -> C.Lam <$> getName x <*> unparse mt <*> unparse e
      A.App{}             -> C.Apps <$> mapM unparse (unparseApplication aexpr)

instance Unparse C.Atom A.Atom where

unparseApplication :: A.Expr -> [A.Expr] 
unparseApplication = undefined