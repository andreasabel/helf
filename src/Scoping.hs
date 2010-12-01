{- | Translation from Concrete to Abstract and back -}

module Scoping where

import Prelude hiding (mapM,print)

import Control.Applicative
import Data.Traversable

import qualified Concrete as C
import qualified Abstract as A

class (Applicative m, Monad m) => Scope m where
  addCon    :: C.Name -> m A.Name
  addDef    :: C.Name -> m A.Name 
  addFixity :: C.Name -> C.Fixity -> m ()
  addVar    :: C.Name -> (A.Name -> m a) -> m a
  getName   :: A.Name -> m C.Name
{-
  addCon :: C.Name -> A.Type -> m A.Name
  addDef :: C.Name -> Maybe (A.Type) -> A.Expr -> m A.Name 
-}

class Parse c a where 
  parse :: Scope m => c -> m a

class Print c a where
  print :: Scope m => a -> m c 

instance Parse c a => Parse (Maybe c) (Maybe a) where
  parse = mapM parse

instance Print c a => Print (Maybe c) (Maybe a) where
  print = mapM print

instance Parse C.Declaration [A.Declaration] where
  parse cdecl = 
    case cdecl of
      C.TypeSig n t -> return <$> (A.TypeSig <$> addCon n <*> parse t)
      C.Defn n mt e -> return <$> (A.Defn <$> addDef n <*> parse mt <*> parse e)
      C.Fixity n fx -> const [] <$> addFixity n fx

instance Print C.Declaration A.Declaration where
  print adecl =
    case adecl of
      A.TypeSig n t -> C.TypeSig <$> getName n <*> print t
      A.Defn n mt e -> C.Defn <$> getName n <*> print mt <*> print e

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

instance Print C.Expr A.Expr where
  print aexpr = 
    case aexpr of
      A.Atom a            -> C.Atom <$> print a
      A.Pi Nothing  t1 t2 -> C.Fun <$> print t1 <*> print t2
      A.Pi (Just x) t1 t2 -> C.Pi <$> getName x <*> print t1 <*> print t2
      A.Lam x mt e        -> C.Lam <$> getName x <*> print mt <*> print e
      A.App{}             -> C.Apps <$> mapM print (printApplication aexpr)

instance Parse C.Atom A.Atom where
instance Print C.Atom A.Atom where

-- monadic because it throws error
parseApplication :: Scope m => [A.Expr] -> m A.Expr
parseApplication = undefined

printApplication :: A.Expr -> [A.Expr] 
printApplication = undefined