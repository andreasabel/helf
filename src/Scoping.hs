{- | Translation from Concrete to Abstract and back -}

module Scoping where

import Prelude hiding (mapM,print)

import Control.Applicative
import Data.Traversable

import qualified Concrete as C
import qualified Abstract as A
import qualified OperatorPrecedenceParser as O

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

type Item = O.Item Int A.Expr

instance Parse C.Atom A.Atom where
  parse a = 
    case a of
      C.Typ     -> return $ A.Typ
      C.Ident n -> getAtom n

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

instance Print C.Atom A.Atom where

instance O.Juxtaposition A.Expr where
  juxtaposition = A.App

parseApplication :: Scope m => [Item] -> m A.Expr
parseApplication is =  
  case O.parseApplication is of
    Left err -> parseError err 
    Right e  -> return e

printApplication :: A.Expr -> [A.Expr] 
printApplication = undefined