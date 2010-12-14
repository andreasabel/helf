{- | Translation from Concrete to Abstract and back -}

{-# LANGUAGE UndecidableInstances #-}

module Scoping where

import Prelude hiding (mapM,print)

import Control.Applicative
import Data.Traversable

import qualified Concrete as C
import qualified Abstract as A
import qualified OperatorPrecedenceParser as O

import Util
import Text.PrettyPrint

-- * abstract scoping monad

type ParseError = O.ParseError

class (Applicative m, Monad m) => ScopeReader m where
  askName    :: A.Name -> m C.Name
  askFixity  :: C.Name -> m (Maybe C.Fixity)

class (Applicative m, Monad m) => Scope m where
  addGlobal  :: (A.Name -> A.Ident) -> C.Name -> m A.Name
  addCon     :: C.Name -> m A.Name
  addCon      = addGlobal A.Con
  addDef     :: C.Name -> m A.Name
  addDef      = addGlobal A.Def 
  addFixity  :: C.Name -> C.Fixity -> m ()
  addVar     :: C.Name -> (A.Name -> m a) -> m a
  getName    :: A.Name -> m C.Name
  getFixity  :: C.Name -> m (Maybe C.Fixity)
  getIdent   :: C.Name -> m A.Ident
--  getIdent n  = fst <$> getIdentAndFixity n  
--  getIdentAndFixity :: C.Name -> m (A.Ident, C.Fixity)
  parseError :: ParseError -> m a
  
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

class Pretty c => Unparse c a | a -> c where
  unparse :: ScopeReader m => a -> m c 
  prettyM :: ScopeReader m => a -> m Doc
  prettyM a = pretty <$> unparse a

{-
instance Unparse c a => Unparse (Maybe c) (Maybe a) where
  unparse = mapM unparse
-}

instance Unparse C.Declarations A.Declarations where
  unparse (A.Declarations adecls) = C.Declarations <$> mapM unparse adecls

instance Unparse C.Declaration A.Declaration where
  unparse adecl =
    case adecl of
      A.TypeSig n t -> C.TypeSig <$> askName n <*> unparse t
      A.Defn n mt e -> C.Defn <$> askName n <*> mapM unparse mt <*> unparse e

instance Unparse C.Expr A.Expr where
  unparse aexpr = 
    case aexpr of
      A.Ident a           -> C.Ident <$> unparse a
      A.Typ               -> return $ C.Typ
      A.Pi Nothing  t1 t2 -> C.Fun <$> unparse t1 <*> unparse t2
      A.Pi (Just x) t1 t2 -> C.Pi <$> askName x <*> unparse t1 <*> unparse t2
      A.Lam x mt e        -> C.Lam <$> askName x <*> mapM unparse mt <*> unparse e
      A.App{}             -> C.Apps <$> unparseApplication aexpr

instance Unparse C.Name A.Ident where
  unparse id = askName (A.name id)

unparseApplication :: ScopeReader m => A.Expr -> m [C.Expr] 
unparseApplication (A.App f a) = mapM unparse [f,a] -- TODO!