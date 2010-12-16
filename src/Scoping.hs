{- | Translation from Concrete to Abstract and back -}

{-# LANGUAGE UndecidableInstances #-}

module Scoping (Scope(..),Parse(..),ParseError,Print(..)) where

import Prelude hiding (mapM,print)

import Control.Applicative
import Control.Monad.State hiding (mapM)

import Data.Map (Map)
import qualified Data.Map as Map 
import Data.Set (Set)
import qualified Data.Set as Set 

import Data.Traversable

import qualified Concrete as C
import qualified Abstract as A
import qualified OperatorPrecedenceParser as O

import Util
import Text.PrettyPrint

-- * abstract scoping monad

type ParseError = O.ParseError

{-
class (Applicative m, Monad m) => ScopeReader m where
  askName    :: A.Name -> m C.Name
  askFixity  :: C.Name -> m (Maybe C.Fixity)
-}

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

{-
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
-}

{- How to print an expression

We distinguish 3 kinds of abstract names
- global names (Con,Def)
- user-generated local names 
- system-generated local names (e.g., from quote)

We can print an expression from left-to-right, bottom-up as follows:

a) never shadow a global name by a local name
- state: 
   * used concrete names, initially the set of all global names
   * map from abstract names to concrete names
- when encountering a global id, just look up its name
- when encountering a local id, check whether it is already in the map
   * if yes, print its name
   * if no, assign it a name and add it
- when abstracting a name, check whether it is in the map
   * if yes, print its name, delete it
   * if no, we have a void abstraction, then choose an unused version of it

b) shadowing of unused global names by local names allowed
- first compute the used global names
- proceed as above, but start with the set of computed global names

In case of b, we do not need to maintain a set of global names after
scope checking.  We can store name suggestions locally with it each
abstract name as in Agda.  Note that there will be sharing, so it is
not more memory intensive.

-}

class Print a c where
  print :: a -> c

instance Print A.Declarations C.Declarations where
  print (A.Declarations adecls) = C.Declarations $ map print adecls

instance Print A.Declaration C.Declaration where
  print adecl =  
    case adecl of
      A.TypeSig n t -> C.TypeSig (A.suggestion n) $ print t
      A.Defn n mt e -> C.Defn (A.suggestion n) (fmap print mt) $ print e

instance Print A.Expr C.Expr where
  print e = evalState (printExpr e) $ nameSet $ A.globalCNames e 
--   print e = fst $ printExpr e $ nameSet $ A.globalCNames e 
     
printIdent :: A.Ident -> NameM C.Name
printIdent id = 
  case id of
    A.Var n -> askName n                   -- locals are potentially renamed
    _ -> return $ A.suggestion $ A.name id -- globals have unique concrete name

printExpr :: A.Expr -> NameM C.Expr
printExpr e = 
  case e of
    A.Ident a           -> C.Ident <$> printIdent a
    A.Typ               -> return $ C.Typ
    A.Pi Nothing  t1 t2 -> C.Fun <$> printExpr t1 <*> printExpr t2
    A.Pi (Just x) t1 t2 -> do
      t1 <- printExpr t1
      t2 <- printExpr t2
      x  <- bindName x
      return $ C.Pi x t1 t2
-- C.Pi <$> bindName x <*> printExpr t1 <*> printExpr t2
    A.Lam x mt e        -> do
      mt <- mapM printExpr mt
      e  <- printExpr e
      x  <- bindName x
      return $ C.Lam x mt e
-- C.Lam <$> bindName x <*> mapM printExpr mt <*> printExpr e
    A.App f e           -> C.Apps <$> printApp f [e]

printApp :: A.Expr -> [A.Expr] -> NameM [C.Expr]
printApp f es = 
  case f of
    A.App f e -> printApp f (e : es)
    -- put extra parentheses around lambda if it is in the head:
    A.Lam{}   -> printExpr f >>= \ f -> (C.Apps [f] :) <$> mapM printExpr es
    _         -> mapM printExpr (f : es)

data NameSet = NameSet 
  { usedNames :: Set C.Name
  , naming    :: Map A.UID C.Name
  }

-- | Create a name set from an initial set of used global names.
nameSet :: Set C.Name -> NameSet
nameSet ns = NameSet ns Map.empty

type NameM = State NameSet
-- type NameM a = NameSet -> (a, NameSet)

-- | Retrieve and delete name.
bindName :: A.Name -> NameM C.Name
bindName (A.Name x n) = do
  ns <- get
  let nam = naming ns
  case Map.lookup x nam of
    Just n' -> do
      put $ NameSet (Set.delete n' (usedNames ns)) (Map.delete x nam)
      return n'
    Nothing -> nextVariant n
 
-- | Retrieve or insert name.
askName :: A.Name -> NameM C.Name
askName n = do
  nam <- gets naming
--  maybe (internalError ["Scoping.askName: not in map", show n]) return $ 
  maybe (nextName n) return $ 
    Map.lookup (A.uid n) nam
 
nextName :: A.Name -> NameM C.Name
nextName (A.Name x n) = do
  n' <- nextVariant n
  modify $ \ (NameSet ns nam) -> NameSet (Set.insert n' ns) (Map.insert x n' nam)
  return n'

-- | Returns an unused variant of a concrete name.
nextVariant :: C.Name -> NameM C.Name
nextVariant n = do
  ns <- gets usedNames
  let loop i = do
      let n' = variant n i
      if Set.member n' ns then loop (i+1)
       else return n'
  loop 0 

variant :: C.Name -> Int -> C.Name
variant n i = 
  case i of
    0 -> n
    1 -> n ++ "'"
    2 -> n ++ "''"
    3 -> n ++ "'''"
    _ -> n ++ "'" ++ show i


 