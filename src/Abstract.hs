{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module Abstract where

import Prelude hiding (foldr)

import Control.Monad.Reader

import Data.Set (Set)
import qualified Data.Set as Set 

-- import Data.Map (Map)
-- import qualified Data.Map as Map 
import qualified Data.IntMap as M 

import Data.Foldable
import Data.Traversable

import qualified Concrete as C 

import Util

-- * Abstract names and identifiers

type UID = Int

-- | Names in the abstract syntax are unique.
data Name = Name 
  { uid        :: UID     -- ^ positive numbers come from user, 
                          --   negative from system (e.g. quoting)
  , suggestion :: C.Name  -- ^ name suggestion for printing
  } 

instance Eq Name where
  x == y = uid x == uid y

instance Ord Name where
  compare x y = compare (uid x) (uid y)

instance Show Name where
  show x = suggestion x ++ ":" ++ show (uid x)

-- | Local and global names.
data Ident 
  = Var { name :: Name }          -- ^ locally bound identifier
  | Con { name :: Name }          -- ^ declared constant
  | Def { name :: Name }          -- ^ defined identifier
  deriving (Eq,Ord)

instance Show Ident where
  show (Var n) = "Var " ++ show n
  show (Con n) = "Con " ++ show n
  show (Def n) = "Def " ++ show n

isGlobalIdent :: Ident -> Bool
isGlobalIdent (Var{}) = False
isGlobalIdent _       = True

-- * Generating local names

systemGeneratedName :: Name
systemGeneratedName = Name { uid = -1,  suggestion = C.noName }

noName = systemGeneratedName

type SysNameCounter = Int

initSysNameCounter :: SysNameCounter
initSysNameCounter = -1

-- | Get the next local name, as variant of an existing name.
nextSysName :: SysNameCounter -> Name -> (Name, SysNameCounter)
nextSysName i n = (n { uid = i }, i - 1)

-- | Get the next local name with no concrete representation.
nextSysName' :: SysNameCounter -> (Name, SysNameCounter)
nextSysName' i = nextSysName i noName

-- * Abstract syntax for declarations and expressions

newtype Declarations = Declarations { declarations :: [Declaration] }

data Declaration
  = TypeSig Name Type            -- ^ @c : A.@
  | Defn Name (Maybe Type) Expr  -- ^ @d : A = e.@ or @d = e.@
    deriving (Show)

type Type = Expr
type Expr = Expression Ident

type TypeExpr id = Expression id
data Expression id
  = Ident id                                        -- ^ @x@ or @c@ or @d@
  | Typ                                             -- ^ @type@
  | Pi   (Maybe Name) (TypeExpr id) (TypeExpr id)   -- ^ @A -> B@ or @{x:A} B@
  | Lam  Name (Maybe (TypeExpr id)) (Expression id) -- ^ @[x:A] E@ or @[x]E@
  | App  (Expression id) (Expression id)            -- ^ @E1 E2@ 
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

-- * Spine view

instance IsApp Expr where
  isApp (App f e) = Just (f, e)
  isApp _         = Nothing

{- code generalized and moved to Util
appView :: Expr -> (Expr, [Expr])
appView = loop [] where
  loop acc (App f e) = loop (e : acc) f
  loop acc f         = (f, acc)
-}

-- * alpha equality

newtype Alpha a = Alpha a
  deriving (Functor)

instance Show a => Show (Alpha a) where
  show (Alpha a) = show a

instance Ord (Alpha Expr) where
  compare (Alpha e) (Alpha e') = aCompare e e'

instance Ord (Alpha a) => Eq (Alpha a) where
  a == a' = compare a a' == EQ
  -- (Alpha e) == (Alpha e') = aeq e e' Map.empty

type IMap = M.IntMap UID -- map directed from left to right
type Cmp = Reader IMap

class OrdAlpha a where
  aCompare :: a -> a -> Ordering
  aCompare e1 e2 = runReader (acmp e1 e2) M.empty

--   acmp :: MonadReader IMap m => a -> a -> m Ordering
  acmp :: a -> a -> Cmp Ordering
  acmp e1 e2 = return $ aCompare e1 e2

instance OrdAlpha Name where
  acmp (Name x _) (Name y _) 
    | x == y = return EQ
    | otherwise = do
        m <- ask
        case M.lookup x m of
          Nothing -> return $ compare x y
          Just y' -> return $ compare y' y

-- | Just look at UID
instance OrdAlpha Ident where
  acmp x y = acmp (name x) (name y)

instance OrdAlpha Expr where
  acmp e e' = case (e, e') of

    (Ident x, Ident x') -> acmp x x'
    (Ident _, _) -> return LT
    (_, Ident _) -> return GT

    (App f e, App f' e') -> lexM [ acmp f f', acmp e e' ]
    (App _ _, _) -> return LT
    (_, App _ _) -> return GT

    (Pi Nothing a b, Pi Nothing a' b') -> acmp (a,b) (a',b')
    (Pi Nothing _ _, _) -> return LT
    (_, Pi Nothing _ _) -> return GT

    (Pi (Just x) a b, Pi (Just x') a' b') -> lexM 
      [ acmp a a'
      , local (M.insert (uid x) (uid x')) $ acmp b b' ]
    (Pi (Just _) _ _, _) -> return LT
    (_, Pi (Just _) _ _) -> return GT

    (Lam x a e, Lam x' a' e') -> lexM 
      [ acmp a a'
      , local (M.insert (uid x) (uid x')) $ acmp e e' ]
    (Lam _ _ _, _) -> return LT
    (_, Lam _ _ _) -> return GT

    (Typ, Typ)   -> return EQ
{-
    (Typ, _)     -> return LT
    (_, Typ)     -> return GT
-}

-- | Lexicographic comparison
instance (OrdAlpha a, OrdAlpha b) => OrdAlpha (a,b) where
  acmp (a1,b1) (a2,b2) = lexM [acmp a1 a2, acmp b1 b2]

-- | Use only for lists of equal length!
instance (OrdAlpha a) => OrdAlpha [a] where
  acmp as bs = lexM $ zipWith acmp as bs

instance (OrdAlpha a) => OrdAlpha (Maybe a) where
  acmp Nothing Nothing   = return EQ
  acmp Nothing (Just _)  = return LT
  acmp (Just _) Nothing  = return GT
  acmp (Just a) (Just b) = acmp a b

-- | Lazy lexicographic combination..
lexM :: Monad m => [m Ordering] -> m Ordering
lexM []     = return EQ
lexM (m:ms) = do
  o <- m
  case o of
     LT -> return LT
     GT -> return GT
     EQ -> lexM ms

-- * Queries

globalIds :: Expr -> Set Ident
globalIds = foldr (\ n ns -> if isGlobalIdent n then Set.insert n ns else ns)
                  Set.empty

globalCNames :: Expr -> Set C.Name
globalCNames = 
  foldr (\ n ns -> if isGlobalIdent n then Set.insert (suggestion $ name n) ns else ns)
        Set.empty
