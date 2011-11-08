{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Abstract where

import Prelude hiding (foldr)

import Data.Set (Set)
import qualified Data.Set as Set 

import Data.Foldable
import Data.Traversable

import qualified Concrete as C 

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

appView :: Expr -> (Expr, [Expr])
appView = loop [] where
  loop acc (App f e) = loop (e : acc) f
  loop acc f         = (f, acc)

-- * Queries

globalIds :: Expr -> Set Ident
globalIds = foldr (\ n ns -> if isGlobalIdent n then Set.insert n ns else ns)
                  Set.empty

globalCNames :: Expr -> Set C.Name
globalCNames = 
  foldr (\ n ns -> if isGlobalIdent n then Set.insert (suggestion $ name n) ns else ns)
        Set.empty
