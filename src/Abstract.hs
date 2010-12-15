module Abstract where

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
  deriving (Eq,Ord,Show)

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

type Type = Expr
data Expr
  = Ident Ident                   -- ^ @x@ or @c@ or @d@
  | Typ                           -- ^ @type@
  | Pi    (Maybe Name) Type Type  -- ^ @A -> B@ or @{x:A} B@
  | Lam   Name (Maybe Type) Expr  -- ^ @[x:A] E@ or @[x]E@
  | App   Expr Expr               -- ^ @E1 E2@ 
  deriving (Show)
