module Value where

import Control.Applicative
import Control.Monad

import Abstract

data Sort 
  = Type
  | Kind -- only internally
    deriving (Eq, Ord, Show)

data ValView head val
  = VPi val val
  | VSort Sort
  | VNe  head val [val]  -- x^A vs, c^A vs
  | VDef head val [val]  -- d^A vs
  | VAbs                 -- Abs

-- | The purpose of @MonadEval@ is read-only access to a global signature
--   to get the definition of symbols, and potentially IO to have references
--   to implement call-by-need.
class (Eq head, Ord head, Functor m, Applicative m, Monad m) => 
    MonadEval head val env m | val -> head, m -> val, m -> env where
  typ     :: m val
  kind    :: m val
  freeVar :: head -> val -> m val      -- typed free variable
  valView :: val -> m (ValView head val)

  apply     :: val  -> val -> m val
  evaluate  :: Expr -> env -> m val  
  evaluate' :: Expr -> m val        -- ^ evaluate a closed expression
  unfold    :: val -> m val         -- ^ unfold head definition 
  unfolds   :: val -> m val         -- ^ unfold definition until constructor
  abstractPi:: val -> (Name, val) -> val -> m val -- ^ abstractPi a x b = pi x:a.b
  reify     :: val -> m Expr        -- ^ quote value as expression

{-
class (Eq head, Ord head) => Value head val | val -> head where
  typ     :: val
  kind    :: val
  freeVar :: head -> val -> val      -- typed free variable
  valView :: val -> ValView head val

-- | The purpose of @MonadEval@ is read-only access to a global signature
--   to get the definition of symbols, and potentially IO to have references
--   to implement call-by-need.
class MonadEval val env m | m -> val, m -> env where
  apply     :: val  -> val -> m val
  evaluate  :: Expr -> env -> m val  
  evaluate' :: Expr -> m val        -- ^ evaluate a closed expression
  unfold    :: val -> m val         -- ^ unfold head definition 
  unfolds   :: val -> m val         -- ^ unfold definition until constructor
  abstractPi:: val -> (Name, val) -> val -> m val -- ^ abstractPi a x b = pi x:a.b
  reify     :: val -> m Expr        -- ^ quote value as expression
-}

-- | Apply a function to a reversed list of arguments.
appsR :: (MonadEval head val env m) => val -> [val] -> m val 
appsR f vs = foldr (\ v mf -> mf >>= \ f -> apply f v) (return f) vs
  