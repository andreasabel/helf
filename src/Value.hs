module Value where

import Abstract

data Sort 
  = Type
  | Kind -- only internally
    deriving (Eq, Ord, Show)

data TyView val
  = VPi val val
  | VSort Sort
  | VBase 

data TmView fvar val
  = VNe  fvar val [val]  -- x^A vs, c^A vs
  | VVal                 -- Abs, Pi, Type, ...

class Eq fvar => Value fvar val | val -> fvar where
  typ     :: val
  kind    :: val
  freeVar :: fvar -> val -> val      -- typed free variable
  tyView  :: val -> TyView val
  tmView  :: val -> TmView fvar val

-- | The purpose of @MonadEval@ is read-only access to a global signature
--   to get the definition of symbols, and potentially IO to have references
--   to implement call-by-need.
class MonadEval val env m | m -> val, m -> env where
  apply     :: val  -> val -> m val
  evaluate  :: Expr -> env -> m val  
  evaluate' :: Expr -> m val        -- ^ evaluate a closed expression
  abstractPi:: val -> val -> val -> m val -- ^ abstractPi a x b = pi x:a.b
--  reify   :: val -> m Expr   -- this will need the local context


  