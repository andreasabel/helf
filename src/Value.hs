module Value where

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

{-
data TyView val
  = VPi val val
  | VSort Sort
  | VBase 

data TmView head val
  = VNe  head val [val]  -- x^A vs, c^A vs
  | VDef head val [val]  -- d^A vs
  | VVal                 -- Abs, Pi, Type, ...
-}

class (Eq head, Ord head) => Value head val | val -> head where
  typ     :: val
  kind    :: val
  freeVar :: head -> val -> val      -- typed free variable
  valView :: val -> ValView head val
{-
  tyView  :: val -> TyView val
  tmView  :: val -> TmView head val
-}

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


  