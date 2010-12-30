{- | Handling of global definitions -}

{-# LANGUAGE UndecidableInstances #-}

module Signature where

import Control.Applicative
import Control.Bifunctor

import Control.Monad.Error
-- import Control.Monad.Reader
import Control.Monad.State

import Data.Array.MArray
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Abstract as A
-- import qualified Scoping -- for printing abstract names
import PrettyM
import Util

-- * Abstract signature 
--
-- A signature is a collection of signature entries.

-- | Signature entry.
data SigEntry val
  = SigCon { symbType :: val }
  | SigDef { symbType :: val
           , symbDef  :: val }

instance PrettyM m val => PrettyM m (SigEntry val) where
  prettyM it = 
    case it of
      SigCon t   -> prettyM t
      SigDef t v -> prettyM t <+> equals <+> prettyM v

class Signature val sig | sig -> val where
  -- sigEmpty   :: sig
  sigAdd     :: A.UID -> SigEntry val -> sig -> sig
  sigLookup  :: A.UID -> sig -> Maybe (SigEntry val)
  sigLookup' :: A.UID -> sig -> SigEntry val -- ^ throws internal error
  sigLookup' x = maybe err id . sigLookup x where
                   err = internalError ["key", show x, "unbound in signature"]

-- * Signature instances

type MapSig val = Map A.UID (SigEntry val)

instance Signature val (MapSig val) where
  -- sigEmpty  = Map.empty
  sigAdd    = Map.insert
  sigLookup = Map.lookup

instance Field (MapSig v) (MapSig v) where
  getF = id
  setF = const
  modF = id

-- type ArraySig val = DiffArray A.UID (SigEntry val) -- DiffArrays are slow

{-
newtype Signature val = Signature
  { signature :: Map A.UID (SigEntry val)
  }
emptySignature = Signature Map.empty
-}

{-
-- | Records can have signatures as substructures.
class HasSig sig a | a -> sig where
  getSig :: a -> sig
  mapSig :: (sig -> sig) -> a -> a
-}

{-
-- | This produces overlapping instances and it thus unusable.
instance (Signature val sig, HasSig sig a) => Signature val a where
  -- sigEmpty cannot be sensibly defined
  sigAdd x it = mapSig (sigAdd x it)
  sigLookup x = sigLookup x . getSig
-}

{-  This also leads to duplicate instances

-- I would like to extend signatures to records, but it only works for tuples,
-- because of limitations of the Haskell language.

instance Signature val sig => Signature val (sig,a) where
  sigAdd    x it = bimap (sigAdd x it) id
  sigLookup x    = sigLookup x . fst

instance Signature val sig => Signature val (a,sig) where
  sigAdd    x it = bimap id (sigAdd x it)
  sigLookup x    = sigLookup x . snd
-}

-- * Abstract signature monad
 
class (Applicative m) => MonadSig val m where
  addGlobal   :: A.Name -> SigEntry val -> m ()
  addCon      :: A.Name -> val -> m ()
  addCon n t   = addGlobal n (SigCon t)
  addDef      :: A.Name -> val -> val -> m ()
  addDef n t v = addGlobal n (SigDef t v)
  lookupName  :: A.Name -> m (SigEntry val)
  lookupCon   :: A.Name -> m val
  lookupCon n  = symbType <$> lookupName n
  lookupDef   :: A.Name -> m (val, val)
  lookupDef n  = pair symbType symbDef <$> lookupName n

-- * implementation of signature as mutable array

{- duplicate instance

instance ( Applicative m
         -- , PrettyM m val     -- for debugging
         , MArray arr (SigEntry val) m 
         , Field (arr A.UID (SigEntry val)) st
         , MonadState st m ) => MonadSig val m where

  addGlobal n it = --  traceM (((A.suggestion n ++ " : ") ++) <$> showM it) $
    modify $ modF $ \ arr -> writeArray arr (A.uid n) it
  
  lookupName n = flip readArray (A.uid n) . getF <$> get

-}

-- * implementation of signature as state monad

instance ( Applicative m
         -- , Scoping.Scope m
         -- , PrettyM m val     -- for debugging
         , Signature val sig
         -- , HasSig sig st
         , Field sig st
         , MonadState st m ) => MonadSig val m where

  addGlobal n it = --  traceM (((A.suggestion n ++ " : ") ++) <$> showM it) $
    modify $ modF $ sigAdd (A.uid n) it
  
  lookupName n = sigLookup' (A.uid n) . getF <$> get

{-
  lookupName n = do
    sig <- get
    case sigLookup n sig of
      Just it -> return it
      Nothing -> do 
        c <- Scoping.getName n
        fail $ "internal error: unbound global identifier, abstract = " ++ show n ++ " concrete = " ++ show c 
-}

{-
  addGlobal n it = modify $ Signature . Map.insert n it . signature
  
  lookupName n = do
    sig <- gets signature 
    case Map.lookup n sig of
      Just it -> return it
      Nothing -> do 
        c <- Scoping.getName n
        fail $ "unbound global identifier, abstract = " ++ show n ++ " concrete = " ++ show c 
-}