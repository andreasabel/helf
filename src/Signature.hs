{- | Handling of global definitions -}

{-# LANGUAGE UndecidableInstances #-}

module Signature where

import Control.Applicative
import Control.Monad.Error
-- import Control.Monad.Reader
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Abstract as A
import qualified Scoping -- for printing abstract names
import Util

-- * abstract signature specification

data SigEntry val
  = SigCon { symbType :: val }
  | SigDef { symbType :: val
           , symbDef  :: val }

class (Applicative m) => MonadSig val m | val -> m where
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

-- * implementation of signature as state/error monad

newtype Signature val = Signature
  { signature :: Map A.Name (SigEntry val)
  }

instance (Monad m, MonadState (Signature val) m, Scoping.Scope m) => MonadSig val m where

  addGlobal n it = modify $ Signature . Map.insert n it . signature
  
  lookupName n = do
    sig <- gets signature 
    case Map.lookup n sig of
      Just it -> return it
      Nothing -> do 
        c <- Scoping.getName n
        fail $ "unbound global identifier, abstract = " ++ show n ++ " concrete = " ++ show c 