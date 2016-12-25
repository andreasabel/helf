{-# LANGUAGE MultiParamTypeClasses, NamedFieldPuns, FlexibleInstances,
      FlexibleContexts, UndecidableInstances #-}

-- | Fresh name monad.
module Fresh where

import Control.Monad.State

import qualified Abstract as A
import ListEnv as Env

type Renaming = Env A.UID A.Name -- Map A.Name A.Name

class MonadFresh m where
  fresh    :: A.Name -> m A.Name   -- ^ return a fresh name variant
  renaming :: m Renaming

data FreshSt = FreshSt
  { nextId  :: A.UID
  , nameMap :: Renaming
  }

instance MonadState FreshSt m => MonadFresh m where

  fresh x = do
    FreshSt { nextId, nameMap } <- get
    let y = x { A.uid = nextId }
    put $ FreshSt { nextId = nextId - 1
                  , nameMap = Env.update nameMap (A.uid x) y
                  }
    return y

  renaming = gets nameMap
