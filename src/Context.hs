{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}

module Context where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
-- import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Abstract as A

-- * abstract context monad

-- class MonadCxt val env m | val -> m, val -> env where
class MonadCxt val env m | m -> val, m -> env where
  addLocal    :: A.Name -> val -> (val -> m a) -> m a
  lookupLocal :: A.Name -> m val
  getEnv      :: m env

-- * implementation as reader monad
