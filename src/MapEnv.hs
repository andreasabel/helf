-- | Environments implemented as tree maps.

module MapEnv where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Traversable as Trav

type Env = Map

-- | Query
lookup :: (Ord k) => k -> Env k v -> Maybe v
lookup = Map.lookup

lookupSafe :: (Ord k, Show k) => k -> Env k v -> v 
lookupSafe k = maybe (error $ "internal error: unbound key " ++ show k) id .
  MapEnv.lookup k

-- | Construction
empty :: Env k v
empty = Map.empty

singleton :: k -> v -> Env k v
singleton = Map.singleton

insert :: (Ord k) => k -> v -> Env k v -> Env k v
insert = Map.insert

update :: (Ord k) => Env k v -> k -> v -> Env k v
update rho x v = Map.insert x v rho

-- | Left-biased union
union :: (Ord k) => Env k v -> Env k v -> Env k v
union = Map.union

mapM :: (Monad m) => (v -> m w) -> Env k v -> m (Env k w)
mapM = Trav.mapM