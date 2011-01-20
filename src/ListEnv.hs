{-# LANGUAGE TupleSections #-}

-- | Environments implemented as association lists.
module ListEnv where

type Env k v = [(k,v)]

-- | Query
lookup :: (Eq k) => k -> Env k v -> Maybe v
lookup = Prelude.lookup

lookupSafe :: (Eq k, Show k) => k -> Env k v -> v 
lookupSafe k = maybe (error $ "internal error: unbound key " ++ show k) id .
  ListEnv.lookup k

-- | Construction
empty :: Env k v
empty = []

singleton :: k -> v -> Env k v
singleton k v = [(k,v)]

insert :: k -> v -> Env k v -> Env k v
insert x v rho = (x,v) : rho

update :: Env k v -> k -> v -> Env k v
update rho x v = (x,v) : rho

-- | Left-biased union
union :: (Ord k) => Env k v -> Env k v -> Env k v
union = (++)

mapM :: (Monad m) => (v -> m w) -> Env k v -> m (Env k w)
mapM f = Prelude.mapM (\ (k,v) -> f v >>= return . (k,))

