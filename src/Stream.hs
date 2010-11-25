{- | Monadic stream library -}
-- {-# LANGUAGE NoImplicitPrelude #-}

module Stream where

import Prelude hiding (map,foldr,(++))

import Control.Applicative
import Control.Monad

import qualified Data.List as List

newtype Stream m a = Stream { front :: Front m a } 
type    Front  m a = m (Maybe (a, Stream m a))

-- * Front construction

emptyF :: Monad m => Front m a
emptyF = return Nothing

consF :: Monad m => a -> Front m a -> Front m a
consF a tl = return $ Just (a, Stream tl)

consFM :: Monad m => m a -> Front m a -> Front m a
consFM hd tl = hd >>= (\ a -> consF a tl) 

-- * Stream construction

empty :: Monad m => Stream m a
empty = Stream $ return $ Nothing

cons :: Monad m => a -> Stream m a -> Stream m a
cons a tl = Stream $ return $ Just $ (a, tl)

-- * Stream destruction

{-
caseListF :: c -> (a -> b -> c) -> Maybe (a, b) -> c
caseListF nil cons = maybe nil (uncurry cons)
-}

caseStream :: Monad m => Stream m a -> m b -> (a -> Stream m a -> m b) -> m b
caseStream s nil cons = maybe nil (uncurry cons) =<< front s

foldr :: Monad m => (a -> m b -> m b) -> m b -> Stream m a -> m b
foldr cons nil = fold where
  fold s = caseStream s nil $ \ a -> cons a . fold 

foldl :: Monad m => (m b -> a -> m b) -> m b -> Stream m a -> m b
foldl f = fold where
  fold acc s = caseStream s acc $ fold . f acc 

-- * Instances of @foldr@

map :: Monad m => (a -> m b) -> Stream m a -> Stream m b
map f = Stream . foldr (\ a -> consFM (f a)) emptyF  

(++) :: Monad m => Stream m a -> Stream m a -> Stream m a
s ++ s' = Stream $ foldr consF (front s') s

filter :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
filter p = Stream . foldr cons emptyF where
  cons a fr = p a >>= \ b -> if b then consF a fr else fr 

-- * List conversion

fromList :: Monad m => [a] -> Stream m a
fromList = Stream . List.foldr consF emptyF

toList :: Monad m => Stream m a -> m [a]
toList = foldr (\ a m -> return . (a:) =<< m) (return [])

