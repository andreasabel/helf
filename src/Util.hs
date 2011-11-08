module Util where

import Control.Applicative
import Control.Monad.Error

import Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace

-- * pretty printing

import Text.PrettyPrint

class Pretty a where
  pretty        :: a -> Doc
  prettyPrec	:: Int -> a -> Doc

  pretty	= prettyPrec 0
  prettyPrec	= const pretty

dot :: Doc
dot = text "."

parensIf :: Bool -> Doc -> Doc
parensIf b = if b then parens else id

pwords :: String -> [Doc]
pwords = map text . words

fwords :: String -> Doc
fwords = fsep . pwords

-- * categories

pair :: (c -> a) -> (c -> b) -> c -> (a,b)
pair f g x = (f x, g x)

-- * monads

-- | Binary version of @=<<@.
appM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
appM2 f ma mb = ma >>= \ a -> mb >>= f a
{-# INLINE appM2 #-}

-- | Ternary version of @=<<@.
appM3 :: Monad m => (a -> b -> c -> m d) -> m a -> m b -> m c -> m d
appM3 f ma mb mc = ma >>= \ a -> mb >>= \ b -> mc >>= f a b
{-# INLINE appM3 #-}

whenMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenMaybe m cont = maybe (return ()) cont m

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM c t e = do
  b <- c
  if b then t else e

whenM :: Monad m => m Bool -> m () -> m ()
whenM cond action = ifM cond action $ return ()

unlessM :: (Applicative m, Monad m) => m Bool -> m () -> m ()
unlessM cond action = ifM (not <$> cond) action $ return ()

-- * records

-- | @Field@ is a specification of a lens (see Data.Record.Labels).
class Field a r | r -> a where
  getF :: r -> a
  setF :: a -> r -> r
  modF :: (a -> a) -> r -> r
  modF f r = setF (f $ getF r) r

{- This does not work well with the functional dependency
instance Field r r where
  getF = id
  setF = const
  modF f = f
-}
-- * error

-- | Expects a non-empty list of words which are concatenated to compose the
--   error message.
internalError :: [String] -> a
internalError = error . unwords . ("internal error:" :)
  -- where unwords =  foldr1 (\ h t -> h ++ ' ' : t) -- defined in Prelude

fails :: Monad m => [String] -> m a
fails = fail . unwords

failDoc :: MonadError String m => m Doc -> m a
failDoc d = throwError . render =<< d

enter :: MonadError String m => String -> m a -> m a
enter s' cont = cont `catchError` \ s -> throwError (s ++ "\n  " ++ s)


enterDoc :: MonadError String m => m Doc -> m a -> m a
enterDoc md cont = cont `catchError` \ s -> do
   d <- md
   throwError (render d ++ "\n  " ++ s)

-- * debugging

traceM :: Monad m => m String -> m ()
traceM mmsg = do
  msg <- mmsg
  trace msg $ return ()

traceDoc :: (Functor m, Monad m) => m Doc -> m ()
traceDoc mdoc = traceM (render <$> mdoc)

{-
traceM :: Monad m => m String -> m b -> m b
traceM mmsg cont = do
  msg <- mmsg
  trace msg cont

doTrace :: Monad m => String -> m ()
doTrace msg = trace msg $ return ()
-}

-- * maps

lookupSafe :: (Ord k, Show k) => k -> Map k v -> v
lookupSafe k = maybe (error $ "internal error: unbound key " ++ show k) id .
  Map.lookup k