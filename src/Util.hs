module Util where

import Data.Map (Map)
import qualified Data.Map as Map

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

-- | Binary version of @>>=@.
appM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
appM2 f ma mb = ma >>= \ a -> mb >>= f a

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

-- * maps

lookupSafe :: (Ord k, Show k) => k -> Map k v -> v
lookupSafe k = maybe (error $ "internal error: unbound key " ++ show k) id .
  Map.lookup k