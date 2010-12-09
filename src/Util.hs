module Util where

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