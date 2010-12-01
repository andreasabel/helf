module Common where

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
