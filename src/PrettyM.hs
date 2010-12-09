module PrettyM where

import Prelude hiding (sequence, mapM)

import Abstract
import Scoping
import qualified Util 

import Control.Applicative hiding (empty)
import Data.Traversable

import qualified Text.PrettyPrint as P


-- from Agda.TypeChecking.Pretty

type Doc = P.Doc

empty, comma, colon :: Monad m => m Doc

empty	   = return P.empty
comma	   = return P.comma
colon      = text ":"
pretty x   = return $ Util.pretty x
-- prettyA x  = P.prettyA x
text s	   = return $ P.text s
pwords s   = map return $ Util.pwords s
fwords s   = return $ Util.fwords s
sep ds	   = P.sep <$> sequence ds
fsep ds    = P.fsep <$> sequence ds
hsep ds    = P.hsep <$> sequence ds
vcat ds    = P.vcat <$> sequence ds
d1 $$ d2   = (P.$$) <$> d1 <*> d2
d1 <> d2   = (P.<>) <$> d1 <*> d2
d1 <+> d2  = (P.<+>) <$> d1 <*> d2
nest n d   = P.nest n <$> d
braces d   = P.braces <$> d
brackets d = P.brackets <$> d
parens d   = P.parens <$> d

prettyList ds = brackets $ fsep $ punctuate comma ds

punctuate _ [] = []
punctuate d ds = zipWith (<>) ds (replicate n d ++ [empty])
    where
	n = length ds - 1

failDoc :: Monad m => m Doc -> m a
failDoc d = fail . P.render =<< d


{-
-- monadic pretty printing 

class ToExpr a where
  toExpression :: a -> TypeCheck Expr 

instance ToExpr Expr where
  toExpression = return

instance ToExpr Val where
  toExpression = toExpr


class PrettyTCM a where
  prettyTCM :: a -> TypeCheck Doc 

instance PrettyTCM Expr where
  prettyTCM = pretty

instance PrettyTCM Val where
  prettyTCM v = pretty =<< toExpr v
-}
  