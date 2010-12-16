{-# LANGUAGE UndecidableInstances #-}

module PrettyM where

import Prelude hiding (sequence, mapM, print)

import Control.Applicative hiding (empty)
import Control.Monad ((<=<))

import Data.Traversable

import qualified Text.PrettyPrint as P

import qualified Abstract as A
import qualified Concrete as C
import Scoping
import Value
import qualified Util 


-- from Agda.TypeChecking.Pretty

type Doc = P.Doc

empty, comma, colon, equals :: Monad m => m Doc

empty	   = return P.empty
comma	   = return P.comma
colon      = text ":"
equals     = text "="
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


{-
-- monadic pretty printing 

class ToExpr a where
  toExpression :: a -> TypeCheck Expr 

instance ToExpr Expr where
  toExpression = return

instance ToExpr Val where
  toExpression = toExpr
-}

class (Applicative m, Monad m) => PrettyM m a where
  prettyM :: a -> m Doc
  showM   :: a -> m String
  showM a = P.render <$> prettyM a

instance (Applicative m, Monad m) => PrettyM m C.Expr where
  prettyM c = return d where 
    d :: Doc
    d = Util.pretty c

instance (Applicative m, Monad m) => PrettyM m C.Declaration where
  prettyM = pretty

instance (Applicative m, Monad m) => PrettyM m A.Expr where
  prettyM a = prettyM c where
    c :: C.Expr
    c = print a

instance (Applicative m, Monad m) => PrettyM m A.Declaration where
  prettyM a = prettyM c where
    c :: C.Declaration
    c = print a

{-
instance MonadEval val env m => PrettyM m val where
  prettyM = prettyM <=< reify 
-}
  