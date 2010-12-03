-- | Frank Pfennings operator precedence parser, translated from SML to Haskell.
module OperatorPrecedenceParser where

import Control.Applicative
import Control.Monad.Error

{- Adaption of Frank Pfenning's operator precedence parser for Twelf.
   Original code in SML. -}

-- | Associativity of infix operators.
data Associativity 
  = AssocLeft
  | AssocRight
  | AssocNone
  deriving (Eq)

-- | Types and binding strength of operators.
data Fixity prec
  = Infix   { precedence :: prec , associativity :: Associativity }
  | Prefix  { precedence :: prec }
  | Postfix { precedence :: prec }
  | Nofix

-- | Items of the parse stack are either atoms or operators.
data Item prec a
  = Atom a
  | Op   { fixity :: Fixity prec , constructor :: [a] -> a }

-- | Expression language @a@ is assumed to have a juxtaposition operator.
class Juxtaposition a where
  juxtaposition :: a -> a -> a  -- ^ application operation.

-- | Juxtaposition.
juxOp :: (Bounded prec, Juxtaposition a) => Item prec a
juxOp = Op (Infix maxBound AssocLeft) (\ [f,x] -> juxtaposition f x)

{- | Stack invariants, refinements of operator list.
@
 <p>       ::= <pStable> | <pRed>
 <pStable> ::= <pAtom> | <pOp?>
 <pAtom>   ::= Atom : <pOp?>
 <pOp?>    ::= [] | <pOp>
 <pOp>     ::= Infix : <pAtom> : <pOp?>
	     | Prefix : <pOp?>
 <pRed>    ::= Postfix : Atom : <pOp?>
	     | Atom : <pOp>
@
-}
type Stack prec a = [Item prec a]

-- | Parse monad.
type Result = Either ParseError

-- | Possible errors of operator precedence parsing.
data ParseError 
  = IncompleteInfix
  | IncompletePrefix
  | EmptyExpression
  | InfixInfix
  | PrefixInfix
  | LeadingInfix
  | InfixPostfix
  | PrefixPostfix
  | LeadingPostfix
  | InfixInfixSamePrec
  | PrefixInfixSamePrec
  | PrefixPostfixSamePrec
  | InfixPostfixSamePrec
  | GenericError String
  deriving (Eq)
  
instance Show ParseError where
   show IncompleteInfix  = "Incomplete infix expression"
   show IncompletePrefix = "Incomplete prefix expression"
   show EmptyExpression = "empty expression"
   show InfixInfix  = "Consecutive infix operators"
   show PrefixInfix = "Infix operator following prefix operator"
   show LeadingInfix = "Leading infix operator"
   show InfixPostfix = "Postfix operators following infix operator"
   show PrefixPostfix = "Postfix operator following prefix operator"
   show LeadingPostfix = "Leading postfix operator"
   show InfixInfixSamePrec = "Ambiguous: infix following infix of identical precedence"
   show PrefixInfixSamePrec = "Ambiguous: infix following prefix of identical precedence"
   show PrefixPostfixSamePrec = "Ambiguous: postfix following prefix of identical precedence"
   show InfixPostfixSamePrec = "Ambiguous: postfix following infix of identical precedence"

instance Error ParseError where
  strMsg = GenericError

-- | @reduce :: <pRed> -> <p>@.  Perform top reduction on stack.
reduce :: Stack prec a -> Stack prec a
reduce (Atom b : Op Infix{} f : Atom a : s) = Atom (f[a,b]) : s
reduce (Atom a : Op Prefix{} f         : s) = Atom (f[a])   : s
reduce (Op Postfix{} f  : Atom a       : s) = Atom (f[a])   : s
-- no other cases should be possible by stack invariant 

-- | @reduceRec : <pStable> -> a@.  Performs all remaining reductions on safe stack.
reduceRec :: Stack prec a -> a
reduceRec [Atom a] = a
reduceRec s = reduceRec (reduce s)

-- | @reduceAll : <p> -> ExtSyn.term@.  
--   Performs all reductions, top of stack might be unsound.
reduceAll :: Stack prec a -> Result a
reduceAll (Op Infix{} _  : s) = throwError IncompleteInfix
reduceAll (Op Prefix{} _ : s) = throwError IncompletePrefix
reduceAll [] = throwError EmptyExpression
reduceAll s  = return $ reduceRec s

-- | @shiftAtom : term * <pStable> -> <p>@. Safe shift.
shiftAtom :: (Bounded prec, Juxtaposition a) => a -> Stack prec a -> Stack prec a
shiftAtom a s@(Atom{} : _) = reduce (Atom a : juxOp : s)
  -- to avoid consecutive atoms, insert juxOp operator and reduce
  -- juxtaposition binds most strongly
shiftAtom a s = Atom a : s


-- | @shift : Item -> <pStable> -> <p>@.  
--   Fails on consecutive operators that cannot be reconciled.
shift :: (Bounded prec, Juxtaposition a) => 
         Item prec a -> Stack prec a -> Result (Stack prec a)

-- ill-formed sequences:
shift (Op Infix{} _)     s@(Op Infix{} _ : _)  = throwError InfixInfix 
shift (Op Infix{} _)     s@(Op Prefix{} _ : _) = throwError PrefixInfix
shift (Op Infix{} _)     []                    = throwError LeadingInfix
shift (Op Postfix{} _)   s@(Op Infix{} _ : _)  = throwError InfixPostfix
shift (Op Postfix{} _)   s@(Op Prefix{} _ : _) = throwError PrefixPostfix
shift (Op Postfix{} _)   []                    = throwError LeadingPostfix

-- juxtaposition insertion:
shift op@(Atom{})        s@(Atom{} : _) = return $ reduce (op : juxOp : s)
  -- juxtaposition binds most strongly!
shift op@(Op Prefix{} _) s@(Atom{} : _) = return $ op : juxOp : s
  -- cannot reduce now, prefix operator waits for its argument

-- remaining cases:
-- * Atom/Infix: shift 
-- * Atom/Prefix: shift 
-- * Atom/Postfix cannot arise 
-- * Atom/Empty: shift 
-- * Infix/Atom: shift 
-- * Infix/Postfix cannot arise 
-- * Prefix/{Infix,Prefix,Empty}: shift 
-- * Prefix/Postfix cannot arise 
-- * Postfix/Atom: shift, reduced immediately 
-- * Postfix/Postfix cannot arise 
shift op s = return $ op : s

-- | Decides, based on precedence of opr compared to the top of the
--   stack whether to shift the new operator or reduce the stack.
resolve :: (Ord prec, Bounded prec, Juxtaposition a) => 
           Item prec a -> Stack prec a -> Result (Stack prec a)

resolve op@(Op (Infix prec assoc) _) s@(Atom{} : Op (Infix prec' assoc') _ : _) =
  case (compare prec prec', assoc, assoc') of
    (GT, _, _)                   -> shift op s
    (LT, _, _)                   -> resolve op (reduce s)
    (EQ, AssocLeft , AssocLeft ) -> resolve op (reduce s)
    (EQ, AssocRight, AssocRight) -> shift op s
    _                            -> throwError InfixInfixSamePrec

resolve op@(Op (Infix prec _) _) s@(Atom{} : Op (Prefix prec') _ : _) =
  case compare prec prec' of
    GT -> shift op s
    LT -> resolve op (reduce s)
    _  -> throwError PrefixInfixSamePrec

-- infix/atom/atom cannot arise 
-- infix/atom/postfix cannot arise 
-- infix/atom/<empty>: shift 

-- always reduce postfix, possibly after prior reduction
resolve op@(Op (Postfix prec) _) s@(Atom{} : Op (Prefix prec') _ : _) =
  case compare prec prec' of
    GT -> reduce <$> shift op s
    LT -> resolve op (reduce s)
    _  -> throwError PrefixPostfixSamePrec

-- always reduce postfix
resolve op@(Op (Postfix prec) _) s@(Atom{} : Op (Infix prec' _) _ : _) =
  case compare prec prec' of
    GT -> reduce <$> shift op s
    LT -> resolve op (reduce s)
    _  -> throwError InfixPostfixSamePrec

resolve op@(Op (Postfix prec) _) s@[Atom{}] = reduce <$> shift op s

-- always shift prefix
-- default is shift 
resolve op s = shift op s

-- | Take a non-empty list $E1 E2 ... En$ of items and parse it.
parseApplication :: (Bounded prec, Ord prec, Juxtaposition a) => [Item prec a] -> Result a
parseApplication l = reduceAll =<< foldl (\ ms i -> resolve i =<< ms) (return []) l
