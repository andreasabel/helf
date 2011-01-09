-- a simplified version of DynArray. get, insert, split, join are all O(log n).

module DatastrucImpl.SimpleDynArray where

import Prelude hiding (repeat)
import qualified Data.List as List

import Text.PrettyPrint (Doc,nest,vcat,text,render)
import qualified Text.PrettyPrint as PP

import Debug.Trace
import Test.QuickCheck

{-
import DataStructure

instance DataStruc DynArray a where
  -- show = render . pretty
  empty             = DatastrucImpl.SimpleDynArray.empty
  insert            = DatastrucImpl.SimpleDynArray.insert
  multiinsert       = DatastrucImpl.SimpleDynArray.multiinsert
  get               = DatastrucImpl.SimpleDynArray.get
  split i datastruc = DatastrucImpl.SimpleDynArray.split datastruc i
  join              = DatastrucImpl.SimpleDynArray.join
  size              = len
-}

data DynArr a 
  = Leaf { entry   :: a }      -- ^ an array filled with element @entry@ 
  | Node { leftLen :: Int      -- ^ length of left part
         , left    :: DynArr a -- ^ left part
         , right   :: DynArr a -- ^ right part
         }
 -- | Empty                     -- ^ not allowed as part of a nonempty tree !
    deriving (Eq,Ord,Show)

data DynArray a = DynArray { len :: Int, array :: DynArr a }
    deriving (Eq,Ord)
  
pretty :: Show a => DynArray a -> Doc
pretty (DynArray 0 _) = text "empty"
pretty (DynArray 1 (Leaf a)) = text $ Prelude.show a
pretty (DynArray n (Leaf a)) = text $ Prelude.show n ++ " * " ++ Prelude.show a
pretty (DynArray n (Node k l r)) = vcat 
  [ nest 2 $ pretty $ DynArray k l
  , text (Prelude.show k)
  , nest 2 $ pretty $ DynArray (n-k) r
  ]

instance Show a => Show (DynArray a) where
  show = render . pretty

leaf :: a -> DynArray a
leaf = DynArray 1 . Leaf

empty :: DynArray a 
empty = DynArray 0 $ error "empty DynArray"

-- | Array lookup with bounds checking. O(log n).
get :: DynArray a -> Int -> a
get arr i | 0 <= i && i < len arr = get' (array arr) i 
        | otherwise = error $ "index " ++ Prelude.show i ++ " out of range"

-- | Lookup without bounds checking. O(log n).
get' :: DynArr a -> Int -> a
get' (Leaf a) 0 = a
get' (Node llen left right) i 
  | i < llen  = get' left i
  | otherwise = get' right (i - llen)
-- failure: 
get' (Leaf a) _ = error "Leaf cannot contain more than one element"
-- get' Empty _ = error "tried to search in an empty array"

-- | Splitting the array. O(log n).
split :: DynArray a -> Int -> (DynArray a, DynArray a)
split arr i 
  | i <= 0 = (DatastrucImpl.SimpleDynArray.empty, arr)
  | i >= len arr = (arr, DatastrucImpl.SimpleDynArray.empty)
  | otherwise = split' arr i

-- Note that with Leaves holding no more than one information, we never have to split a Leaf. We would never try to split empty Arrays as well.
split' :: DynArray a -> Int -> (DynArray a, DynArray a)
split' (DynArray len (Node llen left right)) i =
  if i == llen
    then (DynArray llen left, DynArray (len-llen) right)
    else if i < llen
      then
        let (left', right') = split' (DynArray llen left) i
        in (left', right' `DatastrucImpl.SimpleDynArray.join` (DynArray (len-llen) right))
      else
        let (left', right') = split' (DynArray (len-llen) right) (i-llen)
        in ((DynArray llen left) `DatastrucImpl.SimpleDynArray.join` left', right')




-- | Balancing join. weight = 3 recommended. O(log (n1/n2)), if n1 > n2. This can be written as O(log(n1/n2 + n2/n1)).
-- Reaching certain points in the following function assures that certain inequalities (which we want to note down) are valid. For better readability we identify a tree's name with it's size and use the shortcut "a1 ~ a2" for "a1 <= k*a2 && k*a1 >= a2" as well as "a1 >> a2" for "a1 > k*a2" (where k is the maximal unbalance factor, i.e. the weight).
join :: DynArray a -> DynArray a -> DynArray a
join     (DynArray 0  _) a_2@(DynArray n2 _) = a_2
join a_1@(DynArray n1 _)     (DynArray 0  _) = a_1
join a_1@(DynArray n1 a1) a_2@(DynArray n2 a2) = -- trace ("join " ++ Prelude.show n1 ++ " " ++ Prelude.show n2) $
  let n = n1 + n2 
      join4 n11 a11 n12 a12 n21 a21 a22 = DynArray n $ Node (n11 + n12) 
        (Node n11 a11 a12) (Node n21 a21 a22) in 
  case joinable a_1 a_2 of
    Ok -> DynArray n (Node n1 a1 a2)
    LeftTooBig n11 a11 a12 ->
      -- a_1 >> a_2 , a11+a12 = a_1 , a11 ~ a12
      -- recursively join middle and right part
      let DynArray _ a2'@(Node n21' a21' a22') = DynArray (n1 - n11) a12 `DatastrucImpl.SimpleDynArray.join` a_2
      -- a21'+a22' = a12+a_2 , a21' ~ a22'
      in  if balanced n11 (n - n11) 
          then DynArray n (Node n11 a11 a2')
           -- the new middle node could be too big
          else 
            -- a11 !~ a21'+a22'
            if balanced n11 n21' && balanced (n11+n21') (n-n11-n21')  
            -- note that this is only possible if a2' =/= Node (n1-n11) a12 a2, which is the case if a2' was build 'trivially'
              then DynArray n (Node (n11+n21') (Node n11 a11 a21') a22') 
              else case a21' of -- split middle node
              -- a11 << a21' || a11+a21' >> a22' , a21' = a211'+a212' , a211' ~ a212'
                    -- check that none of a11 !~ a211', a212' !~ a22' , a212' !~ a22' , a1+a211' ~ a212'+a22' is satisfied
                    Node n211' a211' a212' -> join4 n11 a11 n211' a211' (n21' - n211') a212' a22'
                    -- a21' cannot be a Leaf
    RightTooBig n21 a21 a22 ->
      let DynArray _ a1'@(Node n11' a11' a12') = a_1 `DatastrucImpl.SimpleDynArray.join` DynArray n21 a21
      in  if balanced (n1 + n21) (n2 - n21) 
           then DynArray n (Node (n1 + n21) a1' a22)
           else
             if balanced n11' (n-n11') && balanced (n1+n21-n11') (n2-n21)
                then DynArray n (Node n11' a11' (Node (n1+n21-n11') a12' a22))
                else case a12' of -- split middle node
                  Node n121' a121' a122' -> join4 n11' a11' n121' a121' (n1 + n21 - n11' - n121') a122' a22
                  -- a12' cannot be a Leaf




data Joinable a 
  = Ok
  | LeftTooBig Int (DynArr a) (DynArr a)
  | RightTooBig Int (DynArr a) (DynArr a)


weight = 3  -- a tree is balanced iff (the ratio between its left and right subtree does not exceed the weight and they are both balanced)

balanced :: Int -> Int -> Bool
balanced n1 n2 = n1*weight >= n2 && n2*weight >= n1
-- could be written as (n1*n1+n2*n2)*weight =< n1*n2*(weight*weight+1)

joinable :: DynArray a -> DynArray a -> Joinable a
joinable (DynArray n1 arr1) (DynArray n2 arr2) = 
  if balanced n1 n2 then Ok
  else case (arr1, arr2) of
         (Node llen left right, _) | n1 > n2 -> LeftTooBig llen left right 
         (_, Node llen left right) | n2 > n1 -> RightTooBig llen left right 


-- first implementation of insertion. O(log n). However, rotation might be better.
insert :: a -> Int -> DynArray a -> DynArray a
insert x i a@(DynArray k arr) | i <= 0 = insert' x 0 a
insert x i a@(DynArray k arr) | i >= k = insert' x k a
insert x i a                           = insert' x i a

insert' :: a -> Int -> DynArray a -> DynArray a
insert' x i a@(DynArray k arr) =
  let
  (left, right) = DatastrucImpl.SimpleDynArray.split a i
  left' = left `DatastrucImpl.SimpleDynArray.join` (DynArray 1 (Leaf x))
  in
  left' `DatastrucImpl.SimpleDynArray.join` right


-- injects a given value at the given places
-- O(k*log n), n = size of the DynArray, k = number of the position where a value has to be injected
multiinsert:: a -> [Int] -> DynArray a -> DynArray a
multiinsert _ [] dyn = dyn
multiinsert x (k:klist) dyn = 
  let
  (left, right) = DatastrucImpl.SimpleDynArray.split dyn k
  left' = left `DatastrucImpl.SimpleDynArray.join` (DynArray 1 (Leaf x))
  dyn' = DatastrucImpl.SimpleDynArray.multiinsert x klist right
  in
  left' `DatastrucImpl.SimpleDynArray.join` dyn'


mapMonad :: (Monad m) => (a -> m b) -> DynArray a -> m (DynArray b) 
mapMonad f (DynArray n a) = (mapMonad' f a) >>= (\a' -> return $ DynArray n a') 

mapMonad' :: (Monad m) => (a -> m b) -> DynArr a -> m (DynArr b) 
mapMonad' f (Leaf e) = (f e) >>= (\e' -> return $ Leaf e')
mapMonad' f (Node llen l r) = do
                                l' <- mapMonad' f l
                                r' <- mapMonad' f r
                                return $ Node llen l' r'



-------------------------------------------------------------
-- Testing

-- construction and deconstruction

toList :: DynArray a -> [a]
toList (DynArray n arr) = map (get' arr) [0..n-1]

naiveFromList :: [a] -> DynArray a
naiveFromList l = foldl cons empty l 
  where cons :: DynArray a -> a -> DynArray a
        cons arr a = arr `join` leaf a

fromList :: [a] -> DynArray a
fromList l = let n = length l in 
  if n <= 0 then empty else fromList' n l

fromList' :: Int -> [a] -> DynArray a
fromList' 1 [a] = leaf a
fromList' n l = 
  let nl    = n `div` 2
      nr    = n - nl
      (ll,lr) = splitAt nl l
      DynArray llen left  = fromList'  nl ll
      DynArray rlen right = fromList'  nr lr
  in  DynArray (llen + rlen) $ Node llen left right     

prop_equal :: Eq a => DynArray a -> DynArray a -> Bool
prop_equal a1 a2 = toList a1 == toList a2

-- properties

type A = Char

prop_joinSplit' :: Int -> [A] -> Property
prop_joinSplit' i l = 0 <= i && i <= length l ==> prop_equal a $ uncurry join (split a i) where a = fromList l
{-
prop_joinSplit :: Eq a => Int -> DynArray a -> Bool
prop_joinSplit i a = prop_equal a $ uncurry join (split a i) 
-}

prop_joinSplit :: [A] -> Property
prop_joinSplit l = forAll (choose (0,length l)) $ \ i -> prop_equal a $ uncurry join (split a i) 
    where a = fromList l

prop_joinSplitNaive :: String -> Property
prop_joinSplitNaive l = forAll (choose (0,length l)) $ \ i -> prop_equal a $ uncurry join (split a i) 
    where a = naiveFromList l

prop_joinSplitNaivePretty :: Property
prop_joinSplitNaivePretty = forAll (listOf (elements ['a'..'z'])) $ \ l ->
  let a = naiveFromList l in
  forAll (choose (0,length l)) $ \ i -> prop_equal a $ uncurry join (split a i) 


check_balancing :: DynArray a -> Bool
check_balancing d =
  let
  prop_b' :: DynArr a -> Int -> Bool
  prop_b' _ 0 = True
  prop_b' (Leaf _) _ = True
  prop_b' (Node llen l r) i = (balanced llen (i-llen)) && (prop_b' l llen) && (prop_b' r (i-llen))
  in
  prop_b' (array d) (len d)

prop_split_is_balanced :: ([A] -> DynArray a) -> Property
prop_split_is_balanced f = forAll (listOf (elements ['a'..'z'])) $ \ l ->
  forAll (choose (0, length l)) $ \ i ->  
  let 
  a = f l 
  (a1, a2) = split a i
  in check_balancing a1 && check_balancing a2

prop_join_is_balanced :: ([A] -> DynArray a) -> ([A] -> DynArray a) -> Property
prop_join_is_balanced f g = 
  forAll (listOf (elements ['a'..'z'])) $ \ l1 ->
  forAll (listOf (elements ['a'..'z'])) $ \ l2 ->
  let a1 = f l1; a2 = g l2
  in check_balancing (a1 `join` a2)

prop_join_is_balanced' :: ([A] -> DynArray a) -> Property
prop_join_is_balanced' f =
  forAll (listOf (listOf (elements ['a'..'z']))) $ \ l ->
  let 
  arrList = map f l
  bigJoin = foldl join empty arrList
  in
  check_balancing bigJoin
  
  
-- concrete tests

test = quickCheck prop_joinSplit
testv = verboseCheck prop_joinSplit
testn = quickCheck prop_joinSplitNaive
testnv = verboseCheck prop_joinSplitNaive
testnvp = verboseCheck prop_joinSplitNaivePretty

testSplitBalanced = verboseCheck $ prop_split_is_balanced fromList
testJoinBalanced = verboseCheck $ prop_join_is_balanced naiveFromList fromList

testMultiJoinBalanced = verboseCheck $ prop_join_is_balanced' fromList


{-
failed before bugfix (using naiveFromList):
join 10, 3
-}
array10 = naiveFromList ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
array3  = naiveFromList ['X', 'Y', 'Z']


