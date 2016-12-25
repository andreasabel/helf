module TestDynArray where

import Test.QuickCheck

import DatastrucImpl.StrictDynArray

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
