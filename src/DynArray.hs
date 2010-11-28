-- change name.
module DynArray where

import GHC.Float
import Prelude hiding (repeat)
import qualified Data.List as List

import Text.PrettyPrint (Doc,nest,vcat,text,render)
import qualified Text.PrettyPrint as PP

import Debug.Trace

data DynArr a 
  = Repeat { entry :: a }  -- ^ an array filled with element @entry@ 
  | Join { leftLen :: Int  -- ^ length of left part
         , left  :: DynArr a -- ^ left part
         , right :: DynArr a -- ^ right part
         }
    deriving (Eq,Ord,Show)

data DynArray a = DynArray { len :: Int, array :: DynArr a }
    deriving (Eq,Ord)
  
pretty :: Show a => DynArray a -> Doc
pretty (DynArray 0 _) = text "empty"
pretty (DynArray 1 (Repeat a)) = text $ show a
pretty (DynArray n (Repeat a)) = text $ show n ++ " * " ++ show a
pretty (DynArray n (Join k l r)) = vcat 
  [ nest 2 $ pretty $ DynArray k l
  , text (show k)
  , nest 2 $ pretty $ DynArray (n-k) r
  ]

instance Show a => Show (DynArray a) where
  show = render . pretty


empty :: DynArray a 
empty = DynArray 0 $ error "empty dynamic array"

repeat :: Int -> a -> DynArray a
repeat i a = DynArray i $ Repeat a

-- | Array lookup with bounds checking.
(!) :: DynArray a -> Int -> a
arr ! i | 0 <= i && i < len arr = get (array arr) i 
        | otherwise = error $ "index " ++ show i ++ " out of range"

-- | Lookup without bounds checking.
get :: DynArr a -> Int -> a
get (Repeat a) i = a
get (Join llen left right) i 
  | i < llen  = get left i
  | otherwise = get right (i - llen)

-- | Splitting the array. O(log 1 + log 2 + log 3 + ... + log n) =< [or '='?] O((log n)^2), assuming join is O(log n), where n is the number of hold information
split :: DynArray a -> Int -> (DynArray a, DynArray a)
split arr i 
  | i <= 0 = (empty, arr)
  | i >= len arr = (arr, empty)
  | otherwise = 
      let (left, right) = split' (len arr) (array arr) i
      in  (DynArray i left, DynArray (len arr - i) right)

split' :: Int -> DynArr a -> Int -> (DynArr a, DynArr a)
split' len arr i =
  case arr of
    Repeat a -> (Repeat a, Repeat a)
    Join llen left right -> 
      if llen == i then (left, right)
       else if i < llen then 
         let (l1, l2) = split' llen left i 
             l2' = DynArray (llen - i) l2 `join` DynArray (len - llen) right
         in (l1, array l2')
       else
         let (r1, r2) = split' (len - llen) right (i - llen)
             -- r1 has length i - llen
             -- r2 has length len - i
             r1' = DynArray llen left `join` DynArray (i - llen) r1
         in  (array r1', r2)


-- | Balancing join. Works for balancingFactor == 2.
-- Reaching certain points in the following function assures that certain inequalities (which we want to note down) are valid. For better readability we identify a tree's name with it's size and use the shortcut "a1 ~ a2" for "a1 <= k*a2 && k*a1 >= a2" as well as "a1 >> a2" for "a1 > k*a2" (where k is the maximal unbalance factor, 3).
join :: DynArray a -> DynArray a -> DynArray a
join     (DynArray 0  _) a_2@(DynArray n2 _) = a_2
join a_1@(DynArray n1 _)     (DynArray 0  _) = a_1
join a_1@(DynArray n1 a1) a_2@(DynArray n2 a2) = trace ("join " ++ show n1 ++ " " ++ show n2) $
  let n = n1 + n2 
      join4 n11 a11 n12 a12 n21 a21 a22 = DynArray n $ Join (n11 + n12) 
        (Join n1 a11 a12) (Join n21 a21 a22) in
  case joinable a_1 a_2 of
    Ok -> DynArray n (Join n1 a1 a2)
    LeftTooBig n11 a11 a12 ->
      -- a_1 >> a_2 , a11+a12 = a_1 , a11 ~ a12
      -- recursively join middle and right part
      let DynArray _ a2'@(Join n21' a21' a22') = DynArray (n1 - n11) a12 `join` a_2
      -- a21'+a22' = a12+a_2 , a21' ~ a22'
      in  if balanced n11 (n - n11) 
          then DynArray n (Join n11 a11 a2')
           -- the new middle node could be too big
          else 
            -- a11 !~ a21'+a22'
            if balanced n11 n21' && balanced (n11+n21') (n-n11-n21')  
            -- note that this is only possible if a2' =/= Join (n1-n11) a12 a2, which is the case if a2' was build 'trivially'
              then DynArray n (Join (n11+n21') (Join n11 a11 a21') a22') 
              else case a21' of -- split middle node
              -- a11 << a21' || a11+a21' >> a22' , a21' = a211'+a212' , a211' ~ a212'
                    -- check that none of a11 !~ a211', a212' !~ a22' , a212' !~ a22' , a1+a211' ~ a212'+a22' is satisfied
                    Join n211' a211' a212' -> join4 n11 a11 n211' a211' (n21' - n211') a212' a22'
                    Repeat x -> let n211' = n21' `div` 2 
                                in join4 n11 a11 n211' (Repeat x) (n21' - n211') (Repeat x) a22'
    RightTooBig n21 a21 a22 ->
      let DynArray _ a1'@(Join n11' a11' a12') = a_1 `join` DynArray n21 a21
      in  if balanced (n1 + n21) (n2 - n21) 
           then DynArray n (Join (n1 + n21) a1' a22)
           else
             if balanced n11' (n-n11') && balanced (n1+n21-n11') (n2-n21)
                then DynArray n (Join n11' a11' (Join (n1+n21-n11') a12' a22))
                else case a12' of -- split middle node
                  Join n121' a121' a122' -> join4 n11' a11' n121' a121' (n1 + n21 - n11' - n121') a122' a22
                  Repeat x -> let n121_122 = n1 + n21 - n11'
                                  n121' = n121_122 `div` 2
                              in join4 n11' a11' n121' (Repeat x) (n121_122 - n121') (Repeat x) a22 -- last argument CORRECTED!


-- now working:
badExampleTree1 = Join 17 (Repeat 'x') (Join 20 (Join 10 (Repeat 'y') (Repeat 'z')) (Repeat 'w'))
bad1 = DynArray 67 badExampleTree1
badExampleTree2 = Repeat 'v'
bad2 = DynArray 10 badExampleTree2
bad = join bad1 bad2





data Joinable a 
  = Ok
  | LeftTooBig Int (DynArr a) (DynArr a)
  | RightTooBig Int (DynArr a) (DynArr a)

{- maximal unbalancing is

  balancingFactor + 1
  -------------------
  balancingFactor - 1
-}

balancingFactor = 2  -- means 3 : 1

balanced :: Int -> Int -> Bool
balanced n1 n2 = balancingFactor * abs (n1 - n2) <= n1 + n2

joinable :: DynArray a -> DynArray a -> Joinable a
joinable (DynArray n1 arr1) (DynArray n2 arr2) = 
  if balanced n1 n2 then Ok
  else case (arr1, arr2) of
         (Join llen left right, _) | n1 > n2 -> LeftTooBig llen left right 
         (_, Join llen left right) | n2 > n1 -> RightTooBig llen left right 
--         _ -> Ok          This would cause problems. 
--                          (Using this, we would get a 'pseudo-balanced' tree. The 'too big' Repeat node could be split during the recursive call of join, creating a tree which would not be 'pseudo-balanced' any more, but totally unbalanced.)
--                          Hence (unfortunately, I do not see any better solution):
         (Repeat x, _) | n1 > n2 -> LeftTooBig (n1 `div` 2) (Repeat x) (Repeat x)
         (_, Repeat x) | n2 > n1 -> RightTooBig (n2 `div` 2) (Repeat x) (Repeat x)

-- | Insertion (unbalanced).  @insert arr i a@
-- insert :: DynArray a -> Int -> DynArray a -> DynArray a

-- first implementation of insertion. O((log n)^2) [?], probably there are better ideas.
arrayInsert :: a -> Int -> DynArray a -> DynArray a
arrayInsert x i a@(DynArray k arr) | i <= 0 = insert' x 0 a
arrayInsert x i a@(DynArray k arr) | i >= k = insert' x k a
arrayInsert x i a                           = insert' x i a

insert' :: a -> Int -> DynArray a -> DynArray a
insert' x i a@(DynArray k arr) =
  let
  (left, right) = split a i
  left' = left `join` (DynArray 1 (Repeat x))
  in
  left' `join` right

-- Testing

toList :: DynArray a -> [a]
toList (DynArray n arr) = map (get arr) [0..n-1]

fromFreqList :: [(Int,a)] -> DynArray a
fromFreqList l = let n = length l in 
  if n <= 0 then empty else fromFreqList' n l

fromFreqList' :: Int -> [(Int,a)] -> DynArray a
fromFreqList' 1 [(i,a)] = repeat i a
fromFreqList' n l = 
  let nl    = n `div` 2
      nr    = n - nl
      (ll,lr) = splitAt nl l
      DynArray llen left  = fromFreqList'  nl ll
      DynArray rlen right = fromFreqList'  nr lr
  in  DynArray (llen + rlen) $ Join llen left right     

mkFreqList :: Eq a => [a] -> [(Int,a)]
mkFreqList l = map (\ (a:as) -> (length as + 1, a)) $ List.group l

mkArr :: Eq a => [a] -> DynArray a
mkArr = fromFreqList . mkFreqList 

a1 = mkArr "Mississippi"
l1 = toList a1
(a1l,a1r) = split a1 5
(a1l2,a1r2) = split a1 3

buildByJoin :: [(Int,a)] -> DynArray a
buildByJoin [] = empty
buildByJoin ((i,a):l) = repeat i a `join` buildByJoin l

mkArr' = buildByJoin . mkFreqList

a1' = mkArr' "Mississippi"
(a1a,a1b) = split a1' 8

build2 [] = empty
build2 l  = uncurry join $ bla $ map (uncurry repeat) l
  where bla (hd:tl) = (hd, foldl join empty tl)

foo l = bla $ map (uncurry repeat) l
  where bla (hd:tl) = (hd, foldl join empty tl)

a0 = foo (mkFreqList "Mississippi")
b0 = uncurry join a0