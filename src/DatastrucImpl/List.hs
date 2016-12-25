{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module DatastrucImpl.List where

import Data.List

import DataStructure

instance DataStruc [] a where
  empty = []
  insert x n l = left ++ x : right where (left,right) = splitAt n l
  split = splitAt
  join = (++)
  size = length
  get = (!!)
  multiinsert x ks l = val (multiinsert x ks (L l))
  mapMonad = mapM



newtype List a = L {val :: [a]}


instance DataStruc List a where
  empty = L []

  insert x 0 (L li) = L (x:li)
  insert x n (L (l:li)) = L (l:(val (DataStructure.insert x (n-1) (L li))))

  multiinsert x [] li = li
  multiinsert x (0:klist) (L li) = L (x : val(multiinsert x klist (L li)))
  multiinsert x (n:klist) (L (l:li)) = L (l : val (multiinsert x ((n-1):klist) (L li)))

  get (L (x:l)) 0 = x
  get (L (l:li)) n = get (L li) (n-1)

  split i (L l) = let (l1, l2) = splitAt i l in (L l1, L l2)

  join (L l1) (L l2) = L (l1 ++ l2)

  size (L l) = length l

  mapMonad f (L l) = (mapM f l) >>= (\x -> return $ L x)
