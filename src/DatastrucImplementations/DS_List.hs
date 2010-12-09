module DatastrucImplementations.DS_List where

import Data.List

import DataStructure 


data List a = L {val :: [a]}


instance DataStruc List a where
  empty = L []
  
  insert x 0 (L li) = L (x:li)
  insert x (n+1) (L (l:li)) = L (l:(val (DataStructure.insert x n (L li))))
  
  multiinsert x [] li = li
  multiinsert x (0:klist) (L li) = L (x : val(multiinsert x klist (L li)))
  multiinsert x ((n+1):klist) (L (l:li)) = L (l : val (multiinsert x (n:klist) (L li)))
  
  get (L (x:l)) 0 = x
  get (L (l:li)) (n+1) = get (L li) n
    
  split i (L l) = let (l1, l2) = splitAt i l in (L l1, L l2)
  
  join (L l1) (L l2) = L (l1 ++ l2)
  
  size (L l) = length l

