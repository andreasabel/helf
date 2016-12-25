module DataStructure where

import Text.PrettyPrint (Doc)

class DataStruc datastruc a where
--show        :: dataStruc a -> String
  empty       :: datastruc a
  insert      :: a -> Int -> datastruc a -> datastruc a
  multiinsert :: a -> [Int] -> datastruc a -> datastruc a
  get         :: datastruc a -> Int -> a
  split       :: Int -> datastruc a -> (datastruc a, datastruc a)
  join        :: datastruc a -> datastruc a -> datastruc a
  size        :: datastruc a -> Int
  mapMonad    :: (Monad m) => (a -> m b) -> datastruc a -> m (datastruc b) -- better remove this and use Traversable (?)
