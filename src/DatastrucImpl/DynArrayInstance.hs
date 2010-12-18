module DatastrucImpl.DynArrayInstance where

import DataStructure
import qualified DatastrucImpl.SimpleDynArray as DA

instance DataStruc DA.DynArray a where
  -- show = render . pretty
  empty             = DA.empty
  insert            = DA.insert
  multiinsert       = DA.multiinsert
  get               = DA.get
  split i datastruc = DA.split datastruc i
  join              = DA.join
  size              = DA.len
