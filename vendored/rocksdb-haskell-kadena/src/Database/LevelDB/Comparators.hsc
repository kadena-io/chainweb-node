{-# LANGUAGE ForeignFunctionInterface #-}

module Database.LevelDB.Comparators where

import Foreign
import Database.LevelDB.Base

foreign import ccall unsafe "&numcmp_comparator"
  c_numcmp_comparator :: FunPtr ComparatorPtr

foreign import ccall unsafe "&strcmp_comparator"
  c_strcmp_comparator :: FunPtr ComparatorPtr
