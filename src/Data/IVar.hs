{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module: Data.IVar
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Strict immutable variables with distinguished read and write end points.
--
module Data.IVar
(
-- * Classes
  IsRIVar
, awaitIVar
, tryReadIVar
, IsWIVar
, IsIVar
, putIVar

-- * IVar
, IVar
, newIVar
, mkWeakIVar

-- * Writable IVar
, WIVar
, wIVar

-- * Readble IVar
, RIVar
, rIVar
) where

import Control.Concurrent.MVar
import Control.DeepSeq

import Data.Coerce

import GHC.Generics

import System.Mem.Weak

-- -------------------------------------------------------------------------- --
-- Type Classes

class IsRIVar v where
    tryReadIVar :: v a -> IO (Maybe a)
    awaitIVar :: v a -> IO a

class IsWIVar v where
    putIVar :: v a -> a -> IO Bool
    putIVar' :: NFData a => v a -> a -> IO Bool

type IsIVar v = (IsRIVar v, IsWIVar v)

-- -------------------------------------------------------------------------- --
-- IVar

newtype IVar a = IVar (MVar a)
    deriving (Generic)

instance IsRIVar IVar where
    tryReadIVar (IVar v) = tryReadMVar v
    {-# INLINE tryReadIVar #-}
    awaitIVar (IVar v) = readMVar v
    {-# INLINE awaitIVar #-}

instance IsWIVar IVar where
    putIVar (IVar var) a = tryPutMVar var $! a
    {-# INLINE putIVar #-}
    putIVar' (IVar var) a = tryPutMVar var $!! a
    {-# INLINE putIVar' #-}

newIVar :: IO (IVar a)
newIVar = IVar <$> newEmptyMVar
{-# INLINE newIVar #-}

mkWeakIVar :: IVar a -> IO () -> IO (Weak (IVar a))
mkWeakIVar (IVar var) f = coerce <$> mkWeakMVar var f
{-# INLINE mkWeakIVar #-}

-- -------------------------------------------------------------------------- --
-- Writeable IVar

newtype WIVar a = WIVar (IVar a)
    deriving (Generic)
    deriving newtype (IsWIVar)

wIVar :: IVar a -> WIVar a
wIVar = WIVar
{-# INLINE wIVar #-}

-- -------------------------------------------------------------------------- --
-- Readable IVar

newtype RIVar a = RIVar (IVar a)
    deriving (Generic)
    deriving newtype (IsRIVar)

rIVar :: IVar a -> RIVar a
rIVar = RIVar
{-# INLINE rIVar #-}

