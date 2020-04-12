{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module: Data.IVar
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
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

-- | The class of readable immutable variables.
--
class IsRIVar v where
    tryReadIVar :: v a -> IO (Maybe a)
    awaitIVar :: v a -> IO a

-- | The class of writeable immutable variables.
--
class IsWIVar v where
    putIVar :: v a -> a -> IO Bool
    putIVar' :: NFData a => v a -> a -> IO Bool

-- | The class of immutable variables that are readable and writeable.
--
type IsIVar v = (IsRIVar v, IsWIVar v)

-- -------------------------------------------------------------------------- --
-- IVar

-- | An immutable 'MVar'. Once a value has been assigned to the variable it
-- can't be changed any more.
--
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

-- | Create a new immutable variable. Intitially the variable is empty. Once a
-- value got assigned that value can't be changed any more.
--
newIVar :: IO (IVar a)
newIVar = IVar <$> newEmptyMVar
{-# INLINE newIVar #-}

-- | Create an weak 'IVar'. For details and caveats regarding weak references,
-- please, see the documentation for 'mkWeakMVar'.
--
mkWeakIVar :: IVar a -> IO () -> IO (Weak (IVar a))
mkWeakIVar (IVar var) f = coerce <$> mkWeakMVar var f
{-# INLINE mkWeakIVar #-}

-- -------------------------------------------------------------------------- --
-- Writeable IVar

-- | An immutable variable that one can write to (exactly once), but that isn't
-- readable. This is, for instance, useful to model one end of a one-directional
-- communitcation channel.
--
newtype WIVar a = WIVar (IVar a)
    deriving (Generic)
    deriving newtype (IsWIVar)

-- | Restrict an 'IVar' to be only writeabel but not readable.
--
wIVar :: IVar a -> WIVar a
wIVar = WIVar
{-# INLINE wIVar #-}

-- -------------------------------------------------------------------------- --
-- Readable IVar

-- | An immutable variable that one can write to (exactly once), but that isn't
-- readable. This is, for instance, useful to model one end of a one-directional
-- communitcation channel.
--
newtype RIVar a = RIVar (IVar a)
    deriving (Generic)
    deriving newtype (IsRIVar)

-- | Restrict an 'IVar' to be only readable but not writeable.
--
rIVar :: IVar a -> RIVar a
rIVar = RIVar
{-# INLINE rIVar #-}
