{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.TreeDB.Validation
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.TreeDB.Validation
(
  -- * Validation
  isValidEntryDb
, validateEntryDb
, validateEntryDbM
, validateAdditionsDbM
) where

import Control.Monad.Catch

import qualified Data.HashMap.Strict as HM

import Prelude hiding (lookup)

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Validation
import Chainweb.TreeDB

-- -------------------------------------------------------------------------- --
-- Validate Blockheader

-- | Tests if the block header is valid (i.e. 'validateBlockHeader' produces an
-- empty list)
--
isValidEntryDb
    :: (TreeDb db, DbEntry db ~ BlockHeader)
    => db
        -- ^ the database
    -> BlockHeader
        -- ^ The block header to be checked
    -> IO Bool
        -- ^ True if validation succeeded
isValidEntryDb db h = validateBlockParentExists (lookup db) h >>= \case
    Left _ -> return False
    Right p -> return $ isValidBlockHeader p h

-- | Validate properties of the block header, producing a list of the validation
-- failures
--
validateEntryDb
    :: (TreeDb db, DbEntry db ~ BlockHeader)
    => db
        -- ^ the database
    -> BlockHeader
        -- ^ The block header to be checked
    -> IO [ValidationFailureType]
        -- ^ A list of ways in which the block header isn't valid
validateEntryDb db h = validateBlockParentExists (lookup db) h >>= \case
    Left e -> return [e]
    Right p -> return $ validateBlockHeader p h

-- | Validate properties of the block header, throwing an exception detailing
-- the failures if any.
--
validateEntryDbM
    :: (TreeDb db, DbEntry db ~ BlockHeader)
    => db
        -- ^ the database
    -> BlockHeader
        -- ^ The block header to be checked
    -> IO ()
validateEntryDbM db h = validateBlockParentExists (lookup db) h >>= \case
    Left e -> throwM $ ValidationFailure Nothing h [e]
    Right p -> validateBlockHeaderM p h

-- | Validate a set of additions that are supposed to be added atomically to
-- the database.
--
validateAdditionsDbM
    :: (TreeDb db, DbEntry db ~ BlockHeader)
    => db
        -- ^ the database
    -> HM.HashMap BlockHash BlockHeader
        -- ^ The set of block headers to be checked
    -> IO ()
validateAdditionsDbM = validateBlocksM . lookup

