{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.TreeDB.Validation
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
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
import Chainweb.Time
import Chainweb.TreeDB

-- -------------------------------------------------------------------------- --
-- Validate Blockheader

-- | Tests if the block header is valid (i.e. 'validateBlockHeader' produces an
-- empty list)
--
-- This doesn't include any payload validation.
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
    Right p -> do
        t <- getCurrentTimeIntegral
        return $ isValidBlockHeader t p h

-- | Validate properties of the block header, producing a list of the validation
-- failures
--
-- This doesn't include any payload validation.
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
    Right p -> do
        t <- getCurrentTimeIntegral
        return $ validateBlockHeader t p h

-- | Validate properties of the block header, throwing an exception detailing
-- the failures if any.
--
-- This doesn't include any payload validation.
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
    Right p -> do
        t <- getCurrentTimeIntegral
        validateBlockHeaderM t p h

-- | Validate a set of additions that are supposed to be added atomically to
-- the database.
--
-- This doesn't include any payload validation.
--
validateAdditionsDbM
    :: (TreeDb db, DbEntry db ~ BlockHeader)
    => db
        -- ^ the database
    -> HM.HashMap BlockHash BlockHeader
        -- ^ The set of block headers to be checked
    -> IO ()
validateAdditionsDbM db h = do
    t <- getCurrentTimeIntegral
    validateBlocksM t (lookup db) h
