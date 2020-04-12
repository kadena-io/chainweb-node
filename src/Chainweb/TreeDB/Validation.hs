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

import Control.Monad

import Data.CAS
import qualified Data.HashMap.Strict as HM

import Prelude hiding (lookup)

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Validation
import Chainweb.ChainValue
import Chainweb.Time

-- -------------------------------------------------------------------------- --
-- Validate Blockheader

-- | Tests if the block header is valid (i.e. 'validateBlockHeader' produces an
-- empty list)
--
-- This doesn't include any payload validation.
--
isValidEntryDb
    :: HasCasLookup db
    => CasValueType db ~ ChainValue BlockHeader
    => db
        -- ^ the database
    -> BlockHeader
        -- ^ The block header to be checked
    -> IO Bool
        -- ^ True if validation succeeded
isValidEntryDb db h = validateAllParentsExist (chainLookup db) h >>= \case
    Left _ -> return False
    Right w -> do
        t <- getCurrentTimeIntegral
        return $ isValidBlockHeader t w

-- | Validate properties of the block header, producing a list of the validation
-- failures
--
-- This doesn't include any payload validation.
--
validateEntryDb
    :: HasCasLookup db
    => CasValueType db ~ ChainValue BlockHeader
    => db
        -- ^ the database
    -> BlockHeader
        -- ^ The block header to be checked
    -> IO [ValidationFailureType]
        -- ^ A list of ways in which the block header isn't valid
validateEntryDb db h = validateAllParentsExist (chainLookup db) h >>= \case
    Left e -> return [e]
    Right w -> do
        t <- getCurrentTimeIntegral
        return $ validateBlockHeader t w

-- | Validate properties of the block header, throwing an exception detailing
-- the failures if any.
--
-- This doesn't include any payload validation.
--
validateEntryDbM
    :: HasCasLookup db
    => CasValueType db ~ ChainValue BlockHeader
    => db
        -- ^ the database
    -> BlockHeader
        -- ^ The block header to be checked
    -> IO ()
validateEntryDbM db h = do
    t <- getCurrentTimeIntegral
    void $ validateBlockHeaderM t (chainLookup db) h

-- | Validate a set of additions that are supposed to be added atomically to
-- the database.
--
-- This doesn't include any payload validation.
--
validateAdditionsDbM
    :: HasCasLookup db
    => CasValueType db ~ ChainValue BlockHeader
    => db
        -- ^ the database
    -> HM.HashMap BlockHash BlockHeader
        -- ^ The set of block headers to be checked
    -> IO ()
validateAdditionsDbM db h = do
    t <- getCurrentTimeIntegral
    void $ validateBlockHeadersM t (chainLookup db) h
