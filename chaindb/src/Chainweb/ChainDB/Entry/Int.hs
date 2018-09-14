{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.ChainDB.Entry.Int
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Implements "Chainweb.ChainDB.Entry"
--
module Chainweb.ChainDB.Entry.Int
( Key
, Entry
, key
, parent
, rank
, encodeEntry
, decodeEntry

-- implementation specific
, root
, entry
) where

import Control.Monad.Catch

import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Signed
import qualified Data.ByteString as B
import Data.Hashable
import qualified Data.Text as T

import GHC.Generics

import Numeric.Natural


type Key = Int

-- | Type of a database entry
--
data Entry = Entry
    { _entryRank :: !Int
    , _entryParent :: !Int
    , _entryInt :: !Int
    }
    deriving (Show, Eq, Generic, Hashable)

root :: Entry
root = Entry 0 (-1) 0

entry :: Entry -> Int -> Entry
entry p i
    | i < 1 = error "entry must have a positive value"
    | otherwise = Entry (_entryRank p + 1) (key p) i

key :: Entry -> Key
key = hash

parent :: Entry -> Maybe Key
parent e
    | _entryParent e == (-1) = Nothing
    | otherwise = Just (_entryParent e)

rank :: Entry -> Natural
rank = fromIntegral . _entryRank

encodeEntry :: Entry -> B.ByteString
encodeEntry e = runPutS $ do
    putWordhost (unsigned $ _entryRank e)
    putWordhost (unsigned $ _entryParent e)
    putWordhost (unsigned $ _entryInt e)

decodeEntry :: MonadThrow m => B.ByteString -> m Entry
decodeEntry bytes = either (throwM . DecodeFailure . T.pack) return
    $ flip runGetS bytes $ Entry
        <$> (signed <$> getWordhost)
        <*> (signed <$> getWordhost)
        <*> (signed <$> getWordhost)

-- -------------------------------------------------------------------------- --
-- Exceptions

newtype IntEntryException = DecodeFailure T.Text
    deriving (Show)

instance Exception IntEntryException

