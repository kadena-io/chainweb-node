{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Test.Utils.TestHeader
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Respresent a TestHeader with all of it direct dependencies
--
module Chainweb.Test.Utils.TestHeader
( TestHeader(..)
, testHeaderHdr
, testHeaderParent
, testHeaderAdjs
, testHeader
, testHeaderChainLookup
, genesisTestHeader
, genesisTestHeaders
, queryTestHeader
, queryTestHeaderByHeight
, arbitraryTestHeader
, arbitraryTestHeaderHeight
) where

import Chainweb.BlockHash

import Control.Lens hiding ((.=))

import Data.Aeson
import Data.Aeson.Types
import Data.CAS
import Data.Foldable
import qualified Data.HashMap.Strict as HM

import Debug.Trace

import GHC.Generics
import GHC.Stack

import Test.QuickCheck (chooseEnum)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen)

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeight
import Chainweb.ChainValue
import Chainweb.Test.Orphans.Internal
import Chainweb.Test.Utils.ApiQueries
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- TestHeader

data TestHeader = TestHeader
    { _testHeaderHdr :: !BlockHeader
    , _testHeaderParent :: !ParentHeader
    , _testHeaderAdjs :: ![ParentHeader]
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''TestHeader

instance HasChainId TestHeader where
    _chainId = _chainId . _testHeaderHdr
    {-# INLINE _chainId #-}

instance HasChainwebVersion TestHeader where
    _chainwebVersion = _chainwebVersion . _testHeaderHdr
    {-# INLINE _chainwebVersion #-}

instance HasChainGraph TestHeader where
    _chainGraph = _chainGraph . _testHeaderHdr
    {-# INLINE _chainGraph #-}

instance HasCasLookup TestHeader where
    type CasValueType TestHeader = BlockHeader
    casLookup h = return . testHeaderLookup h
    {-# INLINE casLookup #-}

testHeaderLookup :: TestHeader -> BlockHash -> Maybe BlockHeader
testHeaderLookup testHdr x = lookup x cas
  where
    h = _testHeaderHdr testHdr
    p = _parentHeader $ _testHeaderParent testHdr
    a = _testHeaderAdjs testHdr
    cas
        = (_blockHash h, h)
        : (_blockHash p, p)
        : fmap (\(ParentHeader b) -> (_blockHash b, b)) a
{-# INLINE testHeaderLookup #-}

instance FromJSON TestHeader where
    parseJSON = withObject "TestHeader" $ \o -> TestHeader
        <$> o .: "header"
        <*> (ParentHeader <$> o .: "parent")
        <*> (fmap ParentHeader <$> o .: "adjacents")
    {-# INLINE parseJSON #-}

instance ToJSON TestHeader where
    toJSON o = object
        [ "header" .= _testHeaderHdr o
        , "parent" .= _parentHeader (_testHeaderParent o)
        , "adjacents" .= fmap _parentHeader (_testHeaderAdjs o)
        ]
    {-# INLINE toJSON #-}

-- | An unsafe convenience functions for hard coding test headers in the code
-- use Aeson syntax. Cf. Test.Chainweb.BlockHeader.Validation for examples.
--
testHeader :: HasCallStack => [Pair] -> TestHeader
testHeader v = case fromJSON (object v) of
    Success a -> a
    e -> error (show e)
{-# INLINE testHeader #-}

-- -------------------------------------------------------------------------- --
-- arbitrary TestHeader

-- | Create an arbitrary test header.
--
-- Parents are not valid headers but only serve validation of the test header.
--
-- This construction will satisfy all block header valdiation properties except
-- for POW.
--
arbitraryTestHeader :: ChainwebVersion -> ChainId -> Gen TestHeader
arbitraryTestHeader v cid = do
    h <- chooseEnum (genesisHeight v cid, maxBound `div` 2)
    arbitraryTestHeaderHeight v cid h
{-# INLINE arbitraryTestHeader #-}

arbitraryTestHeaderHeight
    :: ChainwebVersion
    -> ChainId
    -> BlockHeight
    -> Gen TestHeader
arbitraryTestHeaderHeight v cid h = do
    parent <- ParentHeader <$> arbitraryBlockHeaderVersionHeightChain v h cid
    trace "a" $ return ()

    -- TODO: support graph changes in arbitary?
    as <- fmap HM.fromList
        $ traverse (\c -> (c,) <$> arbitraryBlockHeaderVersionHeightChain v h c)
        $ toList
        $ adjacentChainIds (chainGraphAt v h) cid
    nonce <- arbitrary
    payloadHash <- arbitrary
    let pt = maximum $ _bct . _blockCreationTime
            <$> HM.insert cid (_parentHeader parent) as
    t <- BlockCreationTime <$> chooseEnum (pt, maxBound)
    return $ TestHeader
        { _testHeaderHdr = newBlockHeader (ParentHeader <$> as) payloadHash nonce t parent
        , _testHeaderParent = parent
        , _testHeaderAdjs = toList $ ParentHeader <$> as
        }

-- -------------------------------------------------------------------------- --
-- HasCasLookup for ChainValue

-- | Convenience function that can be used with BlockHeader validation functions.
--
testHeaderChainLookup
    :: Applicative m
    => TestHeader
    -> ChainValue BlockHash
    -> m (Maybe BlockHeader)
testHeaderChainLookup h x = pure $! testHeaderLookup h $ _chainValueValue x
{-# INLINE testHeaderChainLookup #-}

-- -------------------------------------------------------------------------- --
-- Genesis Test Headers

genesisTestHeaders
    :: HasChainwebVersion v
    => v -> [TestHeader]
genesisTestHeaders v = genesisTestHeader v <$> toList (chainIds v)

genesisTestHeader
    :: HasChainwebVersion v
    => HasChainId c
    => v
    -> c
    -> TestHeader
genesisTestHeader v cid = TestHeader
    { _testHeaderHdr = gen
    , _testHeaderParent = ParentHeader gen
    , _testHeaderAdjs = ParentHeader . genesisBlockHeader (_chainwebVersion v)
        <$> toList (adjacentChainIds (_chainGraph gen) cid)
    }
  where
    gen = genesisBlockHeader (_chainwebVersion v) (_chainId cid)

-- -------------------------------------------------------------------------- --
-- Query TestHeader from a network

queryTestHeader
    :: HasCallStack
    => HasChainwebVersion v
    => HasChainId c
    => v
    -> c
    -> BlockHash
    -> IO TestHeader
queryTestHeader v c h = do
    mgr <- mkMgr
    hdr <- getHeaderByHash mgr ver cid h
    parent <- getHeaderByHash mgr ver cid $ _blockParent hdr
    ads <- itraverse (\ac a -> ParentHeader <$> getHeaderByHash mgr ver ac a)
        $ _getBlockHashRecord
        $ _blockAdjacentHashes hdr
    return $ TestHeader
        { _testHeaderHdr = hdr
        , _testHeaderParent = ParentHeader parent
        , _testHeaderAdjs = toList ads
        }
  where
    ver = _chainwebVersion v
    cid = _chainId c

queryTestHeaderByHeight
    :: HasCallStack
    => HasChainwebVersion v
    => HasChainId c
    => v
    -> c
    -> BlockHeight
    -> IO TestHeader
queryTestHeaderByHeight v c h = do
    mgr <- mkMgr
    hdr <- getHeaderByHeight mgr ver cid h
    parent <- getHeaderByHash mgr ver cid $ _blockParent hdr
    ads <- itraverse (\ac a -> ParentHeader <$> getHeaderByHash mgr ver ac a)
        $ _getBlockHashRecord
        $ _blockAdjacentHashes hdr
    return $ TestHeader
        { _testHeaderHdr = hdr
        , _testHeaderParent = ParentHeader parent
        , _testHeaderAdjs = toList ads
        }
  where
    ver = _chainwebVersion v
    cid = _chainId c

