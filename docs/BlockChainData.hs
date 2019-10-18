{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: BlockChainData
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- = Append-only Content-Addressed Store (CAS)
--
-- All block chain data is stored append-only, content addressed key-value
-- stores (CAS). These stores have a few nice properties. In particular, there
-- can't be any conflicts between concurrent updates and thus there is no need
-- for defining conflict resolution. For every type stored in a CAS there must
-- be a well defined way how the key is computed. In this module we use Merkle
-- logs for defining the how the hashes that are used as keys are computed.
--
-- Some CAS stores are used to index data in other stores, i.e. the values in
-- the stores contain foreign keys for another store. By using Merkle logs for the
-- key computation we can construct compact inclusion proofs for values across
-- indexes in the store.
--
-- = Block Chain Data Schema
--
-- We distinguish between authoritative data and cached data.
--
-- Authoritative data can't be recovered if it is lost. Cached data can be
-- recomputed from the authoritative data, but doing so may be costly. Both
-- types of data can be authenticated. Authenticating cached data increases
-- robustness by providing additional checksums for the data in the chain and it
-- also improves performance, because it can be used in validation without
-- having to recompute the data.
--
-- The authoritative stores are
--
-- * 'BlockHeaderStore'
-- * 'BlockPayloadStore'
-- * 'BlockTransactionsStore'
--
-- The cached stores are
--
-- * 'BlockOutputsStore'
-- * 'OutputTreeStore'
-- * 'TransactionTreeStore'
--
-- = SPV Proofs
--
-- An SPV proof establishes that a transaction is included in the block chain.
-- The subject of the proof is the binary representation of the transaction. The
-- proof doesn't include any information beyond the inclusion of this
-- representation. In particular, it doesn't include any information about the
-- block in which the transaction is included, beyond what is stored in the
-- transaction itself. If any information about the context in the chain needs
-- to be proved, it must be included in the transaction in an authenticated
-- way.
--
-- If the information isn't available in authenticated form from the
-- transaction, it is possible to prove something about the context of a
-- transaction in the chain by using a two-step proof. This comes at the cost of
-- somewhat less efficient and less self-contained proofs. E.g one could use an
-- inclusion proof for the BlockHeader that contains the transaction and a
-- second that proves the inclusion of the transaction in the respective block.
-- Care has to be taken that the combination of the proofs actually establishes
-- the association between the transaction and the header.
--
-- For creating an SPV proof the user has to provide the following information:
--
-- 1. The position of the transaction in the sequence of transactions in block.
-- 2. The block height of the block into which the transaction is included.
--
-- The data structures in the module are only concerned with authenticating the
-- data in the chain establishing the consistency of the chain. It is up to the
-- user to maintain indexes that allow for efficient lookup of transactions and
-- blocks in order to obtain the inputs for creating an SPV proof.
--
-- [Lars:] I think, for cross chain transfers the block height isn't relevant.
-- For SPV the block height is of relevance, because confirmation depth matters
-- in the latter but not for the former.
--
-- = Example
--
-- The module includes examples of how to create instances for
--
-- * 'IsMerkleLogEntry',
-- * 'HasMerkleLog', and
-- * 'IsCas'.
--
-- It also contains an example for creating a chain of a given length, create an
-- 'ChainProof' for a transaction in the chain and verify the proof.
--
module BlockChainData
(
-- * Block Chain Data

  Transaction(..)
, TransactionOutput(..)
, BlockHeight(..)

-- ** Content Addressed Key Value Store

, IsCasValue(..)
, IsCas(..)
, CAS(..)

-- ** Hashes

, HashTag

, BlockHeaderHash(..)
, BlockPayloadHash(..)
, BlockTransactionsHash(..)
, BlockOutputsHash(..)
-- , BlockHeaderMerkleRoot(..)

-- ** Authorative Data

, BlockHeader(..)
, BlockHeaderStore(..)
, BlockPayload(..)
, BlockPayloadStore(..)
, BlockTransactions(..)
, BlockTransactionsStore(..)

-- ** Redundate Data / Caches

, BlockOutputs(..)
, BlockOutputsStore(..)
, TransactionTree(..)
, TransactionTreeStore(..)
, OutputTree(..)
, OutputTreeStore(..)

-- ** Create Data
, BlockTransactionsLog
, newTransactionLog
, newBlockTransactions
, transactionLog

, BlockOutputsLog
, newBlockOutputLog
, newBlockOutputs
, blockOutputLog

, blockPayload
, blockHeader
, genesisBlockHeader

-- * Chain Data Store
, ChainDb(..)
, emptyChainDb
, ChainCache(..)
, emptyCache
, ChainData(..)

-- * SPV Proofs
, ChainProof
, createChainProof
, runChainProof
, headerProof
, headerTree
, headerTree_
, bodyProof
, bodyTree
, bodyTree_

-- * Test
, createNewBlock
, genGenesisBlock
, genNewBlock
, genChain
, main
) where

import Control.Lens
import Control.Monad
import Control.Monad.Catch

import Crypto.Hash.Algorithms (SHA512t_256)

import Data.Bifunctor
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import Data.Foldable
import Data.Function
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Kind
import qualified Data.List.NonEmpty as N
import qualified Data.Map.Strict as M
import Data.MerkleLog
import qualified Data.Sequence as S
import qualified Data.Set as Set
import Data.Void
import Data.Word

import GHC.Generics
import GHC.Stack

import Numeric.Natural

import Test.QuickCheck

-- internal modules

import Chainweb.Crypto.MerkleLog

-- -------------------------------------------------------------------------- --
-- Orphans

instance Hashable BA.Bytes where
    hashWithSalt s bytes = s `hashWithSalt` BA.convert @_ @B.ByteString bytes
instance Hashable (MerkleHash a)
instance Hashable (MerkleRoot a)

-- -------------------------------------------------------------------------- --
-- Merkle Universe for Entry Types

-- | It is important that every entry in a MerkleLog is taggged with its type.
-- This prevents attacks where proofs for some entry type are faked by encoding
-- content for another entry type to match the former. For instance, a free-form
-- "comment" entry may be used to inject the root hash of a Merkle tree with
-- fake transaction values. Using tag ensure that a serialized entry of one two
-- different types can't be equal.
--
data EntryTag
    = BlockHeaderHashTag
    | BlockPayloadHashTag
    | HashTagTag
    | BlockOutputsHashTag
    | BlockTransactionsHashTag
    | TransactionTag
    | TransactionOutputTag
    | BlockHeightTag
    | ChainIdTag
    | VoidTag
    | MerkleRootTag
    deriving (Show, Eq)


instance MerkleUniverse EntryTag where
    type HashAlg EntryTag = SHA512t_256
    type MerkleTagVal EntryTag 'BlockHeaderHashTag = 0x0001
    type MerkleTagVal EntryTag 'BlockPayloadHashTag = 0x0002
    type MerkleTagVal EntryTag 'HashTagTag = 0x0003
    type MerkleTagVal EntryTag 'BlockOutputsHashTag = 0x0004
    type MerkleTagVal EntryTag 'BlockTransactionsHashTag = 0x0005
    type MerkleTagVal EntryTag 'TransactionTag = 0x0006
    type MerkleTagVal EntryTag 'TransactionOutputTag = 0x0007
    type MerkleTagVal EntryTag 'BlockHeightTag = 0x0008
    type MerkleTagVal EntryTag 'ChainIdTag = 0x0009
    type MerkleTagVal EntryTag 'VoidTag = 0x0010
    type MerkleTagVal EntryTag 'MerkleRootTag = 0x0011

instance IsMerkleLogEntry EntryTag Void where
    type Tag Void = 'VoidTag
    toMerkleNode = \case
    fromMerkleNode _ = throwM
        $ MerkleLogDecodeException "can't deserialize value of type Void"

-- -------------------------------------------------------------------------- --
-- Append-Only Content Addressed Key Value Store (CAS)
-- -------------------------------------------------------------------------- --

-- | The casKey function must be morally injective:
--
-- prop> casKey a /= casKey b || a == b
--
-- Usually, 'casKey' is a cryptographic, i.e. collision resistant, hash
-- function.
--
class Eq (CasKeyType v) => IsCasValue v where
    type CasKeyType v
    casKey :: v -> CasKeyType v

class IsCasValue (CasValueType a) => IsCas a where
    type CasValueType a :: Type
    casLookup
        :: CasKeyType (CasValueType a)
        -> a
        -> Either String (CasValueType a)
    casInsert :: CasValueType a -> a -> a

-- | An 'IsCas' implementation that is base on 'HM.HashMap'.
--
data CAS v = IsCasValue v => CAS !(HM.HashMap (CasKeyType v) v)

deriving instance (Show (CasKeyType v), Show v) => Show (CAS v)

instance (Show (CasKeyType v), Hashable (CasKeyType v), IsCasValue v) => IsCas (CAS v) where
    type CasValueType (CAS v) = v
    casLookup k (CAS m) = case HM.lookup k m of
        Nothing -> Left $ "Key not found: " <> show k
        Just x -> Right x
    casInsert a (CAS m) = CAS $ HM.insert (casKey a) a m

instance (Hashable (CasKeyType v), Eq (CasKeyType v), IsCasValue v) => Semigroup (CAS v) where
    CAS a <> CAS b = CAS (a <> b)

instance (Hashable (CasKeyType v), Eq (CasKeyType v), IsCasValue v) => Monoid (CAS v) where
    mempty = CAS mempty

-- TODO: add deriving via support for 'IsMerkleLog' instances, which would
-- guarantee that the 'IsCas', 'IsCasValue' and 'IsMerleLog' instances are
-- consistent.

-- -------------------------------------------------------------------------- --
-- Block Chain Data
-- -------------------------------------------------------------------------- --

-- -------------------------------------------------------------------------- --
-- Hashes

class IsMerkleRoot a b | b -> a where
    _toMerkleRoot :: b -> MerkleRoot a
    _fromMerkleRoot :: MerkleRoot a -> b

instance IsMerkleRoot a (MerkleRoot a) where
    _toMerkleRoot = id
    _fromMerkleRoot = id
    {-# INLINE _toMerkleRoot #-}
    {-# INLINE _fromMerkleRoot #-}

newtype BlockHeaderHash = BlockHeaderHash (MerkleRoot SHA512t_256)
    deriving (Show, Eq, Generic)
    deriving newtype (BA.ByteArrayAccess, Hashable, IsMerkleRoot SHA512t_256)
    deriving (IsMerkleLogEntry EntryTag) via (MerkleRootLogEntry 'BlockHeaderHashTag)

newtype BlockPayloadHash = BlockPayloadHash (MerkleRoot SHA512t_256)
    deriving (Show, Eq, Generic)
    deriving newtype (BA.ByteArrayAccess, Hashable, IsMerkleRoot SHA512t_256)
    deriving (IsMerkleLogEntry EntryTag) via (MerkleRootLogEntry 'BlockPayloadHashTag)

newtype BlockTransactionsHash = BlockTransactionsHash (MerkleRoot SHA512t_256)
    deriving (Show, Eq, Generic)
    deriving newtype (BA.ByteArrayAccess, Hashable, IsMerkleRoot SHA512t_256)
    deriving (IsMerkleLogEntry EntryTag) via (MerkleRootLogEntry 'BlockTransactionsHashTag)

newtype BlockOutputsHash = BlockOutputsHash (MerkleRoot SHA512t_256)
    deriving (Show, Eq, Generic)
    deriving newtype (BA.ByteArrayAccess, Hashable, IsMerkleRoot SHA512t_256)
    deriving (IsMerkleLogEntry EntryTag) via (MerkleRootLogEntry 'BlockOutputsHashTag)

-- -------------------------------------------------------------------------- --
-- HashTag

newtype HashTag = HashTag B.ByteString
    deriving (Show)
    deriving newtype (BA.ByteArrayAccess)
    deriving (IsMerkleLogEntry EntryTag) via (ByteArrayMerkleLogEntry EntryTag 'HashTagTag B.ByteString)

payloadHashTag :: HashTag
payloadHashTag = HashTag "PAYLOAD_HASH"

blockHeaderHashTag :: HashTag
blockHeaderHashTag = HashTag "BLOCK_HEADER_HASH"

-- -------------------------------------------------------------------------- --
-- Transaction

-- | A transaction, including all of its inputs.
--
newtype Transaction = Transaction { _transactionBytes :: B.ByteString }
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (BA.ByteArrayAccess)

instance IsMerkleLogEntry EntryTag Transaction where
    type Tag Transaction = 'TransactionTag
    toMerkleNode = InputNode . _transactionBytes
    fromMerkleNode (InputNode bytes) = Right $ Transaction bytes
    fromMerkleNode (TreeNode _) = throwM expectedInputNodeException

-- | Output of a sinle transaction.
--
newtype TransactionOutput = TransactionOutput
    { _transactionOutputBytes :: B.ByteString }
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (BA.ByteArrayAccess)

instance IsMerkleLogEntry EntryTag TransactionOutput where
    type Tag TransactionOutput = 'TransactionOutputTag
    toMerkleNode = InputNode . _transactionOutputBytes
    fromMerkleNode (InputNode bytes) = Right $ TransactionOutput bytes
    fromMerkleNode (TreeNode _) = throwM expectedInputNodeException

newtype BlockHeight = BlockHeight Word64
    deriving (Show, Eq, Ord, Enum, Generic)
    deriving (IsMerkleLogEntry EntryTag) via (Word64BeMerkleLogEntry 'BlockHeightTag)

newtype ChainId = ChainId Word32
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (Hashable)
    deriving (IsMerkleLogEntry EntryTag) via (Word32BeMerkleLogEntry 'ChainIdTag)

-- -------------------------------------------------------------------------- --
-- Authoritative Key Value Stores
-- -------------------------------------------------------------------------- --

-- -------------------------------------------------------------------------- --
-- Block Header

-- | 'BlockHeader's link the blocks of transactions together to form a chain.
--
-- NOTES:
--
-- SPV proofs over this structure are of this \(O(n)\) in the depth of the
-- target block in the chain. If that isn't acceptable we could add (a hash) of
-- a Merkle index into the block headers, block payloads, or even transactions.
-- Currently, efficient Merkle indexes are only available on the block payload
-- level. Many SPV proofs are short, even with an additional index, one would
-- choose in a case-by-case basis whether to use the proof linear in the depth
-- or a proof logarithmic in the block height of the chain.
--
data BlockHeader = BlockHeader
    { _blockHeaderHash :: !BlockHeaderHash
        -- ^ Hash of '_blockHeaderParent' and '_blockHeaderPayloadHash'. Primary
        -- key of 'BlockHeaderStore'.

    , _blockHeaderParent :: !BlockHeaderHash
        -- ^ Foreign key into 'BlockHeaderStore'.
        --
        -- TODO: add newtype wrapper: BlockHeaderParentHash?

    , _blockHeaderPayloadHash :: !BlockPayloadHash
        -- ^ Foreign key into 'BlockPayloadHashStore'.

    , _blockHeaderHeight :: !BlockHeight
        -- ^ Zero based index of the block in the chain from the genesis block.

    -- , _blockHeaderMerkleRoot :: !BlockHeaderMerkleRoot
    --     -- ^ The index in the Merkle Log of the Block Headers

    , _blockHeaderChainId :: !ChainId

    , _blockHeaderAdjacentParents :: !(S.Seq BlockHeaderHash)

    }
    deriving (Show)

instance IsCasValue BlockHeader where
    type CasKeyType BlockHeader = BlockHeaderHash
    casKey = _blockHeaderHash

instance HasMerkleLog EntryTag BlockHeader where
    type MerkleLogHeader BlockHeader = '[HashTag, BlockHeaderHash, BlockPayloadHash, BlockHeight, ChainId]
    type MerkleLogBody BlockHeader = BlockHeaderHash

    toLog bh = merkleLog root entries
      where
        BlockHeaderHash root = _blockHeaderHash bh
        entries = blockHeaderHashTag
            :+: _blockHeaderParent bh
            :+: _blockHeaderPayloadHash bh
            :+: _blockHeaderHeight bh
            :+: _blockHeaderChainId bh
            :+: MerkleLogBody (_blockHeaderAdjacentParents bh)

    fromLog l = BlockHeader
        { _blockHeaderHash = BlockHeaderHash $ _merkleLogRoot l
        , _blockHeaderParent = parent
        , _blockHeaderPayloadHash = payload
        , _blockHeaderHeight = height
        , _blockHeaderChainId = cid
        , _blockHeaderAdjacentParents = adjs
        }
      where
        (_tag :+: parent :+: payload :+: height :+: cid :+: MerkleLogBody adjs) = _merkleLogEntries l

-- | Store of all 'BlockHeader's
--
-- Primary Key: '_blockHash'
--
newtype BlockHeaderStore = BlockHeaderStore (CAS BlockHeader)
    deriving newtype (Show, IsCas, Semigroup, Monoid)

-- -------------------------------------------------------------------------- --
-- Block Payloads

-- | The Payload of a block.
--
-- The transactions of a block at a given height in the chain are discovered by
-- @_blockPayloadTransactionsHash . _blockHeaderPayloadHash@.
--
-- NOTES:
--
-- The block and it's output is uniquely determined by the transactions.
-- However, we want to include the output hash in the payload hash as a checksum
-- and for inclusion proofs. In order to validate the payload hash without
-- recomputing the outputs we need this extra level of indirection between the
-- BlockHeaders and the Transactions.
--
-- This structure could be recomputed from the block transactions only if it
-- would be possible to discover the order of blocks from the block transactions
-- structure which isn't the case. Instead the order of the blocks is discovered
-- from the 'BlockHeader' chain and block transactions for a given block height
-- are looked up by @_blockPayloadTransactionsHash . _blockPayloadHash@.
--
data BlockPayload = BlockPayload
    { _blockPayloadHash :: !BlockPayloadHash
        -- ^ Hash of '_blockPayloadTransactionsHash' and '_blockPayloadTransactionsHash'.
        -- Primary key of 'BlockPayloadStore'.

    , _blockPayloadTransactionsHash :: !BlockTransactionsHash
        -- ^ Root of 'TransactionTree' of the block. Foreign key into
        -- 'BlockTransactionsStore' and 'TransactionTreeStore'.

    , _blockPayloadOutputsHash :: !BlockOutputsHash
        -- ^ Root of 'OutputsTree' of the block. Foreign key into
        -- 'BlockOutputsStore' and 'OutputTreeStore'.
    }
    deriving (Show)

instance IsCasValue BlockPayload where
    type CasKeyType BlockPayload = BlockPayloadHash
    casKey = _blockPayloadHash

instance HasMerkleLog EntryTag BlockPayload where
    type MerkleLogHeader BlockPayload = '[HashTag, BlockTransactionsHash, BlockOutputsHash]
    type MerkleLogBody BlockPayload = Void

    toLog a = merkleLog root entries
      where
        BlockPayloadHash root = _blockPayloadHash a
        entries = payloadHashTag
            :+: _blockPayloadTransactionsHash a
            :+: _blockPayloadOutputsHash a
            :+: emptyBody

    fromLog l = BlockPayload
        { _blockPayloadHash = BlockPayloadHash $ _merkleLogRoot l
        , _blockPayloadTransactionsHash = txHash
        , _blockPayloadOutputsHash = outHash
        }
      where
        (_tag :+: txHash :+: outHash :+: _) = _merkleLogEntries l

-- | Store of the 'BlockPayloads' for all blocks
--
-- Primary Key: '_blockPayloadHash'
--
newtype BlockPayloadStore = BlockPayloadStore (CAS BlockPayload)
    deriving newtype (Show, IsCas, Semigroup, Monoid)

-- -------------------------------------------------------------------------- --
-- Block Transactions

-- | The block transactions
--
data BlockTransactions = BlockTransactions
    { _blockTransactionsHash :: !BlockTransactionsHash
        -- ^ Root of 'TransactionTree' of the block. Primary key of
        -- 'BlockTransactionsStore'. Foreign key into 'TransactionTreeStore'.

    , _blockTransactions :: !(S.Seq Transaction)
        -- ^ Ordered list of all transactions of the block.
    }
    deriving (Show)

instance IsCasValue BlockTransactions where
    type CasKeyType BlockTransactions = BlockTransactionsHash
    casKey = _blockTransactionsHash

instance HasMerkleLog EntryTag BlockTransactions where
    type MerkleLogHeader BlockTransactions = '[]
    type MerkleLogBody BlockTransactions = Transaction

    toLog a = merkleLog root entries
      where
        BlockTransactionsHash root = _blockTransactionsHash a
        entries = MerkleLogBody (_blockTransactions a)

    fromLog l = BlockTransactions
        { _blockTransactionsHash = BlockTransactionsHash $ _merkleLogRoot l
        , _blockTransactions = txs
        }
      where
        MerkleLogBody txs = _merkleLogEntries l

type BlockTransactionsLog = MkLogType EntryTag BlockTransactions

-- | Store of the 'BlockTransactions' for all blocks.
--
-- Primary Key: '_blockTransactionsHash'
--
newtype BlockTransactionsStore = BlockTransactionsStore (CAS BlockTransactions)
    deriving newtype (Show, IsCas, Semigroup, Monoid)

-- -------------------------------------------------------------------------- --
-- Redundant / Caches
-- -------------------------------------------------------------------------- --

-- -------------------------------------------------------------------------- --
-- Block Outputs

-- | All outputs of the transactions of a block.
--
-- NOTE: the block outputs are associated with the respective block in the
-- 'BlockPayload' structure for the block.
--
data BlockOutputs = BlockOutputs
    { _blockOutputsHash :: !BlockOutputsHash
        -- ^ Root of 'OutputTree' of the block. Primary key of
        -- 'BlockOutputsStore'. Foreign key into 'OutputTreeStore'.

    , _blockOutputs :: !(S.Seq TransactionOutput)
        -- ^ Output of all transactions of a block in the order of the
        -- transactions in the block.
    }
    deriving (Show)

instance IsCasValue BlockOutputs where
    type CasKeyType BlockOutputs = BlockOutputsHash
    casKey = _blockOutputsHash

instance HasMerkleLog EntryTag BlockOutputs where
    type MerkleLogHeader BlockOutputs = '[]
    type MerkleLogBody BlockOutputs = TransactionOutput

    toLog a = merkleLog root entries
      where
        BlockOutputsHash root = _blockOutputsHash a
        entries = MerkleLogBody (_blockOutputs a)

    fromLog l = BlockOutputs
        { _blockOutputsHash = BlockOutputsHash $ _merkleLogRoot l
        , _blockOutputs = outs
        }
      where
        MerkleLogBody outs = _merkleLogEntries l

type BlockOutputsLog = MkLogType EntryTag BlockOutputs

-- | Store of the 'BlockOutputs' for all blocks.
--
newtype BlockOutputsStore = BlockOutputsStore (CAS BlockOutputs)
    deriving newtype (Show, IsCas, Semigroup, Monoid)

-- -------------------------------------------------------------------------- --
-- Transaction Merkle Tree

-- | Merkle tree for the transactions in a block.
--
data TransactionTree = TransactionTree
    { _transactionTreeHash :: !BlockTransactionsHash
        -- ^ Root of '_transactionTree'. Primary key of 'TransactionTreeStore.
        -- Foreign key into 'BlockTransactionsStore'.

    , _transactionTree :: !(MerkleTree SHA512t_256)
    }
    deriving (Show)

instance IsCasValue TransactionTree where
    type CasKeyType TransactionTree = BlockTransactionsHash
    casKey = _transactionTreeHash

-- | Store of the 'TransactionTree' Merkle trees for all blocks.
--
newtype TransactionTreeStore = TransactionTreeStore (CAS TransactionTree)
    deriving newtype (Show, IsCas, Semigroup, Monoid)

-- -------------------------------------------------------------------------- --
-- Output Merkle Tree

-- | Merkle Tree for transaction outputs of a block.
--
data OutputTree = OutputTree
    { _outputTreeHash :: !BlockOutputsHash
        -- ^ Root of '_outputTree'. Primary key of 'OutputTreeStore. Foreign key
        -- into 'BlockOutputsStore'.

    , _outputTree :: !(MerkleTree SHA512t_256)
    }
    deriving (Show)

instance IsCasValue OutputTree where
    type CasKeyType OutputTree = BlockOutputsHash
    casKey = _outputTreeHash

-- | Store of the 'OutputTree' Merkle trees for all blocks.
--
newtype OutputTreeStore = OutputTreeStore (CAS OutputTree)
    deriving newtype (Show, IsCas, Semigroup, Monoid)

-- -------------------------------------------------------------------------- --
-- DataBase
-- -------------------------------------------------------------------------- --

-- | The authoritative CAS stores for a block chain.
--
data ChainDb = ChainDb
    { _chainDbBlockHeaders :: !BlockHeaderStore
    , _chainDbBlockPayloadLoads :: !BlockPayloadStore
    , _chainDbBlockTransactions :: !BlockTransactionsStore
    }
    deriving (Show)

makeLenses ''ChainDb

emptyChainDb :: ChainDb
emptyChainDb = ChainDb mempty mempty mempty

-- | The CAS caches for a block chain.
--
data ChainCache = ChainCache
    { _chainCacheBlockOutputs :: !BlockOutputsStore
        -- ^ This is relatively expensive to rebuild.

    , _chainCacheTransactionTrees :: !TransactionTreeStore
        -- ^ This are relatively cheap to rebuild.
        -- (tens of thousands of blocks per second)

    , _chainCacheOutputTrees :: !OutputTreeStore
        -- ^ This are relatively cheap to rebuild.
        -- (tens of thousands blocks per second)
    }
    deriving (Show)

makeLenses ''ChainCache

emptyCache :: ChainCache
emptyCache = ChainCache mempty mempty mempty

data ChainData = ChainData
    { _chainHead :: !(M.Map ChainId BlockHeader)
    , _chainDb :: !ChainDb
    , _chainCache :: !ChainCache
    }
    deriving (Show)

makeLenses ''ChainData

emptyChainData :: ChainData
emptyChainData = ChainData
    { _chainDb = emptyChainDb
    , _chainCache = emptyCache
    , _chainHead = mempty
    }

-- -------------------------------------------------------------------------- --
-- Data Creation
-- -------------------------------------------------------------------------- --

-- | This forces the 'MerkleTree' which can be an expensive operation.
--
newTransactionLog :: S.Seq Transaction -> BlockTransactionsLog
newTransactionLog = newMerkleLog . MerkleLogBody

-- | This forces the 'MerkleTree' which can be an expensive operation.
--
newBlockTransactions :: S.Seq Transaction -> (TransactionTree, BlockTransactions)
newBlockTransactions txs = (tree, blockTxs)
  where
    mlog = newTransactionLog txs
    blockTxs = fromLog mlog
    tree = TransactionTree
        { _transactionTreeHash = _blockTransactionsHash blockTxs
        , _transactionTree = _merkleLogTree mlog
        }

-- | Create a MerkleLog from a 'BlockTransactions' and a cached
-- 'TransactionTree'.
--
-- It is an error if the 'TransactionTree' doesn't match the
-- 'BlockTransactions'.
--
-- If you only have the 'BlockTransactions' you can use 'toLog' in which case
-- the '_merkleLogTree' will be instantiated lazily.
--
transactionLog
    :: BlockTransactions
    -> TransactionTree
    -> BlockTransactionsLog
transactionLog txs tree
    | _blockTransactionsHash txs == _transactionTreeHash tree
        = (toLog txs) { _merkleLogTree = _transactionTree tree }
    | otherwise = error "Transaction tree and block transactions don't match"

-- | This forces the 'MerkleTree' which can be an expensive operation.
--
newBlockOutputLog :: S.Seq TransactionOutput -> BlockOutputsLog
newBlockOutputLog = newMerkleLog . MerkleLogBody

-- | This forces the 'MerkleTree' which can be an expensive operation.
--
newBlockOutputs :: S.Seq TransactionOutput -> (OutputTree, BlockOutputs)
newBlockOutputs outs = (tree, blkOuts)
  where
    mlog = newBlockOutputLog outs
    blkOuts = fromLog mlog
    tree = OutputTree
        { _outputTreeHash = _blockOutputsHash blkOuts
        , _outputTree = _merkleLogTree mlog
        }

-- | Create a MerkleLog from 'BlockOutputs' and a cached 'OutputTree'.
--
-- It is an error if the 'OutputTree' doesn't match the 'BlockOutputs'.
--
-- If you only have the 'BlockOutputs' you can use 'toLog' in which case the
-- '_merkleLogTree' will be instantiated lazily.
--
blockOutputLog
    :: BlockOutputs
    -> OutputTree
    -> BlockOutputsLog
blockOutputLog outs tree
    | _blockOutputsHash outs == _outputTreeHash tree
        = (toLog outs) { _merkleLogTree = _outputTree tree }
    | otherwise = error "Output tree and block outputs don't match"

-- |
--
-- Note, that forcing the 'MerkleTree' is cheap for 'BlockPayload'.
--
blockPayload :: BlockTransactions -> BlockOutputs -> BlockPayload
blockPayload txs outs
    = fromLog $ newMerkleLog @EntryTag
        $ payloadHashTag
        :+: _blockTransactionsHash txs
        :+: _blockOutputsHash outs
        :+: emptyBody

-- |
--
-- Note, that forcing the 'MerkleTree' is cheap for 'BlockHeader'.
--
blockHeader :: BlockHeader -> S.Seq BlockHeaderHash -> BlockPayload -> BlockHeader
blockHeader parent adjacentParents payload =
    fromLog $ newMerkleLog @EntryTag $ blockHeaderHashTag
        :+: _blockHeaderHash parent
        :+: _blockPayloadHash payload
        :+: succ (_blockHeaderHeight parent)
        :+: _blockHeaderChainId parent
        :+: MerkleLogBody adjacentParents

genesisBlockHeader :: ChainId -> BlockPayload -> BlockHeader
genesisBlockHeader cid payload =
    fromLog $ newMerkleLog @EntryTag $ blockHeaderHashTag
        :+: BlockHeaderHash (merkleRoot emptyMerkleTree)
        :+: _blockPayloadHash payload
        :+: BlockHeight 0
        :+: cid
        :+: emptyBody

-- -------------------------------------------------------------------------- --
-- Inclusion Proofs
-- -------------------------------------------------------------------------- --

-- -------------------------------------------------------------------------- --
-- ChainProof

newtype ChainProof a = ChainProof (MerkleProof a)
    deriving (Show)

runChainProof :: ChainProof SHA512t_256 -> BlockHeaderHash
runChainProof (ChainProof p) = BlockHeaderHash $ runMerkleProof p

chainUntil
    :: BlockHeaderStore
    -> BlockHeader
    -> BlockHeight
    -> Either String (N.NonEmpty BlockHeader)
chainUntil db h i
    | i > _blockHeaderHeight h = Left "target block height is larger than start"
    | otherwise = go h []
  where
    go cur acc
        | i == _blockHeaderHeight cur = return (cur N.:| acc)
        | otherwise = do
            p <- casLookup (_blockHeaderParent cur) db
            go p (cur : acc)

-- | The includes the following trees:
--
-- 1. TransactionTree
-- 2. BlockPayload
-- 3. BlockHeader
-- 4. BlockHeaderParent Chain
-- 5. BlockHeaderAdjacentParent Chain
--
-- TODO: verify inputs?
--
createChainProof
    :: HasCallStack
    => Graph
    -> ChainData
        -- ^ Chain Data
    -> ChainId
        -- ^ target chain
    -> ChainId
        -- ^ source chain
    -> BlockHeight
        -- ^ The block height of the transaction
    -> Int
        -- ^ The index of the transaction in the block
    -> Either String (ChainProof SHA512t_256)
createChainProof graph dat tcid scid txHeight txIx = do

    -- For large bodies one should work with the MerkleLog object, for empty or
    -- small bodies one can work with the type directly
    --
    -- Rule of thumb:
    --
    -- If a specialized function for creating a MerkleLog is provided by the API
    -- it should be used.
    --

    let (_:p) = shortestPath graph 3 {- diameter of Peterson Graph -} tcid scid
    {- assert: last p == trgHeadHeader -}

    -- Follow adjacent parent hashes along path p
    --
    -- FIXME: add some santity checks (in particular for height)
    --
    let goToChain x [] acc = return (x, acc)
        goToChain x (h:t) acc = do
            let adjs = toAdjMap graph (_blockHeaderChainId x) $ _blockHeaderAdjacentParents x

            -- FIXME: this fails with 'error' in case that the chain is too short.
            adjpHdr <- casLookup (adjs M.! h) (_chainDbBlockHeaders db)

            let adjIdx = M.findIndex h adjs
            unless (_blockHeaderHeight adjpHdr >= txHeight)
                $ Left "block of given height is not reachable on source chain"
            goToChain adjpHdr t ((adjIdx, x) : acc)

    -- crossChain == ]srcHeadHeader, trgHeadHeader]
    (srcHeadHeader, crossChain) <- goToChain trgHeadHeader p []

    -- chain == [srcHeader, srcHeadHeader]
    (txHeader N.:| chain) <- chainUntil (_chainDbBlockHeaders db) srcHeadHeader txHeight

    payload <- casLookup (_blockHeaderPayloadHash txHeader) (_chainDbBlockPayloadLoads db)
    let txsHash = _blockPayloadTransactionsHash payload

    -- 1. TX proof
    --
    txs <- casLookup txsHash (_chainDbBlockTransactions db)
    -- txTree <- casLookup txsHash (_chainCacheTransactionTrees cache)
    -- let txLog = transactionLog txs txTree

    let (subj, txsPos, txst) = bodyTree txs txIx -- FIXME use log
    let txsTree = (txsPos, txst)
    -- we blindly trust the ix

    -- 2. Payload proof
    --
    let payloadTree = headerTree_ @BlockTransactionsHash payload

    -- 3. BlockHeader proof
    --
    unless (_blockHeaderPayloadHash txHeader == _blockPayloadHash payload)
        $ Left "inconsistent input data in payload store"
            -- this indicates that the payload store is inconsistent
    let blockHeaderTree = headerTree_ @BlockPayloadHash txHeader

    -- 4. BlockHeader Chain Proof
    --
    let go [] = []
        go (h : t) = headerTree_ @BlockHeaderHash h : go t

        chainTrees = go chain

    -- 5. Cross Chain Proof
    --
    let cross [] = []
        cross ((i,h) : t) = bodyTree_ h i : cross t

        crossTrees = cross crossChain

    -- Put proofs together
    --
    proof <- first displayException
        $ merkleProof_ subj $ (N.:|) txsTree $
            [ payloadTree
            , blockHeaderTree
            ]
            <> chainTrees
            <> crossTrees

    return $ ChainProof proof
  where
    db = _chainDb dat
    -- cache = _chainCache dat
    trgHeadHeader = _chainHead dat M.! tcid

toAdjMap
    :: Graph
    -> ChainId
    -> S.Seq BlockHeaderHash
    -> M.Map ChainId BlockHeaderHash
toAdjMap graph cid adj = M.fromList
    $ zip (Set.toAscList $ graph M.! cid) (toList adj)

fromAdjMap :: M.Map ChainId BlockHeaderHash -> S.Seq BlockHeaderHash
fromAdjMap m = S.fromList $ fmap snd $ M.toAscList m

-- -------------------------------------------------------------------------- --
-- Generate Chain for Testing

instance Arbitrary Transaction where
    arbitrary = Transaction . B.pack <$> arbitrary

instance Arbitrary TransactionOutput where
    arbitrary = TransactionOutput . B.pack <$> arbitrary

instance {-# OVERLAPPING #-} Arbitrary (TransactionTree, BlockTransactions) where
    arbitrary = newBlockTransactions <$> arbitrary

instance {-# OVERLAPPING #-} Arbitrary (OutputTree, BlockOutputs) where
    arbitrary = newBlockOutputs <$> arbitrary

createNewBlock
    :: Graph
    -> ChainData
    -> BlockHeader
    -> BlockTransactions
    -> TransactionTree
    -> BlockOutputs
    -> OutputTree
    -> ChainData
createNewBlock graph dat parentHdr txs txTree outs outTree = dat
    & over (chainDb . chainDbBlockHeaders) (casInsert hdr)
    & over (chainDb . chainDbBlockPayloadLoads) (casInsert payload)
    & over (chainDb . chainDbBlockTransactions) (casInsert txs)
    & over (chainCache . chainCacheBlockOutputs) (casInsert outs)
    & over (chainCache . chainCacheTransactionTrees) (casInsert txTree)
    & over (chainCache . chainCacheOutputTrees) (casInsert outTree)
    & set (chainHead . at (_blockHeaderChainId hdr)) (Just hdr)
  where
    payload = blockPayload txs outs
    hdr = blockHeader parentHdr adjParents payload
    cid = _blockHeaderChainId parentHdr
    adjParents = fromAdjMap $ _blockHeaderHash <$> project (_chainHead dat) (graph M.! cid)

project :: Ord a => M.Map a b -> Set.Set a -> M.Map a b
project m s = m `M.intersection` M.fromSet (const ()) s

genNewBlock :: Graph -> ChainData -> ChainId -> Gen ChainData
genNewBlock graph dat cid = do
    (txTree, txs) <- arbitrary
    (outTree, outs) <- arbitrary
    return $ createNewBlock graph dat (_chainHead dat M.! cid) txs txTree outs outTree

genGenesisBlock :: ChainData -> ChainId -> Gen ChainData
genGenesisBlock dat cid = do
    (txTree, txs) <- arbitrary
    (outTree, outs) <- arbitrary
    let payload = blockPayload txs outs
        hdr = genesisBlockHeader cid payload

    return $! dat
        & over (chainDb . chainDbBlockHeaders) (casInsert hdr)
        & over (chainDb . chainDbBlockPayloadLoads) (casInsert payload)
        & over (chainDb . chainDbBlockTransactions) (casInsert txs)
        & over (chainCache . chainCacheBlockOutputs) (casInsert outs)
        & over (chainCache . chainCacheTransactionTrees) (casInsert txTree)
        & over (chainCache . chainCacheOutputTrees) (casInsert outTree)
        & set (chainHead . at (_blockHeaderChainId hdr)) (Just hdr)

genGenesisBlocks :: Graph -> Gen ChainData
genGenesisBlocks = foldM genGenesisBlock emptyChainData . M.keys

genChain :: Graph -> Int -> Gen ChainData
genChain graph n = do
    chain <- genGenesisBlocks graph
    foldM (genNewBlock graph) chain
        [ ChainId j | _ <- [(0::Int) .. n-1], j <- [0 .. s-1] ]
  where
    s = fromIntegral $ M.size graph

-- -------------------------------------------------------------------------- --
-- Test main

-- | Example
--
-- 1. Generate a random chain of length 10
-- 2. Create proof for first transaction at block height 7
-- 3. run proof
-- 4. validate proof of result matches head of chain.
--
main :: HasCallStack => IO ()
main = do
    c <- generate $ genChain graph 30

    print "SPV:"
    testProof graph c (ChainId 0) (ChainId 0) (BlockHeight 7) 1

    print "Cross Chain Transfer:"
    testProof graph c (ChainId 0) (ChainId 3) (BlockHeight 7) 1
  where
    graph = peterson

testProof
    :: Graph
    -> ChainData
    -> ChainId
    -> ChainId
    -> BlockHeight
    -> Int
    -> IO ()
testProof graph chain tcid scid sheight txIdx = do
    ChainProof proof <- either error return $ do
        p <- createChainProof graph chain tcid scid sheight txIdx
        let r = runChainProof p
        unless (r == _blockHeaderHash (_chainHead chain M.! tcid))
            $ Left "proof result doesn't match chain head"
        return p
    putStrLn $ "proof size in bytes: " <> show (BA.length $ _merkleProofObject proof)

-- -------------------------------------------------------------------------- --
-- Quick and dirty Graph tools

type Graph = M.Map ChainId (Set.Set ChainId)

peterson :: Graph
peterson = fmap (Set.fromList)
    $ M.fromList
    $ fmap (bimap ChainId (fmap ChainId))

        -- outer vertices
        [ (0, [4,1,5])
        , (1, [0,2,6])
        , (2, [1,3,7])
        , (3, [2,4,8])
        , (4, [3,0,9])

        -- inner vertices
        , (5, [9,6,0])
        , (6, [5,7,1])
        , (7, [6,8,2])
        , (8, [7,9,3])
        , (9, [8,5,4])
        ]

-- | Shortest path in a strongly connected graph of a diameter of at most
-- @d@.
--
shortestPath :: Graph -> Natural -> ChainId -> ChainId -> [ChainId]
shortestPath graph d src trg =
    minimumBy (compare `on` length) $ dfs graph d src trg

-- | Quick and dirty depth first pruned by upper bound on search depth.
--
dfs :: Graph -> Natural -> ChainId -> ChainId -> [[ChainId]]
dfs g limit src trg = go 1 (HS.singleton src) src
  where
    go _ _ n | n == trg = return [trg]
    go depth visited n = (:) n <$> do
        guard (depth < limit)
        v <- toList $ g M.! n
        guard (not $ HS.member v visited)
        go (succ depth) (HS.insert v visited) v
