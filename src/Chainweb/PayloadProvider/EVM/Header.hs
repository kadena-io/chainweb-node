{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.PayloadProvider.EVM.Header
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Execution Layer Header For Cacun Hardfork
--
-- For the specification up to the Shanghai Hardfork see:
--
-- [Yellow Paper (SHANGHAI VERSION f3553dd – 2024-11-04), 4.4 The Block](https://ethereum.github.io/yellowpaper/paper.pdf)
--
-- For the Cacun specificic addition see:
--
-- [python execution-specs](https://github.com/ethereum/execution-specs/blob/master/src/ethereum/cancun/blocks.py)
--
--
module Chainweb.PayloadProvider.EVM.Header
( Header(..)
, blockHash
, ommersHash
, difficulty
, nonce
, BaseFeePerGas(..)
, WithdrawalsRoot(..)
) where

import Chainweb.PayloadProvider.EVM.Utils

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Hashable (Hashable)

import Ethereum.Misc
import Ethereum.RLP
import Ethereum.Utils

import Foreign.Storable

import Numeric.Natural
import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleUniverse

-- -------------------------------------------------------------------------- --
-- Header Fields for Paris Hardfork
--
-- The mixHash field was repurposed in the Paris hardfork to store the previous
-- RANDAO mix.

-- | Base Fee Per Gas of a Block
--
-- A natural number [cf. yellow paper 4.4.3 (44)]
--
newtype BaseFeePerGas = BaseFeePerGas Natural
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving ToJSON via (HexQuantity Natural)
    deriving FromJSON via (HexQuantity Natural)

-- -------------------------------------------------------------------------- --
-- Header Fields for Shanghai Hardfork

-- | Withdrawals Root
--
-- 32 bytes [cf. yellow paper 4.4.3 (44)]
--
newtype WithdrawalsRoot = WithdrawalsRoot (BytesN 32)
    deriving (Show, Eq)
    deriving newtype (RLP, Bytes, Storable, Hashable)
    deriving ToJSON via (HexBytes (BytesN 32))
    deriving FromJSON via (HexBytes (BytesN 32))

-- -------------------------------------------------------------------------- --
-- Header Fields for Cacun Hardfork

-- | Parent Beacon Block Root
--
-- Since Cacun Hardfork
--
newtype ParentBeaconBlockRoot = ParentBeaconBlockRoot (BytesN 32)
    deriving (Show, Eq)
    deriving newtype (RLP, Bytes, Storable, Hashable)
    deriving ToJSON via (HexBytes (BytesN 32))
    deriving FromJSON via (HexBytes (BytesN 32))

-- | Gas Used for Blob
--
-- Since Cacun Hardfork
--
newtype BlobGasUsed = BlobGasUsed Natural
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving ToJSON via (HexQuantity Natural)
    deriving FromJSON via (HexQuantity Natural)

-- | Excess Blob Gas
--
-- Since Cacun Hardfork
--
newtype ExcessBlobGas = ExcessBlobGas Natural
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving ToJSON via (HexQuantity Natural)
    deriving FromJSON via (HexQuantity Natural)

-- -------------------------------------------------------------------------- --
-- Header

-- | Execution Layer Header
--
-- For the specification up to the Shanghai Hardfork see:
--
-- [Yellow Paper (SHANGHAI VERSION f3553dd – 2024-11-04), 4.4 The Block](https://ethereum.github.io/yellowpaper/paper.pdf)
--
-- For the Cacun specificic addition see:
--
-- [python execution-specs](https://github.com/ethereum/execution-specs/blob/master/src/ethereum/cancun/blocks.py)
--
--
data Header = Header
    { _hdrParentHash :: !ParentHash
        -- ^ The Keccak 256-bit hash of the parent block’s header, in its
        -- entirety; formally \(H_p\).
    , _hdrOmmersHash :: !OmmersHash
        -- ^ A 256-bit hash field that is now deprecated due to the replacement
        -- of proof of work consensus. It is now to a constant, KEC(RLP(()));
        -- formally \(H_o\).
    , _hdrBeneficiary :: !Beneficiary
        -- ^ The 160-bit address to which priority fees from this block are
        -- transferred; formally \(H_c\).
    , _hdrStateRoot :: !StateRoot
        -- ^ The Keccak 256-bit hash of the root node of the state trie, after
        -- all transactions and withdrawals are executed and finalisations
        -- applied; formally \(H_r\).
    , _hdrTransactionsRoot :: !TransactionsRoot
        -- ^ The Keccak 256-bit hash of the root node of the trie structure
        -- populated with each transaction in the transactions list portion of
        -- the block; formally \(H_t\).
    , _hdrReceiptsRoot :: !ReceiptsRoot
        -- ^ The Keccak 256-bit hash of the root node of the trie structure
        -- populated with the receipts of each transaction in the transactions
        -- list portion of the block; formally \(H_e\).
    , _hdrLogsBloom :: !Bloom
        -- ^ The Bloom filter composed from indexable information (logger
        -- address and log topics) contained in each log entry from the receipt
        -- of each transaction in the transactions list; formally \(H_b\).
    , _hdrDifficulty :: !Difficulty
        -- ^ A scalar field that is now deprecated due to the replacement of
        -- proof of work consensus. It is set to 0; formally \(H_d\).
    , _hdrNumber :: !BlockNumber
        -- ^ A scalar value equal to the number of ancestor blocks. The genesis
        -- block has a number of zero; formally \(H_i\).
    , _hdrGasLimit :: !GasLimit
        -- ^ A scalar value equal to the current limit of gas expenditure per
        -- block; formally \(H_l\).
    , _hdrGasUsed :: !GasUsed
        -- ^ A scalar value equal to the total gas used in transactions in this
        -- block; formally \(H_g\).
    , _hdrTimestamp :: !Timestamp
        -- ^ A scalar value equal to the reasonable output of Unix’s time() at
        -- this block’s inception; formally \(H_s\).
    , _hdrExtraData :: !ExtraData
        -- ^ An arbitrary byte array containing data relevant to this block.
        -- This must be 32 bytes or fewer; formally \(H_x\).
    , _hdrPrevRandao :: !Randao
        -- ^ the latest RANDAO mix7 of the post beacon state of the previous
        -- block; formally \(H_a\).
    , _hdrNonce :: !Nonce
        -- ^ A 64-bit value that is now deprecated due to the replacement of
        -- proof of work consensus. It is set to 0x0000000000000000; formally
        -- \(H_n\).
        --
        -- In parts of the API the nonce is interpreded as a Word64 scalar. In
        -- RLP serialization it is considered as bytes. Hence, we store it as
        -- bytes. Note, however, that it is used in reversed byte order in the
        -- Ethhash computation.
    , _hdrBaseFeePerGas :: !BaseFeePerGas
        -- ^ A scalar value equal to the amount of wei that is burned for each
        -- unit of gas consumed; formally \(H_f\).
    , _hdrWithdrawalsRoot :: !WithdrawalsRoot
        -- ^ The Keccak 256-bit hash of the root node of the trie structure
        -- populated with each withdrawal operations pushed by the consensus
        -- layer for this block; formally \(H_w\).

    -- The following fields are from the Cacun hard fork which is not yet
    -- described in the Yellow Paper.
    --
    -- cf. https://github.com/ethereum/execution-specs/blob/master/src/ethereum/cancun/blocks.py

    , _hdrBlobGasUsed :: !BlobGasUsed
    , _hdrExcessBlobGas :: !ExcessBlobGas
    , _hdrParentBeaconBlockRoot :: !ParentBeaconBlockRoot
    }
    deriving (Show, Eq)

-- -------------------------------------------------------------------------- --
-- Block Hash Computation

blockHash :: Header -> BlockHash
blockHash = BlockHash . keccak256 . putRlpByteString
{-# INLINE blockHash #-}

-- -------------------------------------------------------------------------- --
-- Constant Values after Paris Hardfork

-- | Since the Paris Hardfork the nonce is KEC(RLP(()))
--
-- [cf. 4.4.4 (57)](https://ethereum.github.io/yellowpaper/paper.pdf)
--
ommersHash :: OmmersHash
ommersHash = OmmersHash $ keccak256 $ putRlpByteString ()
{-# INLINE ommersHash #-}

-- | Since the Paris Hardfork the difficulty is 0.
--
-- [cf. 4.4.4 (58)](https://ethereum.github.io/yellowpaper/paper.pdf)
--
difficulty :: Difficulty
difficulty = Difficulty 0
{-# INLINE difficulty #-}

-- | Since the Paris Hardfork the nonce is 0x0000000000000000.
--
-- [cf. 4.4.4 (59)](https://ethereum.github.io/yellowpaper/paper.pdf)
--
nonce :: Nonce
nonce = Nonce $ replicateN 8
{-# INLINE nonce #-}

-- -------------------------------------------------------------------------- --
-- JSON Serialization

-- | These are the property names of the respective RPC API properties
--
-- The JSON serialization also includes the block hash.
--
headerProperties :: KeyValue e kv => Header -> [kv]
headerProperties o =
    [ "parentHash" .= _hdrParentHash o
    , "sha3Uncles" .= _hdrOmmersHash o
    , "miner" .= _hdrBeneficiary o
    , "stateRoot" .= _hdrStateRoot o
    , "transactionsRoot" .= _hdrTransactionsRoot o
    , "receiptsRoot" .= _hdrReceiptsRoot o
    , "logsBloom" .= _hdrLogsBloom o
    , "difficulty" .= _hdrDifficulty o
    , "number" .= _hdrNumber o
    , "gasLimit" .= _hdrGasLimit o
    , "gasUsed" .= _hdrGasUsed o
    , "timestamp" .= _hdrTimestamp o
    , "extraData" .= _hdrExtraData o
    , "mixHash" .= _hdrPrevRandao o -- legacy property name for prev randao
    , "nonce" .= _hdrNonce o
    , "baseFeePerGas" .= _hdrBaseFeePerGas o
    , "withdrawalsRoot" .= _hdrWithdrawalsRoot o
    , "blobGasUsed" .= _hdrBlobGasUsed o
    , "excessBlobGas" .= _hdrExcessBlobGas o
    , "parentBeaconBlockRoot" .= _hdrParentBeaconBlockRoot o
    , "hash" .= blockHash o
    ]
{-# INLINE headerProperties #-}
{-# SPECIALIZE headerProperties :: Header -> [Series] #-}
{-# SPECIALIZE headerProperties :: Header -> [Pair] #-}

instance ToJSON Header where
    toEncoding = pairs . mconcat . headerProperties
    toJSON = object . headerProperties
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON Header where
    parseJSON = withObject "ConsensusHeader" $ \o -> Header
        <$> o .: "parentHash"
        <*> o .: "sha3Uncles"
        <*> o .: "miner"
        <*> o .: "stateRoot"
        <*> o .: "transactionsRoot"
        <*> o .: "receiptsRoot"
        <*> o .: "logsBloom"
        <*> o .: "difficulty"
        <*> o .: "number"
        <*> o .: "gasLimit"
        <*> o .: "gasUsed"
        <*> o .: "timestamp"
        <*> o .: "extraData"
        <*> o .: "mixHash" -- legacy property name for prev randao
        <*> o .: "nonce"
        <*> o .: "baseFeePerGas"
        <*> o .: "withdrawalsRoot"
        <*> o .: "blobGasUsed"
        <*> o .: "excessBlobGas"
        <*> o .: "parentBeaconBlockRoot"
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Serialization

-- | RLP Encoding for Execution Layer Header
--
-- For the specification up to the Shanghai Hardfork see:
--
-- [cf. Yellow Paper 4.4.3 (44)](https://ethereum.github.io/yellowpaper/paper.pdf)
--
-- LH(H) ≡ (Hp,Ho,Hc,Hr,Ht,He,Hb,Hd,Hi,Hl,Hg,Hs,Hx,Ha,Hn,Hf,Hw)
--
instance RLP Header where
    putRlp hdr = putRlpL
        [ putRlp $ _hdrParentHash hdr
        , putRlp $ _hdrOmmersHash hdr
        , putRlp $ _hdrBeneficiary hdr
        , putRlp $ _hdrStateRoot hdr
        , putRlp $ _hdrTransactionsRoot hdr
        , putRlp $ _hdrReceiptsRoot hdr
        , putRlp $ _hdrLogsBloom hdr
        , putRlp $ _hdrDifficulty hdr
        , putRlp $ _hdrNumber hdr
        , putRlp $ _hdrGasLimit hdr
        , putRlp $ _hdrGasUsed hdr
        , putRlp $ _hdrTimestamp hdr
        , putRlp $ _hdrExtraData hdr
        , putRlp $ _hdrPrevRandao hdr
        , putRlp $ _hdrNonce hdr
        , putRlp $ _hdrBaseFeePerGas hdr
        , putRlp $ _hdrWithdrawalsRoot hdr

        -- Cancun Hardfork
        , putRlp $ _hdrBlobGasUsed hdr
        , putRlp $ _hdrExcessBlobGas hdr
        , putRlp $ _hdrParentBeaconBlockRoot hdr
        ]

    getRlp = label "Header" $ getRlpL $ Header
        <$> getRlp -- parent hash
        <*> getRlp -- ommers hash
        <*> getRlp -- beneficiary
        <*> getRlp -- state root
        <*> getRlp -- transactions root
        <*> getRlp -- receipts root
        <*> getRlp -- logs bloom
        <*> getRlp -- difficulty
        <*> getRlp -- number
        <*> getRlp -- gas limit
        <*> getRlp -- gas used
        <*> getRlp -- timestamp
        <*> getRlp -- extra data
        <*> getRlp -- prev randao
        <*> getRlp -- nonce
        <*> getRlp -- base fee per gas
        <*> getRlp -- withdrawals root

        -- Cancun Hardfork
        <*> getRlp -- blob gas used
        <*> getRlp -- excess blob gas
        <*> getRlp -- parent beacon block root

    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

-- -------------------------------------------------------------------------- --
-- MerkleLog Entry

-- TODO

-- instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag ParentHash where
--     type Tag ParentHash = 'EthParentHashTag
--     toMerkleNode = InputNode . _blockHash
--     fromMerkleNode (InputNode bs) = Right $ MinerData bs
--     fromMerkleNode (TreeNode _) = throwM expectedInputNodeException
