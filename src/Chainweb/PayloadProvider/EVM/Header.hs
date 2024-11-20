{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}

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
module Chainweb.PayloadProvider.EVM.Header
( Header(..)
, ommersHash
, difficulty
, nonce
, headerProof
, runHeaderProof
, BaseFeePerGas(..)
, BlobGasUsed(..)
, ExcessBlobGas(..)
, WithdrawalsRoot(..)
, ParentBeaconBlockRoot(..)
, chainwebBlockHashToBeaconBlockRoot
, computeBlockHash
, computeBlockPayloadHash

-- * Misc

, type HeaderCas
, heightToNumber
, numberToHeight
, _hdrHeight

-- * Getter
, hdrParentHash
, hdrOmmersHash
, hdrBeneficiary
, hdrStateRoot
, hdrTransactionsRoot
, hdrReceiptsRoot
, hdrLogsBloom
, hdrDifficulty
, hdrNumber
, hdrGasLimit
, hdrGasUsed
, hdrTimestamp
, hdrExtraData
, hdrPrevRandao
, hdrNonce
, hdrBaseFeePerGas
, hdrWithdrawalsRoot
, hdrHeight
, hdrHash
, hdrPayloadHash
) where

import Chainweb.BlockHash qualified as Chainweb
import Chainweb.BlockHeight qualified as Chainweb
import Chainweb.BlockPayloadHash
import Chainweb.Crypto.MerkleLog hiding (headerProof)
import Chainweb.Crypto.MerkleLog qualified as MerkleLog
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.PayloadProvider.EVM.Utils
import Chainweb.Storage.Table
import Chainweb.Utils.Serialization qualified as Chainweb
import Control.Lens (Getter, to)
import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.ByteString.Short qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.Hashable (Hashable(..))
import Data.MerkleLog
import Data.Text qualified as T
import Data.Void
import Ethereum.Misc
import Ethereum.RLP
import Ethereum.Utils
import Foreign.Storable
import GHC.Generics (Generic)
import GHC.TypeNats
import Data.Function

-- --------------------------------------------------------------------------
-- Utils

numberToHeight :: BlockNumber -> Chainweb.BlockHeight
numberToHeight n = fromIntegral n

heightToNumber :: Chainweb.BlockHeight -> BlockNumber
heightToNumber = fromIntegral

-- ------- ------------------------------------------------------------------- --
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

chainwebBlockHashToBeaconBlockRoot :: Chainweb.BlockHash -> ParentBeaconBlockRoot
chainwebBlockHashToBeaconBlockRoot bh = ParentBeaconBlockRoot
    (unsafeBytesN $ SBS.toShort $ Chainweb.runPutS (Chainweb.encodeBlockHash bh))

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

    -- synthetic fields
    , _hdrHash :: {- Lazy -} BlockHash
        -- ^ Ethereum Execution Header Block Hash
    , _hdrPayloadHash :: {- Lazy -} BlockPayloadHash
        -- ^ Chainweb BlockPayloadHash
    }
    deriving (Show, Generic)

instance Hashable Header where
    hashWithSalt s = hashWithSalt s . _hdrPayloadHash
    {-# INLINEABLE hashWithSalt #-}

instance Eq Header where
    (==) = (==) `on` _hdrPayloadHash

instance Ord Header where
    compare = compare `on` (\h -> (_hdrNumber h, _hdrPayloadHash h))

instance IsCasValue Header where
    type CasKeyType Header = BlockPayloadHash
    casKey = _hdrPayloadHash
    {-# INLINE casKey #-}

type HeaderCas tbl = Cas tbl Header

_hdrHeight :: Header -> Chainweb.BlockHeight
_hdrHeight = numberToHeight . _hdrNumber

instance Hashable BlockHash where
    hashWithSalt s h = let BlockHash r = h in hashWithSalt s r
    {-# INLINEABLE hashWithSalt #-}

-- -------------------------------------------------------------------------- --
-- Internal Block Hash Computation

-- | Does not force _hdrHash or _hdrPayloadHash
--
computeBlockHash :: Header -> BlockHash
computeBlockHash = BlockHash . keccak256 . putRlpByteString
{-# INLINE computeBlockHash #-}

-- | Does not force _hdrHash or _hdrPayloadHash
--
computeBlockPayloadHash :: Header -> BlockPayloadHash
computeBlockPayloadHash h = BlockPayloadHash $ MerkleLogHash $ computeMerkleLogRoot h
{-# INLINE computeBlockPayloadHash #-}

-- -------------------------------------------------------------------------- --
-- Merkle Proofs
--
-- Example:
--
-- ghci> Just p = headerProof @BlockNumber hdr
-- ghci> runHeaderProof p == blockPayloadHash hdr
-- True
-- ghci> proofSubject @_ @_ @BlockNumber p
-- BlockNumber 6373

-- | Creates a Merkle proof for a header property of an EVM execution header.
--
headerProof
    :: forall c a m
    . MonadThrow m
    => a ~ ChainwebMerkleHashAlgorithm
    => HasHeader a ChainwebHashTag c (MkLogType a ChainwebHashTag Header)
    => Header
    -> m (MerkleProof a)
headerProof = MerkleLog.headerProof @c
{-# INLINE headerProof #-}

-- | Runs a header proof. Returns the BlockPayloadHash of the EVM execution
-- header for which inclusion is proven.
--
runHeaderProof :: MerkleProof ChainwebMerkleHashAlgorithm -> BlockPayloadHash
runHeaderProof = BlockPayloadHash . MerkleLogHash . runMerkleProof
{-# INLINE runHeaderProof #-}

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
    , "hash" .= _hdrHash o
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
    parseJSON v = do
        hdr <- flip (withObject "ConsensusHeader") v $ \o -> Header
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
            <*> pure (error "Chainweb.PayloadProvider.EVM.Header: _hdrHash")
            <*> pure (error "Chainweb.PayloadProvider.EVM.Header: _hdrPayloadHash")
        return hdr
            { _hdrHash = computeBlockHash hdr
            , _hdrPayloadHash = computeBlockPayloadHash hdr
            }
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

    getRlp = label "Header" $ do
        hdr <- getRlpL $ Header
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
            <*> pure (error "Chainweb.PayloadProvider.EVM.Header: _hdrHash")
            <*> pure (error "Chainweb.PayloadProvider.EVM.Header: _hdrPayloadHash")
        return hdr
            { _hdrHash = computeBlockHash hdr
            , _hdrPayloadHash = computeBlockPayloadHash hdr
            }

    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

-- -------------------------------------------------------------------------- --
-- MerkleLog Entries

newtype RlpMerkleLogEntry (tag :: ChainwebHashTag) t = RlpMerkleLogEntry t
    deriving newtype RLP
instance
    ( KnownNat (MerkleTagVal ChainwebHashTag tag)
    , MerkleHashAlgorithm a
    , RLP t
    )
    => IsMerkleLogEntry a ChainwebHashTag (RlpMerkleLogEntry tag t) where
    type Tag (RlpMerkleLogEntry tag t) = tag
    toMerkleNode = InputNode . putRlpByteString
    fromMerkleNode (InputNode bs) = case get getRlp bs of
        Left e -> throwM $ MerkleLogDecodeException (T.pack e)
        Right x -> Right x
    fromMerkleNode (TreeNode _) = throwM expectedInputNodeException

deriving via (RlpMerkleLogEntry 'EthParentHashTag ParentHash)
    instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag ParentHash

deriving via (RlpMerkleLogEntry 'EthOmmersHashTag OmmersHash)
    instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag OmmersHash

deriving via (RlpMerkleLogEntry 'EthBeneficiaryTag Beneficiary)
    instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag Beneficiary

deriving via (RlpMerkleLogEntry 'EthStateRootTag StateRoot)
    instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag StateRoot

deriving via (RlpMerkleLogEntry 'EthTransactionsRootTag TransactionsRoot)
    instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag TransactionsRoot

deriving via (RlpMerkleLogEntry 'EthReceiptsRootTag ReceiptsRoot)
    instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag ReceiptsRoot

deriving via (RlpMerkleLogEntry 'EthBloomTag Bloom)
    instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag Bloom

deriving via (RlpMerkleLogEntry 'EthDifficultyTag Difficulty)
    instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag Difficulty

deriving via (RlpMerkleLogEntry 'EthBlockNumberTag BlockNumber)
    instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag BlockNumber

deriving via (RlpMerkleLogEntry 'EthGasLimitTag GasLimit)
    instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag GasLimit

deriving via (RlpMerkleLogEntry 'EthGasUsedTag GasUsed)
    instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag GasUsed

deriving via (RlpMerkleLogEntry 'EthTimestampTag Timestamp)
    instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag Timestamp

deriving via (RlpMerkleLogEntry 'EthExtraDataTag ExtraData)
    instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag ExtraData

deriving via (RlpMerkleLogEntry 'EthRandaoTag Randao)
    instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag Randao

deriving via (RlpMerkleLogEntry 'EthNonceTag Nonce)
    instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag Nonce

deriving via (RlpMerkleLogEntry 'EthBaseFeePerGasTag BaseFeePerGas)
    instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag BaseFeePerGas

deriving via (RlpMerkleLogEntry 'EthWithdrawalsRootTag WithdrawalsRoot)
    instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag WithdrawalsRoot

deriving via (RlpMerkleLogEntry 'EthBlobGasUsedTag BlobGasUsed)
    instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag BlobGasUsed

deriving via (RlpMerkleLogEntry 'EthExcessBlobGasTag ExcessBlobGas)
    instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag ExcessBlobGas

deriving via (RlpMerkleLogEntry 'EthParentBeaconBlockRootTag ParentBeaconBlockRoot)
    instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag ParentBeaconBlockRoot

-- -------------------------------------------------------------------------- --
-- MerkleLog Instance

instance HasMerkleLog ChainwebMerkleHashAlgorithm ChainwebHashTag Header where

    -- /IMPORTANT/ a types must occur at most once in this list
    type MerkleLogHeader Header =
        '[ ParentHash
        , OmmersHash
        , Beneficiary
        , StateRoot
        , TransactionsRoot
        , ReceiptsRoot
        , Bloom
        , Difficulty
        , BlockNumber
        , GasLimit
        , GasUsed
        , Timestamp
        , ExtraData
        , Randao
        , Nonce
        , BaseFeePerGas
        , WithdrawalsRoot
        , BlobGasUsed
        , ExcessBlobGas
        , ParentBeaconBlockRoot
        ]
    type MerkleLogBody Header = Void

    toLog h = newMerkleLog @ChainwebMerkleHashAlgorithm entries
      where
        entries
            = _hdrParentHash h
            :+: _hdrOmmersHash h
            :+: _hdrBeneficiary h
            :+: _hdrStateRoot h
            :+: _hdrTransactionsRoot h
            :+: _hdrReceiptsRoot h
            :+: _hdrLogsBloom h
            :+: _hdrDifficulty h
            :+: _hdrNumber h
            :+: _hdrGasLimit h
            :+: _hdrGasUsed h
            :+: _hdrTimestamp h
            :+: _hdrExtraData h
            :+: _hdrPrevRandao h
            :+: _hdrNonce h
            :+: _hdrBaseFeePerGas h
            :+: _hdrWithdrawalsRoot h
            :+: _hdrBlobGasUsed h
            :+: _hdrExcessBlobGas h
            :+: _hdrParentBeaconBlockRoot h
            :+: emptyBody

    fromLog l = hdr
        { _hdrHash = computeBlockHash hdr
        , _hdrPayloadHash = computeBlockPayloadHash hdr
        }
      where
        hdr = Header
            { _hdrParentHash = hParentHash
            , _hdrOmmersHash = hOmmersHash
            , _hdrBeneficiary = hBeneficiary
            , _hdrStateRoot = hStateRoot
            , _hdrTransactionsRoot = hTransactionsRoot
            , _hdrReceiptsRoot = hReceiptsRoot
            , _hdrLogsBloom = hLogsBloom
            , _hdrDifficulty = hDifficulty
            , _hdrNumber = hNumber
            , _hdrGasLimit = hGasLimit
            , _hdrGasUsed = hGasUsed
            , _hdrTimestamp = hTimestamp
            , _hdrExtraData = hExtraData
            , _hdrPrevRandao = hPrevRandao
            , _hdrNonce = hNonce
            , _hdrBaseFeePerGas = hBaseFeePerGas
            , _hdrWithdrawalsRoot = hWithdrawalsRoot
            , _hdrBlobGasUsed = hBlobGasUsed
            , _hdrExcessBlobGas = hExcessBlobGas
            , _hdrParentBeaconBlockRoot = hParentBeaconBlockRoot
            , _hdrHash = error "Chainweb.PayloadProvider.EVM.Header: _hdrHash"
            , _hdrPayloadHash = error "Chainweb.PayloadProvider.EVM.Header: _hdrPayloadHash"
            }
        ( hParentHash
            :+: hOmmersHash
            :+: hBeneficiary
            :+: hStateRoot
            :+: hTransactionsRoot
            :+: hReceiptsRoot
            :+: hLogsBloom
            :+: hDifficulty
            :+: hNumber
            :+: hGasLimit
            :+: hGasUsed
            :+: hTimestamp
            :+: hExtraData
            :+: hPrevRandao
            :+: hNonce
            :+: hBaseFeePerGas
            :+: hWithdrawalsRoot
            :+: hBlobGasUsed
            :+: hExcessBlobGas
            :+: hParentBeaconBlockRoot
            :+: _
            ) = _merkleLogEntries l

-- -------------------------------------------------------------------------- --
-- Getter

hdrParentHash :: Getter Header ParentHash
hdrParentHash = to _hdrParentHash

hdrOmmersHash :: Getter Header OmmersHash
hdrOmmersHash = to _hdrOmmersHash

hdrBeneficiary :: Getter Header Beneficiary
hdrBeneficiary = to _hdrBeneficiary

hdrStateRoot :: Getter Header StateRoot
hdrStateRoot = to _hdrStateRoot

hdrTransactionsRoot :: Getter Header TransactionsRoot
hdrTransactionsRoot = to _hdrTransactionsRoot

hdrReceiptsRoot :: Getter Header ReceiptsRoot
hdrReceiptsRoot = to _hdrReceiptsRoot

hdrLogsBloom :: Getter Header Bloom
hdrLogsBloom = to _hdrLogsBloom

hdrDifficulty :: Getter Header Difficulty
hdrDifficulty = to _hdrDifficulty

hdrNumber :: Getter Header BlockNumber
hdrNumber = to _hdrNumber

hdrGasLimit :: Getter Header GasLimit
hdrGasLimit = to _hdrGasLimit

hdrGasUsed :: Getter Header GasUsed
hdrGasUsed = to _hdrGasUsed

hdrTimestamp :: Getter Header Timestamp
hdrTimestamp = to _hdrTimestamp

hdrExtraData :: Getter Header ExtraData
hdrExtraData = to _hdrExtraData

hdrPrevRandao :: Getter Header Randao
hdrPrevRandao = to _hdrPrevRandao

hdrNonce :: Getter Header Nonce
hdrNonce = to _hdrNonce

hdrBaseFeePerGas :: Getter Header BaseFeePerGas
hdrBaseFeePerGas = to _hdrBaseFeePerGas

hdrWithdrawalsRoot :: Getter Header WithdrawalsRoot
hdrWithdrawalsRoot = to _hdrWithdrawalsRoot

hdrHeight :: Getter Header Chainweb.BlockHeight
hdrHeight = to _hdrHeight

hdrHash :: Getter Header BlockHash
hdrHash = to _hdrHash

hdrPayloadHash :: Getter Header BlockPayloadHash
hdrPayloadHash = to _hdrPayloadHash

