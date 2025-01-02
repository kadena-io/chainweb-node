{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.PayloadProvider.Minimal.Payload
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.PayloadProvider.Minimal.Payload
( Account
, account
, invalidAccount
, Payload
, newPayload
, payloadMinerReward
, payloadChainwebVersion
, payloadChainId
, payloadBlockHeight
, payloadRedeemChain
, payloadRedeemAccount
, payloadHash
, genesisPayload
, encodePayload
, decodePayload
, type PayloadCas
, proof
, runProof
) where

import Chainweb.BlockHeight
import Chainweb.BlockPayloadHash
import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.MinerReward
import Chainweb.Storage.Table
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Version
import Chainweb.Version.Registry
import Control.Lens (Getter)
import Control.Lens.Getter (to)
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.ByteString.Short qualified as BS
import Data.Function
import Data.Hashable
import Data.MerkleLog (MerkleProof, runMerkleProof)
import Data.Void
import Data.Word
import GHC.Generics (Generic)
import GHC.Stack
import Numeric.Natural

-- -------------------------------------------------------------------------- --
-- Account Info

-- | Maximum allowed account size. This must be verified by the payload
-- provider.
--
-- Omitting to enforce this would be DoS attack vector.
--
maxAccountSize :: Integral n => n
maxAccountSize = 512
{-# INLINE maxAccountSize #-}

data AccountToLargeException =
    AccountTooLarge (Actual Natural) (Expected Natural)
    deriving (Show, Eq, Generic)

instance Exception AccountToLargeException

accountTooLarge :: Integral n => n -> AccountToLargeException
accountTooLarge actual =
    AccountTooLarge (Actual (int actual)) (Expected maxAccountSize)

-- | Account Information in a payload provider specific format.
--
-- NOTE: The maximum length is 512 bytes. This must be enforced during payload
-- validation. It is implied by 'decodeAccount' and the 'FromJSON' instance.
--
newtype Account = Account BS.ShortByteString
    deriving (Show, Eq, Ord, Generic)
    deriving (ToJSON, FromJSON) via (JsonTextRepresentation "Account" Account)

-- | Creates a new Account value and checks that the size is within the
-- acceptable limits.
--
account :: MonadThrow m => BS.ShortByteString -> m Account
account bs
    | BS.length bs > maxAccountSize =
        throwM $ accountTooLarge (BS.length bs)
    | otherwise = return $ Account bs

encodeAccount :: HasCallStack => Account -> Put
encodeAccount (Account bs) = do
    let l = BS.length bs
    void $ when (l > int (maxBound @Word16)) $
        error "Chainweb.PayloadProvider.Minimal.encodePayload: account is too large"
    putWord16le (int l)
    putShortByteString bs
{-# INLINE encodeAccount #-}

decodeAccount :: Get Account
decodeAccount = do
    l <- int <$> getWord16le
    bs <- getShortByteString l
    account bs
{-# INLINE decodeAccount #-}

instance HasTextRepresentation Account where
    toText (Account bs) = encodeB64UrlNoPaddingText $ BS.fromShort bs
    fromText = account . BS.toShort <=< decodeB64UrlNoPaddingText
    {-# INLINEABLE toText #-}
    {-# INLINEABLE fromText #-}

-- | An account that _may_ be invalid. It is not guaranteed to be invalid!
--
-- This assumed to be an invalid account on all providers. However, do not
-- count on it!
--
invalidAccount :: Account
invalidAccount = Account ""

-- -------------------------------------------------------------------------- --

-- | Payload for the Minimal Payload Provider
--
-- Its sole purpose is to enable payout of miner rewards.
--
-- Payload validation witnesses that the payload properties match the respective
-- values frm the evaluation context for the block.
--
-- The output of payload validation is the payload itself. The BlockPayloadHash
-- is the Merkle root of this structure.
--
data Payload = Payload
    -- Provenance data:
    -- (must be consistent with the evaluation context)
    { _payloadChainwebVersion :: !ChainwebVersionCode
        -- ^ The chainweb version of the block
    , _payloadChainId :: !ChainId
        -- ^ The chain of the block
    , _payloadBlockHeight :: !BlockHeight
        -- ^ The height of the block.
    , _payloadMinerReward :: !MinerReward
        -- ^ The miner reward for the block in Stu.

    -- Miner data:
    , _payloadRedeemChain :: !ChainId
        -- ^ The unique chain on which the payload can redeemed.
        -- If the chain does not (yet) exist, the miner reward is inaccessible.
    , _payloadRedeemAccount :: !Account
        -- ^ A key that enables the payout of the miner reward on the target
        -- chain. The format of the key depends on '_payloadRedeemChain'.

    -- Synthetic field
    , _payloadHash :: {- Lazy -} BlockPayloadHash
        -- ^ Merkle root for this payload. It is used as BlockPayloadHash
        -- in the block.
    }
    deriving (Show, Generic)

instance HasChainwebVersion Payload where
    _chainwebVersion = lookupVersionByCode . _payloadChainwebVersion

instance HasChainId Payload where
    _chainId = _payloadChainId

-- -------------------------------------------------------------------------- --

instance Hashable Payload where
    hashWithSalt s = hashWithSalt s . _payloadHash
    {-# INLINEABLE hashWithSalt #-}

instance Eq Payload where
    (==) = (==) `on` _payloadHash

instance Ord Payload where
    compare = compare `on` (\h -> (_payloadBlockHeight h, _payloadHash h))

instance IsCasValue Payload where
    type CasKeyType Payload = RankedBlockPayloadHash
    casKey p = RankedBlockPayloadHash (_payloadBlockHeight p) (_payloadHash p)
    {-# INLINE casKey #-}

type PayloadCas tbl = Cas tbl Payload

-- -------------------------------------------------------------------------- --
-- Internal Block Hash Computation

-- | Compute the hash of a payload.
--
-- Does not force '_payloadHash' in the input
--
computeHash :: Payload -> BlockPayloadHash
computeHash h = BlockPayloadHash $ MerkleLogHash $ computeMerkleLogRoot h
{-# INLINE computeHash #-}

-- -------------------------------------------------------------------------- --
-- Merkle Proofs
--
-- Example:
--
-- ghci> Just p = proof @BlockHeight pld
-- ghci> runProof p == blockPayloadHash pld
-- True
-- ghci> proofSubject @_ @_ @BlockHeight p
-- BlockHeight 6373

-- | Creates a Merkle proof for a header property of a Payload.
--
proof
    :: forall a m
    . MonadThrow m
    => a ~ ChainwebMerkleHashAlgorithm
    -- => HasHeader a ChainwebHashTag c (MkLogType a ChainwebHashTag Payload)
    => Payload
    -> m (MerkleProof a)
proof = headerProof @Payload
{-# INLINE proof #-}

-- | Runs a proof. Returns the BlockPayloadHash of the payload for which
-- inclusion is proven.
--
runProof :: MerkleProof ChainwebMerkleHashAlgorithm -> BlockPayloadHash
runProof = BlockPayloadHash . MerkleLogHash . runMerkleProof
{-# INLINE runProof #-}

-- -------------------------------------------------------------------------- --
-- JSON Serialization

-- | JSON properties
--
-- The JSON serialization also includes the block hash.
--
properties :: KeyValue e kv => Payload -> [kv]
properties o =
    [ "chainwebVersion" .= _payloadChainwebVersion o
    , "chainId" .= _payloadChainId o
    , "blockHeight" .= _payloadBlockHeight o
    , "minerReward" .= _payloadMinerReward o
    , "redeemChain" .= _payloadRedeemChain o
    , "redeemAccount" .= _payloadRedeemAccount o
    , "hash" .= _payloadHash o
    ]
{-# INLINE properties #-}
{-# SPECIALIZE properties :: Payload -> [Series] #-}
{-# SPECIALIZE properties :: Payload -> [Pair] #-}

instance ToJSON Payload where
    toEncoding = pairs . mconcat . properties
    toJSON = object . properties
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON Payload where
    parseJSON v = do
        pld <- flip (withObject "Payload") v $ \o -> Payload
            <$> o .: "chainwebVersion"
            <*> o .: "chainId"
            <*> o .: "blockHeight"
            <*> o .: "minerReward"
            <*> o .: "redeemChain"
            <*> o .: "redeemAccount"
            <*> pure (error "Chainweb.PayloadProvider.Minimal: _payloadHash")
        return pld
            { _payloadHash = computeHash pld
            }
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Serialization

-- | Serialization
--
-- (this does not include the payload hash)
--
encodePayload :: HasCallStack => Payload -> Put
encodePayload pld = do
    encodeChainwebVersionCode (_payloadChainwebVersion pld)
    encodeChainId (_payloadChainId pld)
    encodeBlockHeight (_payloadBlockHeight pld)
    encodeMinerReward (_payloadMinerReward pld)
    encodeChainId (_payloadRedeemChain pld)
    encodeAccount (_payloadRedeemAccount pld)

decodePayload :: HasCallStack => Get Payload
decodePayload = do
    pld <- Payload
        <$> decodeChainwebVersionCode
        <*> decodeChainId
        <*> decodeBlockHeight
        <*> decodeMinerReward
        <*> decodeChainId
        <*> decodeAccount
        <*> pure (error "Chainweb.PayloadProvider.Minimal.decodePayload: attempt decode hash")
    return pld { _payloadHash = computeHash pld }

-- -------------------------------------------------------------------------- --

genesisPayload
    :: HasCallStack
    => HasChainwebVersion v
    => HasChainId cid
    => v
    -> cid
    -> Payload
genesisPayload v cid
    | payloadProviderTypeForChain v cid /= MinimalProvider =
        error "Chainweb.PayloadProvider.Minimal.Payload.genesisPayload: chain does not use minimal provider"
    | otherwise = pld
  where
    genHeight = genesisBlockHeight (_chainwebVersion v) (_chainId cid)
    pld = Payload
        { _payloadChainwebVersion = _versionCode (_chainwebVersion v)
        , _payloadChainId = _chainId cid
        , _payloadBlockHeight = genHeight
        , _payloadMinerReward = MinerReward 0
            -- genesis blocks are not mined, hence there is no reward.
        , _payloadRedeemChain = _chainId cid
            -- There is nothing to redeem in a genesis block.
        , _payloadRedeemAccount = Account mempty
            -- There is nothing to redeem in a genesis block. Even if this would
            -- be valid Account on some chain, the redeem chain is fixed to be
            -- the chain of the block itself and the reward is 0.
        , _payloadHash = computeHash pld
        }

newPayload
    :: HasCallStack
    => HasChainwebVersion v
    => HasChainId cid
    => v
    -> cid
    -> BlockHeight
    -> MinerReward
    -> ChainId
    -> Account
    -> Payload
newPayload v c h r rc acc = pld
  where
    pld = Payload
        { _payloadChainwebVersion = _versionCode (_chainwebVersion v)
        , _payloadChainId = _chainId c
        , _payloadBlockHeight = h
        , _payloadMinerReward = r
        , _payloadRedeemChain = rc
        , _payloadRedeemAccount = acc
        , _payloadHash = computeHash pld
        }

-- -------------------------------------------------------------------------- --
-- MerkleLog Entry

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag Payload
  where
    type Tag @ChainwebHashTag Payload = MinimalPayloadTag
    toMerkleNode = encodeMerkleInputNode encodePayload
    fromMerkleNode = decodeMerkleInputNode decodePayload
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

-- -------------------------------------------------------------------------- --
-- MerkleLog Instance

instance HasMerkleLog ChainwebMerkleHashAlgorithm ChainwebHashTag Payload where

    -- /IMPORTANT/ a types must occur at most once in this list
    type MerkleLogHeader Payload = '[Payload]
    type MerkleLogBody Payload = Void

    toLog h = newMerkleLog @ChainwebMerkleHashAlgorithm entries
      where
        entries = h :+: emptyBody

    fromLog l = pld
        { _payloadHash = computeHash pld
        }
      where
        (pld :+: _) = _merkleLogEntries l

-- -------------------------------------------------------------------------- --
-- Getter

payloadMinerReward :: Getter Payload MinerReward
payloadMinerReward = to _payloadMinerReward

payloadChainwebVersion :: Getter Payload ChainwebVersionCode
payloadChainwebVersion = to _payloadChainwebVersion

payloadChainId :: Getter Payload ChainId
payloadChainId = to _payloadChainId

payloadBlockHeight :: Getter Payload BlockHeight
payloadBlockHeight = to _payloadBlockHeight

payloadRedeemChain :: Getter Payload ChainId
payloadRedeemChain = to _payloadRedeemChain

payloadRedeemAccount :: Getter Payload Account
payloadRedeemAccount = to _payloadRedeemAccount

payloadHash :: Getter Payload BlockPayloadHash
payloadHash = to _payloadHash

