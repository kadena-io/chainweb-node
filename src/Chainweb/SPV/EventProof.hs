{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.SPV.EventProof
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.SPV.EventProof
(
-- * Int256
  Int256
, int256
, unsafeInt256
, int256ToInteger
, OutOfBoundsException(..)
, putInt256Le
, putInt256Be
, getInt256Le
, getInt256Be
, int256Hex

-- * Transaction Output Events
, OutputEvents(..)
, encodeOutputEvents
, decodeOutputEvents

-- * Block Outputs Events
, BlockEventsHash
, BlockEventsHash_(..)
, encodeBlockEventsHash
, decodeBlockEventsHash
, BlockEvents
, BlockEvents_(..)
, blockEvents
, type BlockEventsLog
, verifyBlockEvents
, getBlockEvents

-- * Create Events Proofs
, createEventsProof
, createEventsProofKeccak256
, createEventsProofDb
, createEventsProofDbKeccak256

-- * Validate Events Proofs
, runEventsProof

-- * Internal
, eventsMerkleProof
, createEventsProof_
, createEventsProofDb_

-- ** Serialization Utils
, encodePactEvent
, decodePactEvent
, encodeParam
, decodeParam
, encodeBytes
, decodeBytes
, encodeString
, decodeString
, encodeArray
, decodeArray
, encodeInt256
, decodeInt256
) where

import Chainweb.Crypto.MerkleLog

import Control.DeepSeq
import Control.Exception (throw)
import Control.Monad
import Control.Monad.Catch

import Crypto.Hash.Algorithms

import Data.Aeson
import qualified Data.ByteArray as BA
import Data.Bytes.Get
import Data.Bytes.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.CAS
import Data.Decimal
import Data.Foldable
import Data.Hashable
import Data.MerkleLog hiding (Actual, Expected)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Data.Word

import GHC.Generics

import Numeric.Natural

import Pact.Types.Command
import Pact.Types.PactValue
import Pact.Types.Pretty
import Pact.Types.Runtime

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.SPV
import Chainweb.SPV.PayloadProof
import Chainweb.TreeDB hiding (entries, root)
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
--  Int256

newtype OutOfBoundsException = OutOfBoundsException Integer
    deriving (Show, Eq, Ord, Generic)

instance Exception OutOfBoundsException

newtype Int256 = Int256 Integer
    deriving (Show, Eq, Ord)

instance Bounded Int256 where
    minBound = Int256 (- 2^(255 :: Int))
    maxBound = Int256 (2^(255 :: Int) - 1)

int256ToInteger :: Int256 -> Integer
int256ToInteger (Int256 i) = i

int256 :: MonadThrow m => Integer -> m Int256
int256 i
    | i > int256ToInteger maxBound = throwM $ OutOfBoundsException i
    | i < int256ToInteger minBound = throwM $ OutOfBoundsException i
    | otherwise = return $ Int256 i

-- | Exposes the 'Int256' constructor without checking bounds.
--
unsafeInt256 :: Integer -> Int256
unsafeInt256 = Int256

putInt256Le :: forall m . MonadPut m => Int256 -> m ()
putInt256Le (Int256 i) = case compare i 0 of
    EQ -> replicateM_ 4 (putWord64le 0)
    GT -> go i
    LT -> go (2^(256 :: Int) + i)
  where
    go x = do
        putWord64le $ int r0
        putWord64le $ int r1
        putWord64le $ int r2
        putWord64le $ int r3
      where
        m64 = 2^(64 :: Int)
        (a0, r0) = quotRem x m64
        (a1, r1) = quotRem a0 m64
        (a2, r2) = quotRem a1 m64
        (_, r3) = quotRem a2 m64

putInt256Be :: forall m . MonadPut m => Int256 -> m ()
putInt256Be (Int256 i) = case compare i 0 of
    EQ -> replicateM_ 4 (putWord64be 0)
    GT -> go i
    LT -> go (2^(256 :: Int) + i)
  where
    go x = do
        putWord64le $ int r3
        putWord64le $ int r2
        putWord64le $ int r1
        putWord64le $ int r0
      where
        m64 = 2^(64 :: Int)
        (a0, r0) = quotRem x m64
        (a1, r1) = quotRem a0 m64
        (a2, r2) = quotRem a1 m64
        (_, r3) = quotRem a2 m64

getInt256Le :: forall m . MonadGet m => m Int256
getInt256Le = do
    w0 <- int <$> getWord64le
    w1 <- int <$> getWord64le
    w2 <- int <$> getWord64le
    w3 <- int <$> getWord64le
    let r = w0 + 2^(64 :: Int) * (w1 + 2^(64 :: Int) * (w2 + 2^(64 :: Int) * w3))
    return $ if r > int256ToInteger maxBound
      then Int256 (- (2^(256::Int) - r))
      else Int256 r

getInt256Be :: forall m . MonadGet m => m Int256
getInt256Be = do
    w3 <- int <$> getWord64le
    w2 <- int <$> getWord64le
    w1 <- int <$> getWord64le
    w0 <- int <$> getWord64le
    let r = w0 + 2^(64 :: Int) * (w1 + 2^(64 :: Int) * (w2 + 2^(64 :: Int) * w3))
    return $ if r > int256ToInteger maxBound
      then Int256 (- (2^(256::Int) - r))
      else Int256 r

int256Hex :: Int256 -> B.ByteString
int256Hex x@(Int256 i)
  | i >= 0 = "0x" <> B16.encode (runPut $ putInt256Be x)
  | otherwise = "-" <> int256Hex (Int256 (-i))

-- -------------------------------------------------------------------------- --
-- Pact Event Encoding

data PactEventEncodingException
    = ArrayTooBigException !Int
    | ByteStringTooBigException !Int
    | IntegerOutOfBoundsException !Integer
    | UnsupportedPactValueException !PactValue
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

instance Exception PactEventEncodingException

encodePactEvent :: MonadPut m => PactEvent -> m ()
encodePactEvent e = do
    encodeString $ _eventName e
    encodeString $ asString $ _eventModule e
    encodeHash $ _mhHash $ _eventModuleHash e
    encodeArray (_eventParams e) encodeParam

-- | This throws a pure exception of type 'PactEventEncodingException', if the
-- input bytestring is too long.
--
encodeBytes :: MonadPut m => B.ByteString -> m ()
encodeBytes b
    | B.length b <= int (maxBound @Word32)
        = putWord8 0x0 >> putWord32le (int $ B.length b) >> putByteString b
    | otherwise = throw $ ByteStringTooBigException (B.length b)

-- | This throws a pure exception of type 'PactEventEncodingException', if the
-- input value is to big or too small.
--
encodeInteger :: MonadPut m => Integer -> m ()
encodeInteger i = case int256 i of
    Left _ -> throw $ IntegerOutOfBoundsException i
    Right x -> encodeInt256 x

encodeInt256 :: MonadPut m => Int256 -> m ()
encodeInt256 i = putWord8 0x1 >> putInt256Le i

encodeString :: MonadPut m => T.Text -> m ()
encodeString n = encodeBytes $ T.encodeUtf8 n

encodeDecimal :: MonadPut m => Decimal -> m ()
encodeDecimal d = case int256 i of
    Left _ -> throw $ IntegerOutOfBoundsException i
    Right x -> putWord8 0x2 >> putInt256Le x
  where
    i = round @Decimal @Integer $ d * 10^(18 :: Int)

-- | This throws a pure exception of type 'PactEventEncodingException', if the
-- size of the input array is too big.
--
encodeArray
    :: forall a f m
    . MonadPut m
    => Foldable f
    => f a
    -> (a -> m ())
    -> m ()
encodeArray a f
    | length a > int (maxBound @Word32) = throw $ ArrayTooBigException (length a)
    | otherwise = putWord32le (int $ length a) >> traverse_ f a

encodeHash :: MonadPut m => Hash -> m ()
encodeHash = encodeBytes . unHash

encodeParam :: MonadPut m => PactValue -> m ()
encodeParam (PLiteral (LString t)) = encodeString t
encodeParam (PLiteral (LInteger i)) = encodeInteger i
encodeParam (PLiteral (LDecimal i)) = encodeDecimal i
encodeParam (PModRef n) = encodeModRef n
encodeParam e = throw $ UnsupportedPactValueException e

encodeModRef :: MonadPut m => ModRef -> m ()
encodeModRef n = do
    putWord8 0x3
    encodeString (renderCompactText n)

-- -------------------------------------------------------------------------- --
-- Pact Event Decoding

decodePactEvent :: MonadGetExtra m => m PactEvent
decodePactEvent = label "decodeEvent" $ do
    name <- decodeString
    m <- decodeModule
    mh <- ModuleHash <$> decodeHash
    params <- decodeArray decodeParam
    return $ PactEvent
        { _eventModule = m
        , _eventName = name
        , _eventModuleHash = mh
        , _eventParams = params
        }

decodeArray :: MonadGetExtra m => m a -> m [a]
decodeArray f = label "decodeArray" $ do
    l <- getWord32le
    label ("#" <> show l) $ forM [0 :: Int .. int l - 1] $ \i ->
        label ("[" <> show i <> "]") f

decodeHash :: MonadGetExtra m => m Hash
decodeHash = label "decodeHash" $ Hash <$> decodeBytes

decodeBytes :: MonadGetExtra m => m B.ByteString
decodeBytes = label "decodeBytes" $ do
    expect 0x0
    l <- getWord32le
    getBytes (int l)

expect :: MonadGetExtra m => Word8 -> m ()
expect c = label "expect" $ do
    c' <- getWord8
    unless (c == c') $
        fail $ "decodeOutputEvents: failed to decode, expected " <> show c <> " but got " <> show c'

decodeInt256 :: MonadGetExtra m => m Int256
decodeInt256 = label "decodeInt256" $ expect 0x1 >> getInt256Le

decodeInteger :: MonadGetExtra m => m Integer
decodeInteger = label "decodeInteger" $ int256ToInteger <$> decodeInt256

decodeParam :: MonadGetExtra m => m PactValue
decodeParam = label "decodeParam" $ lookAhead getWord8 >>= \case
    0x0 -> label "LString" $ PLiteral . LString <$> decodeString
    0x1 -> label "LInteger" $ PLiteral . LInteger <$> decodeInteger
    0x2 -> label "LDecimal" $ PLiteral . LDecimal <$> decodeDecimal
    e -> fail $ "decodeParam: unexpected parameter with type tag " <> show e
  where
    decodeDecimal = error "TODO"

decodeModule :: MonadGetExtra m => m ModuleName
decodeModule = label "decodeModule" $ do
    t <- decodeString
    case T.split (== '.') t of
        [] -> fail "empty module name"
        [m] -> return $ ModuleName m Nothing
        [n,m] -> return $ ModuleName m (Just $ NamespaceName n)
        _ -> fail $ "illegal nested module name namespace: " <> show t

decodeString :: MonadGetExtra m => m T.Text
decodeString = label "decodeString" $ do
    t <- decodeBytes
    case T.decodeUtf8' t of
        Left e -> fail $ "failed to decode UTF8 string: " <> show e
        Right x -> return x

-- -------------------------------------------------------------------------- --
-- Block Events Hash
--

type BlockEventsHash = BlockEventsHash_ ChainwebMerkleHashAlgorithm

-- | Merkle Root for all events in a Block
--
newtype BlockEventsHash_ a = BlockEventsHash (MerkleLogHash a)
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (BA.ByteArrayAccess)
    deriving newtype (Hashable, ToJSON, FromJSON)

encodeBlockEventsHash :: MonadPut m => BlockEventsHash_ a -> m ()
encodeBlockEventsHash (BlockEventsHash w) = encodeMerkleLogHash w

decodeBlockEventsHash
    :: MerkleHashAlgorithm a
    => MonadGet m
    => m (BlockEventsHash_ a)
decodeBlockEventsHash = BlockEventsHash <$!> decodeMerkleLogHash

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag (BlockEventsHash_ a) where
    type Tag (BlockEventsHash_ a) = 'BlockEventsHashTag
    toMerkleNode = encodeMerkleTreeNode
    fromMerkleNode = decodeMerkleTreeNode
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

-- -------------------------------------------------------------------------- --
-- Output Events

-- | This type represents all events within the output of a transaction.
--
-- Pact events are included in transaction outputs. We provide a separate Merkle
-- tree for events because they are more compact and have and encoding that is
-- easier to parse in Solidity code. The flip side is that the trees are not
-- included in the chainweb block chains and the roots of respective proof must
-- be authenticated by other means.
--
data OutputEvents = OutputEvents
    { _outputEventsRequestKey :: !RequestKey
    , _outputEventsEvents :: !(V.Vector PactEvent)
    }
    deriving (Show, Eq, Generic)

encodeOutputEvents :: forall m . MonadPut m => OutputEvents -> m ()
encodeOutputEvents es = do
    encodeHash $ unRequestKey $ _outputEventsRequestKey es
    encodeArray (_outputEventsEvents es) encodePactEvent

decodeOutputEvents :: forall m . MonadGetExtra m => m OutputEvents
decodeOutputEvents = label "OutputEvents" $ OutputEvents
    <$> (RequestKey <$> decodeHash)
    <*> (V.fromList <$> decodeArray decodePactEvent)

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag OutputEvents where
    type Tag OutputEvents = 'OutputEventsTag
    toMerkleNode = encodeMerkleInputNode encodeOutputEvents
    fromMerkleNode = decodeMerkleInputNode decodeOutputEvents

-- -------------------------------------------------------------------------- --
-- Block Events

type BlockEvents = BlockEvents_ ChainwebMerkleHashAlgorithm

data BlockEvents_ a = BlockEvents
    { _blockEventsHash :: !(BlockEventsHash_ a)
    , _blockEventsEvents :: !(V.Vector OutputEvents)
    }
    deriving (Show, Eq, Generic)

instance MerkleHashAlgorithm a => HasMerkleLog a ChainwebHashTag (BlockEvents_ a) where
    type MerkleLogHeader (BlockEvents_ a) = '[]
    type MerkleLogBody (BlockEvents_ a) = OutputEvents
    toLog a = merkleLog root entries
      where
        BlockEventsHash (MerkleLogHash !root) = _blockEventsHash a
        !entries = MerkleLogBody (_blockEventsEvents a)

    fromLog l = BlockEvents
        { _blockEventsHash = BlockEventsHash $! MerkleLogHash $! _merkleLogRoot l
        , _blockEventsEvents = es
        }
      where
        (MerkleLogBody es) = _merkleLogEntries l

-- | Smart Constructor for 'BlockEvents'
--
blockEvents
    :: forall a
    . MerkleHashAlgorithm a
    => V.Vector OutputEvents
    -> BlockEvents_ a
blockEvents = fromLog . newMerkleLog @a . MerkleLogBody

type BlockEventsLog a = MkLogType a ChainwebHashTag (BlockEvents_ a)

-- | Verify the consistency of the MerkleTree of a 'BlockEvents' value.
--
-- This forces the MerkleTree which can be (somewhat) expensive for large input
-- values.
--
verifyBlockEvents :: MerkleHashAlgorithm a => BlockEvents_ a -> Bool
verifyBlockEvents p =
    BlockEventsHash (MerkleLogHash $ computeMerkleLogRoot p) == _blockEventsHash p

getBlockEvents
    :: forall a b m
    . MonadThrow m
    => MerkleHashAlgorithm b
    => PayloadWithOutputs_ a
    -> m (BlockEvents_ b)
getBlockEvents p = fmap blockEvents $
    forM (_payloadWithOutputsTransactions p) $ \(_, o) -> do
        r <- decodeStrictOrThrow @_ @(CommandResult Hash) $ _transactionOutputBytes o
        return $ OutputEvents (_crReqKey r) (V.fromList (_crEvents r))

-- -------------------------------------------------------------------------- --
-- Events Proofs

eventsMerkleProof
    :: forall a m h
    . MonadThrow m
    => MerkleHashAlgorithm a
    => PayloadWithOutputs_ h
    -> RequestKey
        -- ^ RequestKey of the transaction
    -> m (MerkleProof a)
eventsMerkleProof p reqKey = do
    events <- getBlockEvents @_ @a p

    -- the pact events of the tx output with @reqKey@ within the block
    i <- case V.findIndex ((== reqKey) . _outputEventsRequestKey) (_blockEventsEvents events) of
        Nothing -> throwM $ RequestKeyNotFoundException reqKey
        Just x -> return x

    -- Create proof from outputs tree and payload tree
    let (!subj, !pos, !t) = bodyTree events i
    merkleProof subj pos t

createEventsProof_
    :: forall a
    . MerkleHashAlgorithm a
    => PayloadWithOutputs
    -> RequestKey
        -- ^ RequestKey of the transaction
    -> IO (PayloadProof a)
createEventsProof_ payload reqKey = do
    proof <- eventsMerkleProof @a payload reqKey
    return PayloadProof
        { _payloadProofRootType = RootBlockEvents
        , _payloadProofBlob = proof
        }

createEventsProof
    :: PayloadWithOutputs
    -> RequestKey
        -- ^ RequestKey of the transaction
    -> IO (PayloadProof ChainwebMerkleHashAlgorithm)
createEventsProof = createEventsProof_

createEventsProofKeccak256
    :: PayloadWithOutputs
    -> RequestKey
        -- ^ RequestKey of the transaction
    -> IO (PayloadProof Keccak_256)
createEventsProofKeccak256 = createEventsProof_

-- -------------------------------------------------------------------------- --
-- Create Events Proof using Payload Db and check header depth

-- TODO: add a parameter for a minimum depth of the proof

createEventsProofDb_
    :: forall a cas
    . MerkleHashAlgorithm a
    => PayloadCasLookup cas
    => BlockHeaderDb
    -> PayloadDb cas
    -> Natural
        -- ^ minimum depth of the target header in the block chain. The current
        -- header of the chain has depth 0.
    -> BlockHash
        -- ^ the target header of the proof
    -> RequestKey
        -- ^ RequestKey of the transaction
    -> IO (PayloadProof a)
createEventsProofDb_ headerDb payloadDb d h reqKey = do
    hdr <- casLookupM headerDb h
    p <- casLookupM payloadDb (_blockPayloadHash hdr)
    unless (_payloadWithOutputsPayloadHash p /= _blockPayloadHash hdr) $
        throwM $ SpvExceptionInconsistentPayloadData
            { _spvExceptionMsg = "The stored payload hash doesn't match the the db index"
            , _spvExceptionMsgPayloadHash = _blockPayloadHash hdr
            }
    curRank <- maxRank headerDb
    unless (int (_blockHeight hdr) + d <= curRank) $
        throwM $ SpvExceptionInsufficientProofDepth
            { _spvExceptionMsg = "Insufficient depth of root header for SPV proof"
            , _spvExceptionExpectedDepth = Expected d
            , _spvExceptionActualDepth = Actual $ curRank `minusOrNull` int (_blockHeight hdr)
            }
    createEventsProof_ p reqKey

createEventsProofDb
    :: PayloadCasLookup cas
    => BlockHeaderDb
    -> PayloadDb cas
    -> Natural
        -- ^ minimum depth of the target header in the block chain. The current
        -- header of the chain has depth 0.
    -> BlockHash
        -- ^ the target header of the proof
    -> RequestKey
        -- ^ RequestKey of the transaction
    -> IO (PayloadProof ChainwebMerkleHashAlgorithm)
createEventsProofDb = createEventsProofDb_

createEventsProofDbKeccak256
    :: PayloadCasLookup cas
    => BlockHeaderDb
    -> PayloadDb cas
    -> Natural
        -- ^ minimum depth of the target header in the block chain. The current
        -- header of the chain has depth 0.
    -> BlockHash
        -- ^ the target header of the proof
    -> RequestKey
        -- ^ RequestKey of the transaction
    -> IO (PayloadProof Keccak_256)
createEventsProofDbKeccak256 = createEventsProofDb_

-- -------------------------------------------------------------------------- --
-- Proof Validation

-- | Run the Payload Proof. Returns the root and the subject. The first type
-- parameter determines what type is expected.
--
-- Running the proof reduces that task of authenticating the subject to the
-- (smaller) task of authenticating just the root. Also, all transactions
-- of the a block share the same root. So, given respective proofs, all elements
-- of the payload can be authenticated via authentication of a single root.
--
-- NOTE: It is up the caller to validate the authenticity of the returned root
-- hash. The proof only claims that the subject is contained in the root.
--
runEventsProof
    :: forall a m
    . MonadThrow m
    => MerkleHashAlgorithm a
    => PayloadProof a
    -> m (BlockEventsHash_ a, OutputEvents)
runEventsProof p = do
    (t, r, s) <- runPayloadProof p
    unless (t == RootBlockEvents) $ throwM
        $ MerkleRootMismatch (Expected RootBlockEvents) (Actual t)
    return (BlockEventsHash r, s)

