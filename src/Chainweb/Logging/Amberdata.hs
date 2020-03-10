{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Logging.Amberdata
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Data structures and functions for logging 'BlocHeader' data to Amberdata
--
module Chainweb.Logging.Amberdata
( AmberdataBlock(..)
, AmberdataConfig(..)
, defaultAmberdataConfig
, validateAmberdataConfig
, pAmberdataConfig
, amberdataBlockMonitor
, withAmberDataBlocksBackend
) where

import Configuration.Utils hiding (Error, Lens)

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Lens (view)
import Control.Lens.TH
import Control.Monad
import Control.Monad.Error.Class (throwError)

import Data.Bool
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import Data.CAS
import qualified Data.Foldable as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T


import GHC.Generics

import qualified Network.HTTP.Client as HTTP

import Numeric.Natural

import qualified Streaming.Prelude as S

import System.IO
import qualified System.Logger as L
import System.LogLevel

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.BlockWeight
import Chainweb.CutDB
import Chainweb.HostAddress
import Chainweb.Logger
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Time
import Chainweb.Utils hiding (check)
import Chainweb.Version

import Data.LogMessage

import Utils.Logging

-- -------------------------------------------------------------------------- --
-- AmberDataBlock

data AmberdataBlock = AmberdataBlock
  { _amberdataNumber :: {-# UNPACK #-} !BlockHeight
  , _amberdataHash :: {-# UNPACK #-} !BlockHash
  , _amberdataTimestamp :: {-# UNPACK #-} !BlockCreationTime
  , _amberdataParentHash :: {-# UNPACK #-} !BlockHash
  , _amberdataNonce :: {-# UNPACK #-} !Nonce
  , _amberdataSize :: {-# UNPACK #-} !Word  -- ^ Bytes
  , _amberdataNumTransactions :: {-# UNPACK #-} !Word
  , _amberdataMeta :: {-# UNPACK #-} !ChainId
  , _amberdataDifficulty :: {-# UNPACK #-} !BlockWeight
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON AmberdataBlock where
    toJSON o = object
        [ "number" .= _amberdataNumber o
        , "hash" .= _amberdataHash o
        , "timestamp" .= microToMilliSeconds (_amberdataTimestamp o)
        , "parentHash" .= _amberdataParentHash o
        , "nonce" .= _amberdataNonce o
        , "size" .= _amberdataSize o
        , "numTransactions" .= _amberdataNumTransactions o
        , "meta" .= toText (_amberdataMeta o)
        , "difficulty" .= blockWeightToNumber (_amberdataDifficulty o)
        ]
      where
        microToMilliSeconds :: BlockCreationTime -> Integer
        microToMilliSeconds (BlockCreationTime (Time (TimeSpan (Micros m))))
            = int $ m `div` 1000

        blockWeightToNumber :: BlockWeight -> Value
        blockWeightToNumber (BlockWeight w) = (toJSON . Number . fromIntegral) w

-- -------------------------------------------------------------------------- --
-- Amberdata config

-- | Backend for logging to Amberdata
--
amberdataComponentPrefix :: String
amberdataComponentPrefix = "amberdata"


defaultAmberdataHost :: HostAddress
defaultAmberdataHost
  = unsafeHostAddressFromText "localhost:443"

validateAmberdataHost :: ConfigValidation HostAddress []
validateAmberdataHost h
  | h == defaultAmberdataHost =
      throwError "Amberdata host must be provided by the user."
  | otherwise = return ()

pAmberdataHost :: OptionParser HostAddress
pAmberdataHost = textOption
  $ prefixLong (Just amberdataComponentPrefix) "host"
  <> suffixHelp Nothing "<HOST>:<PORT> to send Amberdata logs."


newtype AmberdataApiKey
  = AmberdataApiKey T.Text
  deriving (Show, Eq, Ord, Generic, NFData)
instance HasTextRepresentation AmberdataApiKey where
    toText (AmberdataApiKey a) = a
    fromText t = return $ AmberdataApiKey t
    {-# INLINE toText #-}
    {-# INLINE fromText #-}
instance ToJSON AmberdataApiKey where
    toJSON = toJSON . toText
    {-# INLINE toJSON #-}
instance FromJSON AmberdataApiKey where
    parseJSON = parseJsonFromText "AmberdataApiKey"
    {-# INLINE parseJSON #-}

validateAmberdataApiKey :: ConfigValidation AmberdataApiKey []
validateAmberdataApiKey (AmberdataApiKey "") =
  throwError "Nonempty Amberdata api key must be provided by the user."
validateAmberdataApiKey _ = return ()

pAmberdataApiKey :: OptionParser AmberdataApiKey
pAmberdataApiKey = textOption
    $ prefixLong (Just amberdataComponentPrefix) "api-key"
    <> suffixHelp Nothing "API key for logging to Amberdata."


newtype AmberdataBlockchainId
  = AmberdataBlockchainId T.Text
  deriving (Show, Eq, Ord, Generic, NFData)
instance HasTextRepresentation AmberdataBlockchainId where
    toText (AmberdataBlockchainId a) = a
    fromText t = return $ AmberdataBlockchainId t
    {-# INLINE toText #-}
    {-# INLINE fromText #-}
instance ToJSON AmberdataBlockchainId where
    toJSON = toJSON . toText
    {-# INLINE toJSON #-}
instance FromJSON AmberdataBlockchainId where
    parseJSON = parseJsonFromText "AmberdataBlockchainId"
    {-# INLINE parseJSON #-}

validateAmberdataBlockchainId :: ConfigValidation AmberdataBlockchainId []
validateAmberdataBlockchainId (AmberdataBlockchainId "") =
  throwError "Nonempty Amberdata blockchain id must be provided by the user."
validateAmberdataBlockchainId _ = return ()

pAmberdataBlockchainId :: OptionParser AmberdataBlockchainId
pAmberdataBlockchainId = textOption
    $ prefixLong (Just amberdataComponentPrefix) "blockchain-id"
    <> suffixHelp Nothing "Blockchain id for logging to Amberdata."

pAmberdataOptionalChainId :: OptionParser (Maybe ChainId)
pAmberdataOptionalChainId = fmap Just $ textOption
  $ prefixLong (Just amberdataComponentPrefix) "chain-id"
  <> suffixHelp Nothing
     ("If present, will only log Amberdata data from given chain. "
      ++ "If absent, will log Amberdata data from ALL chains.")


pEnableAmberdataDebug
  :: OptionParser Bool
pEnableAmberdataDebug = enableDisableFlag
  $ prefixLong (Just amberdataComponentPrefix) "debug"
  <> suffixHelp Nothing "Whether to enable Amberdata debugging."


data AmberdataConfig = AmberdataConfig
    { _ambredataConfigHost :: HostAddress
    , _amberdataApiKey :: AmberdataApiKey
    , _amberdataBlockchainId :: AmberdataBlockchainId
    , _amberdataChainId :: Maybe ChainId
    , _amberdataDebug :: Bool
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''AmberdataConfig

defaultAmberdataConfig :: AmberdataConfig
defaultAmberdataConfig
  = AmberdataConfig
    defaultAmberdataHost
    (AmberdataApiKey "")
    (AmberdataBlockchainId "")
    Nothing
    True

validateAmberdataConfig :: ConfigValidation AmberdataConfig []
validateAmberdataConfig o = do
  validateAmberdataHost $ _ambredataConfigHost o
  validateAmberdataApiKey $ _amberdataApiKey o
  validateAmberdataBlockchainId $ _amberdataBlockchainId o

pAmberdataConfig
  :: MParser AmberdataConfig
pAmberdataConfig = id
  <$< ambredataConfigHost .:: pAmberdataHost
  <*< amberdataApiKey .:: pAmberdataApiKey
  <*< amberdataBlockchainId .:: pAmberdataBlockchainId
  <*< amberdataChainId .:: pAmberdataOptionalChainId
  <*< amberdataDebug .:: pEnableAmberdataDebug

instance ToJSON AmberdataConfig where
  toJSON o = object
    [ "host" .= _ambredataConfigHost o
    , "apiKey" .= _amberdataApiKey o
    , "blockchainId" .= _amberdataBlockchainId o
    , "chainId" .= _amberdataChainId o
    , "debug" .= _amberdataDebug o
    ]

instance FromJSON (AmberdataConfig -> AmberdataConfig) where
  parseJSON = withObject "AmberdataConfig" $ \o -> id
    <$< ambredataConfigHost ..: "host" % o
    <*< amberdataApiKey ..: "apiKey" % o
    <*< amberdataBlockchainId ..: "blockchainId" % o
    <*< amberdataChainId ..: "chainId" % o
    <*< amberdataDebug ..: "debug" % o


-- -------------------------------------------------------------------------- --
-- Monitor

amberdataBlockMonitor :: (PayloadCas cas, Logger logger) => Maybe ChainId -> logger -> CutDb cas -> IO ()
amberdataBlockMonitor cid logger db = do
    logFunctionText logger Info "Initialized Amberdata Block Monitor"
    case cid of
      Nothing -> logFunctionText logger Info "Sending blocks from ALL chains"
      Just cid' -> logFunctionText logger Info ("Sending blocks from chain " <> toText cid')
    void
        $ S.mapM_ logBlocks
        $ blockStream db
        & S.filter (\x -> cid == Just (_chainId x)
                          || cid == Nothing)
  where
    logBlocks :: BlockHeader -> IO ()
    logBlocks bheader = do
      amberdataBlock <- blockHeaderToAmberdataBlock bheader
      logFunctionJson logger Info amberdataBlock

    blockHeaderToAmberdataBlock :: BlockHeader -> IO AmberdataBlock
    blockHeaderToAmberdataBlock bh = do
      bpayload <- getBlockPayload bh
      return $ AmberdataBlock
        { _amberdataNumber = blockHeightFunction bh
        , _amberdataHash = _blockHash bh
        , _amberdataTimestamp = _blockCreationTime bh
        , _amberdataParentHash = _blockParent bh
        , _amberdataNonce = _blockNonce bh
        , _amberdataSize = getPayloadSize bpayload
        , _amberdataNumTransactions = getPayloadNumTransaction bpayload
        , _amberdataMeta = _blockChainId bh
        , _amberdataDifficulty = _blockWeight bh
        }

    -- | Gives unique block height when sending all blocks.
    --   Otherwise, block height not changed when sending blocks from specific chain.
    blockHeightFunction :: BlockHeader -> BlockHeight
    blockHeightFunction = case cid of
      Nothing -> uniqueBlockHeight
      Just _ -> _blockHeight

    uniqueBlockHeight :: BlockHeader -> BlockHeight
    uniqueBlockHeight bheader =
        BlockHeight $ (h * (fromIntegral totalChains)) + (chainIdInt (_chainId bcid))
      where
        BlockHeight h = _blockHeight bheader
        bcid = _blockChainId bheader
        totalChains = length $ chainIds bheader

    payloadCas = _webBlockPayloadStoreCas $ view cutDbPayloadStore db

    getBlockPayload :: BlockHeader -> IO PayloadWithOutputs
    getBlockPayload bheader = (casLookupM payloadCas . _blockPayloadHash) bheader

    getPayloadNumTransaction :: PayloadWithOutputs -> Word
    getPayloadNumTransaction = fromIntegral . length . _payloadWithOutputsTransactions

    getPayloadSize :: PayloadWithOutputs -> Word
    getPayloadSize payload = int $
      HM.foldl' (\acc (Transaction bs, _) -> acc + BS.length bs) 0 $
                    _payloadWithOutputsTransactions payload

-- -------------------------------------------------------------------------- --
-- Amberdata Backend

amberDataBatchSize :: Natural
amberDataBatchSize = 5

amberDataBatchDelayMs :: Natural
amberDataBatchDelayMs = 10000

-- | A backend for JSON log messages that sends all logs to the Amberdata /blocks
-- endpoint.
-- Messages are sent in a fire-and-forget fashion. If a connection fails, the
-- messages are dropped without notice.
--
withAmberDataBlocksBackend
    :: HTTP.Manager
    -> AmberdataConfig
    -> (Backend (JsonLog AmberdataBlock) -> IO b)
    -> IO b
withAmberDataBlocksBackend mgr conf inner = do
    queue <- newTBQueueIO 2000
    withAsync (runForever errorLogFun "Utils.Logging.withAmberdataBackend" (processor queue)) $ \_ -> do
        inner $ \a -> atomically (writeTBQueue queue a)

  where
    (AmberdataConfig esServer (AmberdataApiKey api) (AmberdataBlockchainId bid) _ doDebug) = conf

    errorLogFun Error msg = T.hPutStrLn stderr msg
    -- Print debug statements only if debugging is turned on.
    errorLogFun Debug msg = bool (return ()) (T.hPutStrLn stdout msg) doDebug
    errorLogFun _ _ = return ()

    -- Collect messages. If there is at least one pending message, submit a
    -- `blocks` request every second or when the batch size is {amberDataBatchSize} messages,
    -- whatever happens first.
    --
    processor queue = do
        -- ensure that there is at least one transaction in every batch
        h <- atomically $ readTBQueue queue

        -- set timer to 1 second
        timer <- registerDelay (int amberDataBatchDelayMs)

        -- Fill the batch
        (remaining, batch) <- go amberDataBatchSize (initIndex h) timer

        errorLogFun Debug $ "[Amberdata] Send " <> sshow (amberDataBatchSize - remaining) <> " messages"
        let body = BB.toLazyByteString (mkList batch)
        resp <- HTTP.httpLbs (putBulkLog body) mgr
        errorLogFun Debug $ "[Amberdata] Request Body: " <> sshow body
        errorLogFun Debug $ "[Amberdata] Response: " <> sshow (HTTP.responseBody resp)

      where
        getNextAction timer = atomically $ isTimeout `orElse` fill
          where
            isTimeout = Nothing <$ (readTVar timer >>= check)
            fill = tryReadTBQueue queue >>= maybe retry (return . Just)

        go 0 !batch _ = return $! (0, batch)
        go !remaining !batch !timer = getNextAction timer >>= \case
            Nothing -> return (remaining, batch)
            Just x -> go (remaining - 1) (batch <> indexWithComma x) timer

    putBulkLog a = HTTP.defaultRequest
        { HTTP.method = "POST"
        , HTTP.host = hostnameBytes (_hostAddressHost esServer)
        , HTTP.port = int (_hostAddressPort esServer)
        , HTTP.secure = True
        , HTTP.path = "/api/v1/blocks"
        , HTTP.responseTimeout = HTTP.responseTimeoutMicro 10000000
        , HTTP.requestHeaders =
            [ ("content-type", "application/json")
            , ("accept", "application/json")
            , ("x-amberdata-api-key", T.encodeUtf8 api)
            , ("x-amberdata-blockchain-id", T.encodeUtf8 bid)
            ]
        , HTTP.requestBody = HTTP.RequestBodyLBS a
        }

    e = fromEncoding . toEncoding
    initIndex (L.LogMessage a _ _ _)
        = BB.char7 ' ' <> e a
    indexWithComma (L.LogMessage a _ _ _)
        = BB.char7 ',' <> e a
    mkList a
         = BB.char7 '[' <> a <> BB.char7 ']'
