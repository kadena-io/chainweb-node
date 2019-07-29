{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Logging.Amberdata
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Data structures and functions for logging 'BlocHeader' data to Amberdata
--
module Chainweb.Logging.Amberdata
( AmberdataBlock(..)
, AmberdataConfig(..)
, amberdataBlockMonitor
, withAmberDataBlocksBackend
) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Monad
import Control.Monad.Catch

import Data.Aeson hiding (Error)
import Data.Bool
import qualified Data.ByteString.Builder as BB
import qualified Data.CaseInsensitive as CI
import qualified Data.Foldable as HM
import Data.String
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

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.HostAddress
import Chainweb.Logger
import Chainweb.NodeId
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
  , _amberdataMiner :: {-# UNPACK #-} !ChainNodeId
  , _amberdataSize :: !(Maybe Word)   -- ^ Bytes
  , _amberdataNumTransactions :: !(Maybe Word)
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
        , "miner" .= _amberdataMiner o
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
data AmberdataConfig = AmberdataConfig
    { _ambredataConfigHost :: HostAddress
    , _amberdataApiKey :: T.Text
    , _amberdataBlockchainId :: T.Text
    , _amberdataDebug :: Bool
    }
    deriving (Show, Eq, Ord, Generic)

amberdataConfigFromText :: MonadThrow m => T.Text -> m AmberdataConfig
amberdataConfigFromText x
  | CI.mk (T.take (T.length hostPrefix) x) == CI.mk hostPrefix =
      case T.splitOn "::" x of
        [hostStr, api, bid, "debug"] -> formatConfig hostStr api bid True
        [hostStr, api, bid] -> formatConfig hostStr api bid False
        _ -> configFromTextErr $ CI.mk x
  | otherwise = configFromTextErr $ CI.mk x

  where
    hostPrefix = "amberdata:"

    configFromTextErr e =
        throwM $ DecodeException $ "unexpected logger handle value: "
            <> fromString (show e)
            <> ", expected \"amberdata:<HOST>:<PORT>::<APIKEY>::<BLOCKCHAINID>\""
            <> " or \"amberdata:<HOST>:<PORT>::<APIKEY>::<BLOCKCHAINID>::debug\""

    formatConfig hostStr api bid doDebug = do
        hostAddr <- fromText $ T.drop (T.length hostPrefix) hostStr
        return $ AmberdataConfig hostAddr api bid doDebug

amberdataConfigToText :: AmberdataConfig -> T.Text
amberdataConfigToText (AmberdataConfig serv api bid debugResp)
    = "amberdata:" <> toText serv
        <> "::" <> api
        <> "::" <> bid
        <> bool "" ("::" <> "debug") debugResp

instance HasTextRepresentation AmberdataConfig where
    toText = amberdataConfigToText
    fromText = amberdataConfigFromText

    {-# INLINE toText #-}
    {-# INLINE fromText #-}

instance ToJSON AmberdataConfig where
    toJSON = String . amberdataConfigToText

instance FromJSON AmberdataConfig where
    parseJSON = parseJsonFromText "AmberdataConfig"


-- -------------------------------------------------------------------------- --
-- Monitor

amberdataBlockMonitor :: Logger logger => logger -> CutDb cas -> IO ()
amberdataBlockMonitor logger db
    = L.withLoggerLabel ("component", "amberdata-block-monitor") logger $ \l -> do
        go l `catchAllSynchronous` \e ->
            logFunctionText l Error ("Amberdata Block Monitor failed: " <> sshow e)
        logFunctionText l Info "Stopped Amberdata Block Monitor"
  where
    go l = do
        logFunctionText l Info "Initialized Amberdata Block Monitor"
        void
            $ S.mapM_ (logAllBlocks l)
            $ S.map cutToAmberdataBlocks
            $ cutStream db

    logAllBlocks :: Logger logger => logger -> [AmberdataBlock] -> IO ()
    logAllBlocks l = mapM_ (logFunctionJson l Info)

    cutToAmberdataBlocks :: Cut -> [AmberdataBlock]
    cutToAmberdataBlocks c =
        let totalChains = length (_cutMap c)
        in flip fmap (HM.toList $ _cutMap c) $ \bh -> AmberdataBlock
            { _amberdataNumber = uniqueBlockHeight bh totalChains
            , _amberdataHash = _blockHash bh
            , _amberdataTimestamp = _blockCreationTime bh
            , _amberdataParentHash = _blockParent bh
            , _amberdataNonce = _blockNonce bh
            , _amberdataMiner = _blockMiner bh
            , _amberdataSize = Nothing
            , _amberdataNumTransactions = Nothing
            , _amberdataMeta = _blockChainId bh
            , _amberdataDifficulty = _blockWeight bh
            }


    uniqueBlockHeight :: BlockHeader -> Int -> BlockHeight
    uniqueBlockHeight bheader totalChains =
        BlockHeight $ (h * (fromIntegral totalChains)) + (chainIdInt (_chainId cid))
      where
        BlockHeight h = _blockHeight bheader
        cid = _blockChainId bheader

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
withAmberDataBlocksBackend mgr (AmberdataConfig esServer api bid doDebug) inner = do
    queue <- newTBQueueIO 2000
    withAsync (runForever errorLogFun "Utils.Logging.withAmberdataBackend" (processor queue)) $ \_ -> do
        inner $ \a -> atomically (writeTBQueue queue a)

  where
    errorLogFun Error msg = T.hPutStrLn stderr msg
    -- Print debug statements only if debugging turned on
    errorLogFun Debug msg = bool (return ()) (T.hPutStrLn stdout msg) doDebug
    errorLogFun _ _ = return ()

    -- Collect messages. If there is at least one pending message, submit a
    -- `blocks` request every second or when the batch size is {amberDataBatchSize} messages,
    -- whatever happens first.
    --
    processor queue = do
        -- set timer to 1 second
        timer <- registerDelay (int amberDataBatchDelayMs)

        -- Fill the batch
        (remaining, batch) <- atomically $ do
            -- ensure that there is at least one transaction in every batch
            h <- readTBQueue queue
            go amberDataBatchSize (initIndex h) timer

        errorLogFun Debug $ "[Amberdata] Send " <> sshow (amberDataBatchSize - remaining) <> " messages"
        let body = BB.toLazyByteString (mkList batch)
        resp <- HTTP.httpLbs (putBulkLog body) mgr
        errorLogFun Debug $ "[Amberdata] Request Body: " <> sshow body
        errorLogFun Debug $ "[Amberdata] Response: " <> sshow (HTTP.responseBody resp)

      where
        go 0 !batch _ = return $! (0, batch)
        go !remaining !batch !timer = isTimeout `orElse` fill
          where
            isTimeout = do
                check =<< readTVar timer
                return $! (remaining, batch)
            fill = tryReadTBQueue queue >>= \case
                Nothing -> return $! (remaining, batch)
                Just x -> do
                    go (pred remaining) (batch <> indexWithComma x) timer

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

