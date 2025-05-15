{-# language NamedFieldPuns #-}
{-# language ImportQualifiedPost #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE DataKinds #-}

module Chainweb.Test.MockEVM
    ( jsonRpcExecuteRequestOnMockEVM
    , newMockEVM
    ) where

import Control.Concurrent.STM
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Aeson qualified as Aeson

import Chainweb.PayloadProvider.EVM.JsonRPC
import GHC.Exts (Symbol, coerce)
import Chainweb.PayloadProvider.EVM.EngineAPI qualified as EngineAPI
import Chainweb.PayloadProvider.EVM.EthRpcAPI qualified as EthRpcAPI
import Data.Text (Text)
import Chainweb.Utils (symbolVal_, fromJuste, int, sshow)
import Control.Monad.State
import Control.Monad
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Client as HTTP
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Text.Encoding as T
import Chainweb.PayloadProvider.EVM.EngineAPI (ForkchoiceUpdatedV1Response(ForkchoiceUpdateV1Response))
import Chainweb.BlockHeader
import Data.Proxy
import GHC.TypeLits
import Data.Type.Equality
import Data.Map (Map)
import Ethereum.Misc qualified as E
import qualified Chainweb.PayloadProvider.EVM.Utils as E
import Data.List.NonEmpty (NonEmpty)
import Chainweb.ChainId
import Chainweb.Version (HasVersion)
import qualified Chainweb.PayloadProvider.EVM.Genesis as Genesis
import qualified Chainweb.PayloadProvider.EVM.Header as EVM.Header
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Control.Lens
import qualified Data.ByteString.Short as BS
import qualified Ethereum.Transaction as E

data Finished = NotFinished | Finished
    deriving (Eq, Show)

data RequestLog = forall m. JsonRpcMethod m => RequestLog
    (Proxy m) (MethodRequest m) (MethodResponse m)

data MockEVM = MockEVM
    { mockEVMRequestLogChan :: TChan RequestLog
    , mockEVMChainwebChainId :: ChainId
    , mockEVMEthChainId :: E.ChainId
    , mockEVMForkChoiceState :: TVar EngineAPI.ForkchoiceStateV1
    , mockEVMPayloads :: TVar (Map EngineAPI.PayloadId E.BlockHash)
    , mockEVMParents :: TVar (Map BS.ShortByteString (E.ParentHash, E.BlockNumber))
    , mockEVMAuthToken :: Maybe T.Text
    }

newMockEVM :: HasVersion => ChainId -> E.ChainId -> Maybe T.Text -> STM MockEVM
newMockEVM chainwebChainId ethChainId authToken = do
    let gh = EVM.Header._hdrHash $ Genesis.genesisBlocks chainwebChainId
    MockEVM
        <$> newTChan
        <*> pure chainwebChainId
        <*> pure ethChainId
        <*> newTVar (EngineAPI.ForkchoiceStateV1 gh gh gh)
        <*> newTVar Map.empty
        <*> newTVar
            (Map.singleton (E._getBytesN $ coerce gh)
                (E.ParentHash (E.Keccak256Hash (E.replicateN 0)), E.BlockNumber 0))
        <*> pure authToken

jsonRpcExecuteRequestOnMockEVM
    :: HasVersion
    => MockEVM
    -> (forall m. JsonRpcMethod m => Proxy m -> MethodRequest m -> HTTP.ResponseTimeout -> HTTP.RequestHeaders -> IO (MethodResponse m))
jsonRpcExecuteRequestOnMockEVM
    MockEVM {..} (Proxy @m) body _timeout reqHeaders = do
        let authHeader = lookup "Authorization" reqHeaders
        case authHeader of
            Just auth
                | Just actualToken <- mockEVMAuthToken
                , auth /= T.encodeUtf8 actualToken ->
                    error "wrong auth token"
            _ -> do
                let ss = symbolSing @m
                if
                    | Just Refl <- testEquality ss (symbolSing @EngineAPI.Engine_ForkchoiceUpdatedV3) -> do
                        let requestState = EngineAPI._forkchoiceUpdatedV3RequestState body
                        let parentHash = EngineAPI._forkchoiceHeadBlockHash requestState
                        case EngineAPI._forkchoiceUpdatedV3RequestPayloadAttributes body of
                            Nothing ->
                                return $ EngineAPI.ForkchoiceUpdateV1Response
                                    { _forkchoiceUpdatedV1ResponsePayloadStatus = EngineAPI.PayloadStatusV1
                                        { _payloadStatusV1Status = EngineAPI.Valid
                                        , _payloadStatusV1LatestValidHash = Just parentHash
                                        , _payloadStatusV1ValidationError = Nothing
                                        }
                                    , _forkchoiceUpdatedV1ResponsePayloadId = Nothing
                                    }
                            Just payloadAttrs -> do
                                let parentBeaconBlockRoot = EngineAPI._payloadAttributesV3parentBeaconBlockRoot payloadAttrs
                                let withdrawals = EngineAPI._payloadAttributesV2Withdrawals $ EngineAPI._payloadAttributesV2 payloadAttrs
                                let EngineAPI.PayloadAttributesV1 {..} =
                                        EngineAPI._payloadAttributesV1 $ EngineAPI._payloadAttributesV2 payloadAttrs
                                let newBHash = E.BlockHash $ E.keccak256 $ mconcat
                                        [ sshow (1 :: Word, withdrawals)
                                        , sshow (2 :: Word, parentHash)
                                        , sshow (3 :: Word, parentBeaconBlockRoot)
                                        , sshow (4 :: Word, _payloadAttributesV1Timestamp)
                                        , sshow (5 :: Word, _payloadAttributesV1SuggestedFeeRecipient)
                                        , sshow (6 :: Word, _payloadAttributesV1PrevRandao)
                                        ]
                                atomically $ do
                                    modifyTVar mockEVMPayloads $ \m ->
                                        let latestPayloadId = maybe (EngineAPI.PayloadId $ E.encodeLeN 0) fst $ Map.lookupMax m
                                        in let succPayloadId =
                                                    EngineAPI.PayloadId $ E.unsafeBytesN $ E._getBytesN $ coerce $
                                                        E.keccak256 $ BS.fromShort $ E._getBytesN $ coerce latestPayloadId
                                        in Map.insert succPayloadId newBHash m
                                    modifyTVar mockEVMParents $ \m ->
                                        let (_, parentNumber) = fromJuste $ Map.lookup (E._getBytesN $ coerce parentHash) m
                                        in Map.insert (E._getBytesN $ coerce newBHash) (coerce parentHash, parentNumber) m
                                return $ EngineAPI.ForkchoiceUpdateV1Response
                                    { _forkchoiceUpdatedV1ResponsePayloadStatus = EngineAPI.PayloadStatusV1
                                        { _payloadStatusV1Status = EngineAPI.Valid
                                        , _payloadStatusV1LatestValidHash = Nothing
                                        , _payloadStatusV1ValidationError = Nothing
                                        }
                                    , _forkchoiceUpdatedV1ResponsePayloadId = Nothing
                                    }
                    | Just Refl <- testEquality ss (symbolSing @EngineAPI.Engine_GetPayloadV4) -> do
                        -- TODO: why is this a list?
                        let [payloadId :: EngineAPI.PayloadId] = body
                        payloadMap <- readTVarIO mockEVMPayloads
                        blockNumbers <- readTVarIO mockEVMParents
                        if
                            | Just newBlockHash <- Map.lookup payloadId payloadMap
                            , Just (parentHash, parentBlockNumber) <- Map.lookup (E._getBytesN $ coerce newBlockHash) blockNumbers
                            -> return EngineAPI.GetPayloadV4Response
                                { _getPayloadV4ResponseExecutionPayload = EngineAPI.ExecutionPayloadV3
                                    { _executionPayloadV2 = EngineAPI.ExecutionPayloadV2
                                        { _executionPayloadV1 = EngineAPI.ExecutionPayloadV1
                                            { _executionPayloadV1Transactions = []
                                            , _executionPayloadV1Timestamp = E.Timestamp 0
                                            , _executionPayloadV1StateRoot = E.StateRoot $ E.keccak256 ""
                                            , _executionPayloadV1ReceiptsRoot = E.ReceiptsRoot $ E.keccak256 ""
                                            , _executionPayloadV1PrevRandao = E.Randao (E.replicateN 0)
                                            , _executionPayloadV1ParentHash = parentHash
                                            , _executionPayloadV1LogsBloom = E.mkBloom []
                                            , _executionPayloadV1GasUsed = E.GasUsed 0
                                            , _executionPayloadV1GasLimit = E.GasLimit 0
                                            , _executionPayloadV1FeeRecipient = E.Beneficiary (E.Address (E.replicateN 0))
                                            , _executionPayloadV1ExtraData = E.ExtraData mempty
                                            , _executionPayloadV1BlockNumber = succ parentBlockNumber
                                            , _executionPayloadV1BlockHash =
                                                E.BlockHash $ E.keccak256 $ BS.fromShort $ E._getBytesN $ coerce parentHash
                                            , _executionPayloadV1BaseFeePerGas = EVM.Header.BaseFeePerGas 1
                                            }
                                        -- TODO: use withdrawals requested
                                        , _executionPayloadV2Withdrawals = []
                                        }
                                    , _executionPayloadV3BlobGasUsed = EVM.Header.BlobGasUsed 0
                                    , _executionPayloadV3ExcessBlobGas = EVM.Header.ExcessBlobGas 0
                                    }
                                    -- ^ executionPayload: ExecutionPayloadV2
                                , _getPayloadV4ResponseBlockValue = E.BlockValue (E.Wei 0)
                                , _getPayloadV4ResponseBlobsBundle = EngineAPI.BlobsBundleV1
                                    { _blobsBundleV1Commitments = []
                                    , _blobsBundleV1Proofs = []
                                    , _blobsBundleV1Blobs = []
                                    }
                                , _getPayloadV4ResponseShouldOverrideBuilder = True
                                , _getPayloadV4ResponseExecutionRequests = []
                                }
                            | otherwise ->
                                error "missing payload ID!"
                    | Just Refl <- testEquality ss (symbolSing @EthRpcAPI.Eth_GetBlockByNumber) -> do
                        let (requestedBlock, something) = body
                        when something $ error "something is True! Oh no!"
                        case requestedBlock of
                            E.DefaultBlockNumber 0 ->
                                return $ Just $ Genesis.genesisBlocks mockEVMChainwebChainId
                            E.DefaultBlockEarliest ->
                                return $ Just $ Genesis.genesisBlocks mockEVMChainwebChainId
                            _ -> error $ "unknown block! " <> sshow requestedBlock

                    | Just Refl <- testEquality ss (symbolSing @EthRpcAPI.Eth_ChainId) -> do
                        return mockEVMEthChainId
                    | otherwise -> error "unknown method"

-- expect
--     :: s
--     -> MockEVM
--     -> Expectation s
--     -> IO s
-- expect s MockEVM { mockEVMCurrentExpectations } expectation = do
--     atomically $ do
--         tryReadTMVar mockEVMCurrentExpectations >>= \case
--             Just (SomeExpectation _expectation _s finished) ->
--                 guard (finished == Finished)
--             Nothing -> return ()
--         writeTMVar mockEVMCurrentExpectations (SomeExpectation expectation s NotFinished)
--     atomically $ do
--         tryReadTMVar mockEVMCurrentExpectations >>= \case
--             Just (SomeExpectation _expectation s' Finished) ->
--                 return $ unsafeCoerce s'
--             _ -> retry

-- smokeTest = do
--     withBhdb \bhdb -> do
--     withCutDb bhdb \cutdb -> do
--     withMockEvm \evm -> do
--         miner <- mkMockMiner
--         hdr1_1 <- withConsensus cutdb bhdb evm \consensus -> do
--             expect fcu (tr genesisHeader) genesisHeader
--                 -- <return genesisHeader push payload>
--             hdr1_1 <- mine evm consensus
--             expect fcu (tr hdr1_1) hdr1_1
--                 -- <return new header push payload>
--         withConsensus cutdb bhdb evm \consensus -> do
--             expect fcu (tr hdr1_1) hdr1_1
--                 -- <return new header, push payload>
--         fcu (tr) genesisHeader
--             -- no payload
--         withConsensus cutdb bhdb evm \consensus -> do
--             expect fcu (tr hdr1_1) hdr1_1
--                 -- <return genesisHeader do not push payload>
--             expect fcu (tr genesisHeader, hdr1_1) hdr1_1
--                 -- <return hdr1_1, push payload>


