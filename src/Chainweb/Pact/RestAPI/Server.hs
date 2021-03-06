{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Pact.RestAPI.Server
( PactServerData
, PactServerData_
, PactCmdLog(..)
, SomePactServerData(..)
, somePactServerData
, pactServer
, somePactServer
, somePactServers
, validateCommand
) where

import Control.Applicative
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TVar
import Control.DeepSeq
import Control.Lens (set, view, preview, (^?!), _head)
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe

import Data.Aeson as Aeson
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Short as SB
import Data.CAS
import Data.Default (def)
import Data.Foldable
import Data.Function
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import Data.Maybe
import Data.Singletons
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Tuple.Strict
import Data.Vector (Vector)
import qualified Data.Vector as V

import Ethereum.Block
import Ethereum.Header
import Ethereum.Misc (bytes)
import Ethereum.Receipt
import Ethereum.Receipt.ReceiptProof
import Ethereum.RLP (putRlpByteString)

import GHC.Generics
import GHC.Stack

import Prelude hiding (init, lookup)

import Servant

import qualified Streaming.Prelude as S

import System.LogLevel

-- internal modules

import Pact.Types.API
import qualified Pact.Types.ChainId as Pact
import Pact.Types.Command
import Pact.Types.Hash (Hash(..))
import qualified Pact.Types.Hash as Pact
import Pact.Types.PactError (PactError(..), PactErrorType(..))
import Pact.Types.Pretty (pretty)

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Chainweb.ChainResources
import Chainweb.Chainweb.CutResources
import Chainweb.Cut
import qualified Chainweb.CutDB as CutDB
import Chainweb.Logger
import Chainweb.Mempool.Mempool
    (InsertError(..), InsertType(..), MempoolBackend(..), TransactionHash(..))
import Chainweb.Pact.RestAPI
import Chainweb.Pact.RestAPI.EthSpv
import Chainweb.Pact.Service.Types
import Chainweb.Pact.SPV
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.SPV (SpvException(..))
import Chainweb.SPV.CreateProof
import Chainweb.Transaction (ChainwebTransaction, mkPayloadWithText)
import qualified Chainweb.TreeDB as TreeDB
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebPactExecutionService

------------------------------------------------------------------------------
type PactServerData logger cas =
    (CutResources logger cas, ChainResources logger)

newtype PactServerData_ (v :: ChainwebVersionT) (c :: ChainIdT) logger cas
    = PactServerData_ { _unPactServerData :: PactServerData logger cas }

data SomePactServerData = forall v c logger cas
    . (KnownChainwebVersionSymbol v,
       KnownChainIdSymbol c,
       PayloadCasLookup cas,
       Logger logger)
    => SomePactServerData (PactServerData_ v c logger cas)


somePactServerData
    :: PayloadCasLookup cas
    => Logger logger
    => ChainwebVersion
    -> ChainId
    -> PactServerData logger cas
    -> SomePactServerData
somePactServerData v cid db =
    case someChainwebVersionVal v of
      (SomeChainwebVersionT (Proxy :: Proxy vt)) ->
          case someChainIdVal cid of
              (SomeChainIdT (Proxy :: Proxy cidt)) ->
                  SomePactServerData (PactServerData_ @vt @cidt db)


pactServer
    :: forall v c cas logger
     . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => PayloadCasLookup cas
    => Logger logger
    => PactServerData logger cas
    -> Server (PactServiceApi v c)
pactServer (cut, chain) =
    pactApiHandlers
        :<|> pactSpvHandler
        :<|> ethSpvHandler
  where
    cid = FromSing (SChainId :: Sing c)
    mempool = _chainResMempool chain
    logger = _chainResLogger chain
    pact = _chainResPact chain
    cdb = _cutResCutDb cut

    pactApiHandlers
      = sendHandler logger mempool
      :<|> pollHandler logger cdb cid pact mempool
      :<|> listenHandler logger cdb cid pact mempool
      :<|> localHandler logger pact

    pactSpvHandler = spvHandler logger cdb cid pact


somePactServer :: SomePactServerData -> SomeServer
somePactServer (SomePactServerData (db :: PactServerData_ v c logger cas))
    = SomeServer (Proxy @(PactServiceApi v c)) (pactServer @v @c $ _unPactServerData db)


somePactServers
    :: PayloadCasLookup cas
    => Logger logger
    => ChainwebVersion
    -> [(ChainId, PactServerData logger cas)]
    -> SomeServer
somePactServers v =
    mconcat . fmap (somePactServer . uncurry (somePactServerData v))

data PactCmdLog
  = PactCmdLogSend (NonEmpty (Command Text))
  | PactCmdLogPoll (NonEmpty Text)
  | PactCmdLogListen Text
  | PactCmdLogLocal (Command Text)
  | PactCmdLogSpv Text
  deriving (Show, Generic, ToJSON, NFData)

sendHandler
    :: Logger logger
    => logger
    -> MempoolBackend ChainwebTransaction
    -> SubmitBatch
    -> Handler RequestKeys
sendHandler logger mempool (SubmitBatch cmds) = Handler $ do
    liftIO $ logg Info (PactCmdLogSend cmds)
    case traverse validateCommand cmds of
       Right enriched -> do
           let txs = V.fromList $ NEL.toList enriched
           -- If any of the txs in the batch fail validation, we reject them all.
           liftIO (mempoolInsertCheck mempool txs) >>= checkResult
           liftIO (mempoolInsert mempool UncheckedInsert txs)
           return $! RequestKeys $ NEL.map cmdToRequestKey enriched
       Left err -> failWith $ "Validation failed: " <> err
  where
    failWith :: String -> ExceptT ServerError IO a
    failWith err = throwError $ err400 { errBody = BSL8.pack err }

    logg = logFunctionJson (setComponent "send-handler" logger)

    toPactHash :: TransactionHash -> Pact.TypedHash h
    toPactHash (TransactionHash h) = Pact.TypedHash $ SB.fromShort h

    checkResult :: Either (T2 TransactionHash InsertError) () -> ExceptT ServerError IO ()
    checkResult (Right _) = pure ()
    checkResult (Left (T2 hash insErr)) =
        failWith $ concat [ "Validation failed for hash "
                          , show $ toPactHash hash
                          , ": "
                          , show insErr
                          ]

pollHandler
    :: HasCallStack
    => PayloadCasLookup cas
    => Logger logger
    => logger
    -> CutDB.CutDb cas
    -> ChainId
    -> PactExecutionService
    -> MempoolBackend ChainwebTransaction
    -> Poll
    -> Handler PollResponses
pollHandler logger cdb cid pact mem (Poll request) = do
    traverse_ validateRequestKey request

    liftIO $! logg Info $ PactCmdLogPoll $ fmap requestKeyToB16Text request
    -- get current best cut
    cut <- liftIO $! CutDB._cut cdb
    PollResponses <$!> liftIO (internalPoll pdb bdb mem pact cut request)
  where
    pdb = view CutDB.cutDbPayloadCas cdb
    bdb = fromJuste $ preview (CutDB.cutDbBlockHeaderDb cid) cdb
    logg = logFunctionJson (setComponent "poll-handler" logger)

listenHandler
    :: PayloadCasLookup cas
    => Logger logger
    => logger
    -> CutDB.CutDb cas
    -> ChainId
    -> PactExecutionService
    -> MempoolBackend ChainwebTransaction
    -> ListenerRequest
    -> Handler ListenResponse
listenHandler logger cdb cid pact mem (ListenerRequest key) = do
    validateRequestKey key

    liftIO $ logg Info $ PactCmdLogListen $ requestKeyToB16Text key
    liftIO (registerDelay defaultTimeout >>= runListen)
  where
    pdb = view CutDB.cutDbPayloadCas cdb
    bdb = fromJuste $ preview (CutDB.cutDbBlockHeaderDb cid) cdb
    logg = logFunctionJson (setComponent "listen-handler" logger)
    runListen :: TVar Bool -> IO ListenResponse
    runListen timedOut = go Nothing
      where
        go :: Maybe Cut -> IO ListenResponse
        go !prevCut = do
            m <- waitForNewCut prevCut
            case m of
                Nothing -> return $! ListenTimeout defaultTimeout
                (Just cut) -> poll cut

        poll :: Cut -> IO ListenResponse
        poll cut = do
            hm <- internalPoll pdb bdb mem pact cut (pure key)
            if HM.null hm
              then go (Just cut)
              else return $! ListenResponse $ snd $ head $ HM.toList hm

        waitForNewCut :: Maybe Cut -> IO (Maybe Cut)
        waitForNewCut lastCut = atomically $ do
             -- TODO: we should compute greatest common ancestor here to bound the
             -- search
             t <- readTVar timedOut
             if t
                 then return Nothing
                 else Just <$> do
                     !cut <- CutDB._cutStm cdb
                     when (lastCut == Just cut) retry
                     return cut

    -- TODO: make configurable
    defaultTimeout = 120 * 1000000 -- two minutes

localHandler
    :: Logger logger
    => logger
    -> PactExecutionService
    -> Command Text
    -> Handler (CommandResult Hash)
localHandler logger pact cmd = do
    liftIO $ logg Info $ PactCmdLogLocal cmd
    cmd' <- case validateCommand cmd of
      (Right !c) -> return c
      Left err ->
        throwError $ err400 { errBody = "Validation failed: " <> BSL8.pack err }
    r <- liftIO $ _pactLocal pact cmd'
    case r of
      Left err ->
        throwError $ err400 { errBody = "Execution failed: " <> BSL8.pack (show err) }
      (Right !r') -> return r'
  where
    logg = logFunctionJson (setComponent "local-handler" logger)


spvHandler
    :: forall cas l
    . ( Logger l
      , PayloadCasLookup cas
      )
    => l
    -> CutDB.CutDb cas
        -- ^ cut db
    -> ChainId
    -> PactExecutionService
        -- ^ pact execution service
    -> SpvRequest
        -- ^ Contains the (pact) chain id of the target chain id used in the
        -- 'target-chain' field of a cross-chain-transfer.
        -- Also contains the request key of of the cross-chain transfer
        -- tx request.
    -> Handler TransactionOutputProofB64
spvHandler l cdb cid pe (SpvRequest rk (Pact.ChainId ptid)) = do
    validateRequestKey rk

    liftIO $! logg (sshow ph)

    T2 bhe _bha <- liftIO (_pactLookup pe (NoRewind cid) (pure ph)) >>= \case
      Left e ->
        toErr $ "Internal error: transaction hash lookup failed: " <> sshow e
      Right v -> case v ^?! _head of
        Nothing -> toErr $ "Transaction hash not found: " <> sshow ph
        Just t -> return t

    idx <- liftIO (getTxIdx bdb pdb bhe ph) >>= \case
      Left e -> toErr
        $ "Internal error: Index lookup for hash failed: "
        <> sshow e
      Right i -> return i

    tid <- chainIdFromText ptid
    p <- liftIO (try $ createTransactionOutputProof cdb tid cid bhe idx) >>= \case
      Left e@SpvExceptionTargetNotReachable{} ->
        toErr $ "SPV target not reachable: " <> spvErrOf e
      Left e@SpvExceptionVerificationFailed{} ->
        toErr $ "SPV verification failed: " <> spvErrOf e
      Left e -> toErr $ "Internal error: SPV verification failed: " <> spvErrOf e
      Right q -> return q

    return $! b64 p
  where
    ph = Pact.fromUntypedHash $ unRequestKey rk
    pdb = view CutDB.cutDbPayloadCas cdb
    bdb = fromJuste $ preview (CutDB.cutDbBlockHeaderDb cid) cdb
    b64 = TransactionOutputProofB64
      . encodeB64UrlNoPaddingText
      . BSL8.toStrict
      . Aeson.encode

    logg = logFunctionJson (setComponent "spv-handler" l) Info
      . PactCmdLogSpv

    toErr e = throwError $ err400 { errBody = e }

    spvErrOf = BSL8.fromStrict
      . encodeUtf8
      . _spvExceptionMsg

ethSpvHandler
    :: EthSpvRequest
    -> Handler EthSpvResponse
ethSpvHandler req = do

    -- find block with transaction
    (block, rest) <- case evalState start Nothing of
        Left () -> toErr $ "the transaction " <> sshow tx <> " is not contained in any of the provided blocks"
        Right x -> return x

    -- select and order set of receipts in the block
    --
    -- How big can blocks be? Should we create an index instead?
    --
    rcs <- forM (_rpcBlockTransactions block) $ \t -> do
        case L.find (\r -> _rpcReceiptTransactionHash r == t) receipts of
            Nothing -> toErr $ "missing receipt for tx " <> sshow t
            Just x -> return x

    -- select and order set of extra headers and create proof
    case rpcReceiptProof (_rpcBlockHeader block) (hdrs block rest) rcs (TransactionIndex 28) of
        Left e -> toErr $ "failed to create proof: " <> sshow e
        Right proof -> return $ EthSpvResponse $
            encodeB64UrlNoPaddingText (putRlpByteString proof)

  where
    receipts = _ethSpvReqReceipts req
    blocks = _ethSpvReqBlocks req
    orderedBlocks = L.sortOn (_hdrNumber . _rpcBlockHeader) blocks
    tx = _ethSpvReqTransactionHash req

    start = S.each orderedBlocks
        & S.dropWhile (notElem tx . _rpcBlockTransactions)
        & S.next

    -- filter sequence consecutive headers
    hdrs block rest = flip evalState (Just $ _rpcBlockHash block) $ rest
        & S.filterM (\b -> do
            c <- get
            if Just (bytes $ _hdrParentHash $ _rpcBlockHeader b) == (bytes <$> c)
                then True <$ put (Just $ _rpcBlockHash b)
                else return False
        )
        & S.map _rpcBlockHeader
        & S.toList_

    toErr e = throwError $ err400 { errBody = e }

-- --------------------------------------------------------------------------- --

internalPoll
    :: PayloadCasLookup cas
    => PayloadDb cas
    -> BlockHeaderDb
    -> MempoolBackend ChainwebTransaction
    -> PactExecutionService
    -> Cut
    -> NonEmpty RequestKey
    -> IO (HashMap RequestKey (CommandResult Hash))
internalPoll pdb bhdb mempool pactEx cut requestKeys0 = do
    -- get leaf block header for our chain from current best cut
    chainLeaf <- lookupCutM cid cut
    results0 <- _pactLookup pactEx (DoRewind chainLeaf) requestKeys >>= either throwM return
        -- TODO: are we sure that all of these are raised locally. This will cause the
        -- server to shut down the connection without returning a result to the user.
    let results1 = V.zip requestKeysV results0
    let (present0, missing) = V.unstablePartition (isJust . snd) results1
    let present = V.map (second fromJuste) present0
    lookedUp <- (catMaybes . V.toList) <$> mapM lookup present
    badlisted <- V.toList <$> checkBadList (V.map fst missing)
    let outputs = lookedUp ++ badlisted
    return $! HM.fromList outputs
  where
    cid = _chainId bhdb
    !requestKeysV = V.fromList $ NEL.toList requestKeys0
    !requestKeys = V.map (Pact.fromUntypedHash . unRequestKey) requestKeysV

    lookup
        :: (RequestKey, T2 BlockHeight BlockHash)
        -> IO (Maybe (RequestKey, CommandResult Hash))
    lookup (key, T2 _ ha) = fmap (key,) <$> lookupRequestKey key ha

    -- TODO: group by block for performance (not very important right now)
    lookupRequestKey key bHash = runMaybeT $ do
        let keyHash = unRequestKey key
        let pactHash = Pact.fromUntypedHash keyHash
        let matchingHash = (== pactHash) . _cmdHash . fst
        blockHeader <- liftIO $ TreeDB.lookupM bhdb bHash
        let payloadHash = _blockPayloadHash blockHeader
        (PayloadWithOutputs txsBs _ _ _ _ _) <- MaybeT $ casLookup pdb payloadHash
        !txs <- mapM fromTx txsBs
        case find matchingHash txs of
            (Just (_cmd, (TransactionOutput output))) -> do
                out <- MaybeT $ return $! decodeStrict' output
                when (_crReqKey out /= key) $
                    fail "internal error: Transaction output doesn't match its hash!"
                enrichCR blockHeader out
            Nothing -> mzero

    fromTx (!tx, !out) = do
        !tx' <- MaybeT (return (toPactTx tx))
        return (tx', out)

    checkBadList :: Vector RequestKey -> IO (Vector (RequestKey, CommandResult Hash))
    checkBadList rkeys = do
        let thash = TransactionHash . SB.toShort . unHash . unRequestKey
        let !hashes = V.map thash rkeys
        out <- mempoolCheckBadList mempool hashes
        let bad = V.map (RequestKey . Hash . SB.fromShort . unTransactionHash . fst) $
                  V.filter snd $ V.zip hashes out
        return $! V.map hashIsOnBadList bad

    hashIsOnBadList :: RequestKey -> (RequestKey, CommandResult Hash)
    hashIsOnBadList rk =
        let res = PactResult (Left err)
            err = PactError TxFailure def [] doc
            doc = pretty (T.pack $ show InsertErrorBadlisted)
            !cr = CommandResult rk Nothing res 0 Nothing Nothing Nothing []
        in (rk, cr)

    enrichCR :: BlockHeader -> CommandResult Hash -> MaybeT IO (CommandResult Hash)
    enrichCR bh = return . set crMetaData
      (Just $ object
       [ "blockHeight" .= _blockHeight bh
       , "blockTime" .= _blockCreationTime bh
       , "blockHash" .= _blockHash bh
       , "prevBlockHash" .= _blockParent bh
       ])

toPactTx :: Transaction -> Maybe (Command Text)
toPactTx (Transaction b) = decodeStrict' b

validateCommand :: Command Text -> Either String ChainwebTransaction
validateCommand cmdText = case verifyCommand cmdBS of
    ProcSucc cmd -> Right (mkPayloadWithText <$> cmd)
    ProcFail err -> Left err
  where
    cmdBS :: Command ByteString
    cmdBS = encodeUtf8 <$> cmdText

-- | Validate the length of the request key's underlying hash.
--
validateRequestKey :: RequestKey -> Handler ()
validateRequestKey (RequestKey h'@(Hash h))
    | keyLength == blakeHashLength = return ()
    | otherwise = throwError err400
      { errBody = "Request Key "
        <> keyString
        <> " has incorrect hash of length "
        <> BSL8.pack (show keyLength)
      }
  where
    keyString = BSL8.pack $ T.unpack $ Pact.hashToText h'

    -- length of the encoded request key hash
    --
    keyLength = BS.length h

    -- Blake hash length = 32 - the length of a
    -- Blake2b_256 hash
    --
    blakeHashLength :: Int
    blakeHashLength = Pact.hashLength Pact.Blake2b_256
{-# INLINE validateRequestKey #-}
