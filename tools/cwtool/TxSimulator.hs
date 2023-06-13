{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TxSimulator
  where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Crypto.Hash.Algorithms
import Data.Aeson (decodeStrict')
import Data.Default
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Options.Applicative
import System.LogLevel

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB.RestAPI.Client
import Chainweb.BlockHeaderDB.Internal
import Chainweb.BlockHeight
import Chainweb.Crypto.MerkleLog
import Chainweb.Logger
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.RelationalCheckpointer
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.PactService
import Chainweb.Pact.PactService.ExecBlock
import Chainweb.Pact.RestAPI.Server
import Chainweb.Pact.Service.Types
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.InMemory
import Chainweb.Payload.RestAPI.Client
import Chainweb.SPV
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Utils.Paging
import Chainweb.Version

import Network.Connection
import Network.HTTP.Client.TLS
import Servant.Client.Core
import Servant.Client

import Chainweb.Storage.Table.RocksDB

import Pact.Gas
import Pact.Interpreter
import Pact.Native
import Pact.Runtime.Utils
import Pact.Typechecker
import Pact.Types.Command
import Pact.Types.Hash
import Pact.Types.Info
import Pact.Types.Logger
import Pact.Types.Namespace
import Pact.Types.Persistence
import Pact.Types.Pretty
import Pact.Types.RPC
import Pact.Types.Runtime (runEval,keys,RefStore(..))
import Pact.Types.SPV
import Pact.Types.Term
import Pact.Types.Typecheck


import Utils.Logging.Trace

data SimConfig = SimConfig
    { scDbDir :: FilePath
      -- ^ db dir containing sqlite pact db files
    , scTxIndex :: Maybe Int
      -- ^ index in payload transactions list
    , scApiHostUrl :: BaseUrl
    , scRange :: (BlockHeight,BlockHeight)
    , scChain :: ChainId
    , scVersion :: ChainwebVersion
    , scGasLog :: Bool
    , scTypecheck :: Bool
    }

simulate :: SimConfig -> IO ()
simulate sc@(SimConfig dbDir txIdx' _ _ cid ver gasLog doTypecheck) = do
  cenv <- setupClient sc
  (parent:hdrs) <- fetchHeaders sc cenv
  pwos <- fetchOutputs sc cenv hdrs
  withSqliteDb cid cwLogger dbDir False $ \sqlenv -> do
    cpe@(CheckpointEnv cp _) <-
      initRelationalCheckpointer (initBlockState defaultModuleCacheLimit 0) sqlenv logger ver cid
    bracket_
      (_cpBeginCheckpointerBatch cp)
      (_cpDiscardCheckpointerBatch cp) $ case (txIdx',doTypecheck) of
        (Just txIdx,_) -> do -- single-tx simulation
          let PayloadWithOutputs txs md _ _ _ _ :: PayloadWithOutputs = head pwos
          miner <- decodeStrictOrThrow $ _minerData md
          let Transaction tx = fst $ txs V.! txIdx
          cmdTx <- decodeStrictOrThrow tx
          case validateCommand cmdTx of
            Left _ -> error "bad cmd"
            Right cmdPwt -> do
              let cmd = payloadObj <$> cmdPwt
                  txc = txContext parent cmd
              PactDbEnv' pde <-
                _cpRestore cp $ Just (succ (_blockHeight parent), _blockHash parent)
              mc <- readInitModules logger pde txc
              T3 !cr _mc _ <-
                trace (logFunction cwLogger) "applyCmd" () 1 $
                  applyCmd ver logger gasLogger pde miner (getGasModel txc)
                  txc noSPVSupport cmd (initGas cmdPwt) mc ApplySend
              T.putStrLn (encodeToText cr)
        (_,True) -> do
          PactDbEnv' pde <-
              _cpRestore cp $ Just (succ (_blockHeight parent), _blockHash parent)
          let refStore = RefStore nativeDefs
              pd = ctxToPublicData $ TxContext (ParentHeader parent) def
              loadMod = fmap inlineModuleData . getModule (def :: Info)
          ee <- setupEvalEnv pde Nothing Local (initMsgData pactInitialHash) refStore freeGasEnv
              permissiveNamespacePolicy noSPVSupport pd def
          void $ runEval def ee $ do
            mods <- keys def Modules
            coin <- loadMod "coin"
            let dynEnv = M.singleton "fungible-v2" coin
            forM mods $ \mn -> do
              md <- loadMod mn
              case _mdModule md of
                MDInterface _ -> return ()
                MDModule _ -> do
                  tcr :: Either CheckerException ([TopLevel Node],[Failure]) <-
                    try $ liftIO $ typecheckModule False dynEnv md
                  case tcr of
                    Left (CheckerException ei e) ->
                      liftIO $ putStrLn $ "TC_FAILURE: " ++ showPretty mn ++ ": "
                        ++ renderInfo ei ++ ": " ++ showPretty e
                    Right (_,[]) -> liftIO $ putStrLn $ "TC_SUCCESS: " ++ showPretty mn
                    Right (_,fails) ->
                      liftIO $ putStrLn $ "TC_FAILURE: " ++ showPretty mn ++ ": "
                      ++ "Unable to resolve all types: " ++ show (length fails) ++ " failures"


        (Nothing,False) -> do -- blocks simulation
          paydb <- newPayloadDb
          withRocksDb "txsim-rocksdb" modernDefaultOptions $ \rdb ->
            withBlockHeaderDb rdb ver cid $ \bdb -> do
              let pse = PactServiceEnv Nothing cpe paydb bdb getGasModel readRewards 100 0 ferr
                        ver True False logger gasLogger (pactLoggers cwLogger) False 1 defaultBlockGasLimit cid
                  pss = PactServiceState Nothing mempty (ParentHeader parent) noSPVSupport
              evalPactServiceM pss pse $ doBlock True parent (zip hdrs pwos)




  where

    cwLogger = genericLogger Debug T.putStrLn
    initGas cmd = initialGasOf (_cmdPayload cmd)
    logger = newLogger (pactLoggers cwLogger) "TxSimulator"
    gasLogger | gasLog = Just logger
              | otherwise = Nothing
    txContext parent cmd = TxContext (ParentHeader parent) $ publicMetaOf cmd
    ferr e _ = throwM e

    doBlock
        :: CanReadablePayloadCas cas
        => Bool
        -> BlockHeader
        -> [(BlockHeader,PayloadWithOutputs)]
        -> PactServiceM cas ()
    doBlock _ _ [] = return ()
    doBlock initMC parent ((hdr,pwo):rest) = do
      !cp <- getCheckpointer
      pde'@(PactDbEnv' pde) <-
          liftIO $ _cpRestore cp $ Just (succ (_blockHeight parent), _blockHash parent)
      when initMC $ do
        mc <- liftIO $ readInitModules logger pde (TxContext (ParentHeader parent) def)
        updateInitCache mc
      psParentHeader .= ParentHeader parent
      liftIO (spvSim sc hdr pwo) >>= assign psSpvSupport
      _r <- trace (logFunction cwLogger) "execBlock" () 1 $
          execBlock hdr (payloadWithOutputsToPayloadData pwo) pde'
      liftIO $ _cpSave cp (_blockHash hdr)
      doBlock False hdr rest

-- | Block-scoped SPV mock by matching cont proofs to payload txs.
-- Transactions are eliminated by searching for matching proof in input;
-- there should always be as many exact matches as proofs.
spvSim :: SimConfig -> BlockHeader -> PayloadWithOutputs -> IO SPVSupport
spvSim sc bh pwo = do
  mv <- newMVar (V.toList (_payloadWithOutputsTransactions pwo))
  return $ SPVSupport (_spvSupport noSPVSupport) (go mv)
  where
    go mv cp = modifyMVar mv $ searchOuts cp
    searchOuts _ [] = return ([],Left "spv: proof not found")
    searchOuts cp@(ContProof pf) ((Transaction ti,TransactionOutput _o):txs) =
      case codecDecode (chainwebPayloadCodec (Just (scVersion sc,_blockHeight bh))) ti of
        Left {} -> internalError "input decode failed"
        Right cmd -> case _pPayload $ payloadObj $ _cmdPayload cmd of
          Continuation cm | _cmProof cm == Just cp -> do
            -- the following adapted from Chainweb.Pact.SPV.verifyCont with matching errors
            t <- decodeB64UrlNoPaddingText $ T.decodeUtf8 pf
            case decodeStrict' t of
              Nothing -> internalError "unable to decode continuation proof"
              Just (TransactionOutputProof pcid p :: TransactionOutputProof SHA512t_256) -> do
                unless (pcid == scChain sc) $
                  internalError "cannot redeem continuation proof on wrong target chain"
                TransactionOutput tout <- proofSubject p
                case decodeStrict' tout :: Maybe (CommandResult Hash) of
                  Nothing -> internalError "unable to decode spv transaction output"
                  Just cro -> case _crContinuation cro of
                    Nothing -> return (txs,Left "no pact exec found in command result")
                    Just pe -> return (txs,Right pe)
          _ -> searchOuts cp txs

setupClient :: SimConfig -> IO ClientEnv
setupClient sc = flip mkClientEnv (scApiHostUrl sc) <$> newTlsManagerWith mgrSettings
  where
    mgrSettings = mkManagerSettings
        (TLSSettingsSimple True False False)
        Nothing

-- | note, fetches [low - 1, hi] to have parent headers
fetchHeaders :: SimConfig -> ClientEnv -> IO [BlockHeader]
fetchHeaders sc cenv = do
  r <- (`runClientM` cenv) $
      headersClient (scVersion sc) (scChain sc) Nothing Nothing
      (Just $ fromIntegral $ pred $ fst $ scRange sc)
      (Just $ fromIntegral $ snd $ scRange sc)
  case r of
    Left e -> throwM e
    Right p -> return $! _pageItems p

fetchOutputs :: SimConfig -> ClientEnv -> [BlockHeader] -> IO [PayloadWithOutputs]
fetchOutputs sc cenv bhs = do
  r <- (`runClientM` cenv) $ do
    outputsBatchClient (scVersion sc) (scChain sc) (map _blockPayloadHash bhs)
  case r of
    Left e -> throwM e
    Right ps -> return ps

simulateMain :: IO ()
simulateMain = do
  execParser opts >>= \(d,s,e,i,h,c,v,g,r) -> do
    vv <- chainwebVersionFromText (T.pack v)
    cc <- chainIdFromText (T.pack c)
    u <- parseBaseUrl h
    let rng = (fromIntegral @Integer s,fromIntegral @Integer (fromMaybe s e))
    simulate $ SimConfig d i u rng cc vv g r
  where
    opts = info (parser <**> helper)
        (fullDesc <> progDesc "Single Transaction simulator")
    parser = (,,,,,,,,)
        <$> strOption
             (short 'd'
              <> metavar "DBDIR"
              <> help "Pact database directory")
        <*> option auto
             (short 's'
              <> metavar "START_BLOCK_HEIGHT"
              <> help "Starting block height")
        <*> optional (option auto
             (short 'e'
              <> metavar "END_BLOCK_HEIGHT"
              <> help "Ending block height, if running more than one block"))
        <*> optional (option auto
             (short 'i'
              <> metavar "INDEX"
              <> help "Transaction index in payload list. If provided, only runs first block with this tx."))
        <*> (fromMaybe "api.chainweb.com" <$> optional (strOption
             (short 'h'
              <> metavar "API_HOST"
              <> help "API host, default is api.chainweb.com")))
        <*> (strOption
             (short 'c'
              <> metavar "CHAIN"
              <> help "Chain ID"))
        <*> (fromMaybe (show Mainnet01) <$> optional (strOption
             (short 'v'
              <> metavar "VERSION"
              <> help ("Chainweb version, default is "
                       ++ show Mainnet01))))
        <*> switch
             (short 'g'
              <> help "Enable gas logging")
        <*> switch
             (short 't'
              <> help "Typecheck modules")
