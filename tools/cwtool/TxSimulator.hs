{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TxSimulator
  where

import Control.Monad.Catch
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Options.Applicative
import System.LogLevel

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB.RestAPI.Client
import Chainweb.BlockHeight
import Chainweb.Logger
import Chainweb.Pact.Backend.RelationalCheckpointer
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.PactService
import Chainweb.Pact.RestAPI.Server
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Payload.RestAPI.Client
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Utils.Paging
import Chainweb.Version

import Network.Connection
import Network.HTTP.Client.TLS
import Servant.Client.Core
import Servant.Client

import Pact.Types.Command
import Pact.Types.Logger
import Pact.Types.SPV

import Utils.Logging.Trace

data SimConfig = SimConfig
    { scDbDir :: FilePath
      -- ^ db dir containing sqlite pact db files
    , scTxIndex :: Int
      -- ^ index in payload transactions list
    , scApiHostUrl :: BaseUrl
    , scRange :: (BlockHeight,BlockHeight)
    , scChain :: ChainId
    , scVersion :: ChainwebVersion
    }

simulate :: SimConfig -> IO ()
simulate sc@(SimConfig dbDir txIdx _ _ cid ver) = do
  cenv <- setupClient sc
  (parent:hdr:_) <- fetchHeaders sc cenv
  [PayloadWithOutputs txs md _ _ _ _ :: PayloadWithOutputs] <- fetchOutputs sc cenv [hdr]
  let Transaction tx = fst $ txs V.! txIdx
  cmdTx <- decodeStrictOrThrow tx
  miner <- decodeStrictOrThrow $ _minerData md
  case validateCommand cmdTx of
    Left _ -> error "bad cmd"
    Right cmdPwt ->
      withSqliteDb cid cwLogger dbDir False $ \sqlenv -> do
        CheckpointEnv cp _ <-
          initRelationalCheckpointer (initBlockState 0) sqlenv logger ver cid
        bracket_
          (_cpBeginCheckpointerBatch cp)
          (_cpDiscardCheckpointerBatch cp) $ do
            let cmd = payloadObj <$> cmdPwt
                txc = txContext parent cmd
            PactDbEnv' pde <-
              _cpRestore cp $ Just (succ (_blockHeight parent), _blockHash parent)
            mc <- readInitModules logger pde txc
            (T2 !cr _mc) <-
              trace (logFunction cwLogger) "applyCmd" () 1 $
                applyCmd ver logger gasLogger pde miner (getGasModel txc)
                txc noSPVSupport cmd (initGas cmdPwt) mc
            T.putStrLn (encodeToText cr)


  where
    cwLogger = genericLogger Debug T.putStrLn
    initGas cmd = initialGasOf (_cmdPayload cmd)
    logger = newLogger (pactLoggers cwLogger) "TxSimulator"
    gasLogger = Nothing
    txContext parent cmd = TxContext (ParentHeader parent) $ publicMetaOf cmd


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
  execParser opts >>= \(d,s,e,i,h,c,v) -> do
    vv <- chainwebVersionFromText (T.pack v)
    cc <- chainIdFromText (T.pack c)
    u <- parseBaseUrl h
    let rng = (fromIntegral @Integer s,fromIntegral @Integer (fromMaybe s e))
    simulate $ SimConfig d i u rng cc vv
  where
    opts = info (parser <**> helper)
        (fullDesc <> progDesc "Single Transaction simulator")
    parser = (,,,,,,)
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
        <*> option auto
             (short 'i'
              <> metavar "INDEX"
              <> help "Transaction index in payload list")
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
