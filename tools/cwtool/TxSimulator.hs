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
import Chainweb.Logger
import Chainweb.Pact.Backend.RelationalCheckpointer
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.PactService
import Chainweb.Pact.RestAPI.Server
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version

import Pact.Types.Command
import Pact.Types.Logger
import Pact.Types.SPV

data SimConfig = SimConfig
    { scDbDir :: FilePath
      -- ^ db dir containing sqlite pact db files
    , scParentBlock :: FilePath
      -- ^ block header file.
    , scPayload :: FilePath
      -- ^ payload file
    , scTxIndex :: Int
      -- ^ index in payload transactions list
    , scChain :: ChainId
    , scVersion :: ChainwebVersion
    }

simulate :: SimConfig -> IO ()
simulate (SimConfig dbDir parentBlockFile payloadFile txIdx cid ver) = do
  parent <- decodeFileStrictOrThrow parentBlockFile
  ((PayloadData txs md _ _ _) :: PayloadData) <- decodeFileStrictOrThrow payloadFile
  let (Transaction tx) = txs V.! txIdx
  cmdTx <- decodeStrictOrThrow tx
  miner <- decodeStrictOrThrow $ _minerData md
  case validateCommand cmdTx of
    Left _ -> error "bad cmd"
    Right cmdPwt ->
      withSqliteDb cid cwLogger dbDir False $ \sqlenv -> do
        (CheckpointEnv cp _) <- initRelationalCheckpointer (initBlockState 0) sqlenv logger ver cid
        bracket
          (_cpBeginCheckpointerBatch cp)
          (\_ -> _cpDiscardCheckpointerBatch cp) $ \_ -> do
            let cmd = payloadObj <$> cmdPwt
                txc = txContext parent cmd
            (PactDbEnv' pde) <- _cpRestore cp $ Just (succ (_blockHeight parent), _blockHash parent)
            mc <- readInitModules logger pde txc
            (t0 :: Time Micros) <- getCurrentTimeIntegral
            (T2 !cr _mc) <- applyCmd ver logger gasLogger pde miner chainweb213GasModel txc noSPVSupport cmd (initGas cmdPwt) mc
            t1 <- getCurrentTimeIntegral
            logLog logger "DEBUG" $ show ("Elapsed micros" :: String,diff t1 t0)
            T.putStrLn (encodeToText cr)


  where
    cwLogger = genericLogger Debug T.putStrLn
    initGas cmd = initialGasOf (_cmdPayload cmd)
    logger = newLogger (pactLoggers cwLogger) "TxSimulator"
    gasLogger = Nothing
    txContext parent cmd = TxContext (ParentHeader parent) $ publicMetaOf cmd


simulateMain :: IO ()
simulateMain = do
  execParser opts >>= \(d,h,p,i,c,v) -> do
    vv <- chainwebVersionFromText (T.pack v)
    cc <- chainIdFromText (T.pack c)
    simulate $ SimConfig d h p i cc vv
  where
    opts = info (parser <**> helper)
        (fullDesc <> progDesc "Single Transaction simulator")
    parser = (,,,,,)
        <$> strOption
             (short 'd'
              <> metavar "DBDIR"
              <> help "Pact database directory")
        <*> strOption
             (short 'h'
              <> metavar "PARENT_HEADER"
              <> help "Parent header, example: curl 'https://api.chainweb.com/chainweb/0.0/mainnet01/chain/2/header?maxheight=3195192&minheight=3195192' -s | jq '.items[0]'")
        <*> strOption
             (short 'p'
              <> metavar "PAYLOAD"
              <> help "Block payload, example: curl https://api.chainweb.com/chainweb/0.0/mainnet01/chain/2/payload/3uU_CZVy3ZynNOniLbSDykNdzO5pFYuKftuynax9eUo")
        <*> option auto
             (short 'i'
              <> metavar "INDEX"
              <> help "Transaction index in payload list")
        <*> (strOption
             (short 'c'
              <> metavar "CHAIN"
              <> help "Chain ID"))
        <*> (fromMaybe (show Mainnet01) <$> optional (strOption
             (short 'v'
              <> metavar "VERSION"
              <> help ("Chainweb version, default is "
                       ++ show Mainnet01))))
