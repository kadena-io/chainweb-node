{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Chainweb.Pact.TxSimulator

  where

import Control.Monad.Catch
import qualified Data.Text.IO as T
import qualified Data.Vector as V
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
      -- curl 'https://api.chainweb.com/chainweb/0.0/mainnet01/chain/2/header?maxheight=3195192&minheight=3195192'
    , scPayload :: FilePath
      -- ^ payload file
      -- curl https://api.chainweb.com/chainweb/0.0/mainnet01/chain/2/payload/3uU_CZVy3ZynNOniLbSDykNdzO5pFYuKftuynax9eUo
    , scTxIndex :: Int
      -- ^ index in payload transactions list
    , scChain :: ChainId
    , scVersion :: ChainwebVersion
    }

_confMainnetChain2 :: SimConfig
_confMainnetChain2 = SimConfig
  "pactdbs"
  "header.json"
  "payload.json"
  0
  (unsafeChainId 2)
  Mainnet01

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
            ((Time (TimeSpan t0)) :: Time Micros) <- getCurrentTimeIntegral
            (T2 !cr _mc) <- applyCmd ver logger gasLogger pde miner chainweb213GasModel txc noSPVSupport cmd (initGas cmdPwt) mc
            (Time (TimeSpan t1)) <- getCurrentTimeIntegral
            print ("Elapsed micros" :: String,(t1-t0))
            T.putStrLn (encodeToText cr)


  where
    cwLogger = genericLogger Info T.putStrLn
    initGas cmd = initialGasOf (_cmdPayload cmd)
    logger = newLogger alwaysLog "TxSimulator"
    gasLogger = Nothing
    txContext parent cmd = TxContext (ParentHeader parent) $ publicMetaOf cmd
