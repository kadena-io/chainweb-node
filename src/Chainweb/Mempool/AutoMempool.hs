{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Chainweb.Mempool.AutoMempool

  where

import Control.Concurrent
import Control.Monad.Catch
import Data.Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.HashMap.Strict as HM
import Data.String
import Data.Text (Text,pack)
import Data.Word (Word64)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Yaml as Y

import Chainweb.ChainId
import Chainweb.HostAddress
import Chainweb.Mempool.Mempool
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.Version

import Pact.ApiReq
import Pact.Types.Gas
import Pact.Parse
import Pact.Types.Command
import Pact.Types.ChainMeta
import Pact.Types.RPC


data AutoMempoolState = AutoMempoolState
  { _amsLastBlockTime :: Time Micros
  , _amsTxCount :: Word64
  } deriving (Show)

initAMState :: IO (MVar AutoMempoolState)
initAMState = newMVar $ AutoMempoolState epoch 0

data AutoMempoolConf = AutoMempoolConf
  { _amcTxPerSecond :: Word64
  -- ^ Rate for transactions to be _offered_ to new blocks.
  , _amcMaxTxPerBlock :: Word64
  -- ^ Max txs in a block
  , _amcGasPrice :: GasPrice
  , _amcSenderKeyPair :: ApiKeyPair
  , _amcSender :: Text
  , _amcGasLimit :: GasLimit
  , _amcCmd :: Text
  , _amcData :: Value
  } deriving (Show)

data AutoMempool = AutoMempool
  { _amConf :: AutoMempoolConf
  , _amChainId :: ChainId
  , _amNodeId :: Text
  , _amState :: MVar AutoMempoolState
  }

makeAutoNodeId :: HostAddress -> ChainId -> Text
makeAutoNodeId h c = pack $ show h ++ ":" ++ show c

-- | First arg is unique node id, most likely peer host addy
autoMempool
  :: AutoMempool
  -> IO (MempoolBackend ChainwebTransaction)
autoMempool am = noopMempool >>= \m -> return $
  m { mempoolGetBlock = \_ -> autoGetBlock am }


autoGetBlock :: AutoMempool -> IO (Vector ChainwebTransaction)
autoGetBlock am@AutoMempool{..} = do
  AutoMempoolState{..} <- takeMVar _amState
  currentTime <- getCurrentTimeIntegral
  let AutoMempoolConf{..} = _amConf
      (TimeSpan msSinceLast) = diff currentTime _amsLastBlockTime
      txCount
        | msSinceLast <= 0 = 0
        | otherwise = min _amcMaxTxPerBlock $
                      (fromIntegral msSinceLast * _amcTxPerSecond) `div` mega
      newTxCount = _amsTxCount + txCount
  putMVar _amState $ AutoMempoolState currentTime newTxCount
  (`V.unfoldrM` _amsTxCount) $ \txid -> if txid >= newTxCount
    then return Nothing
    else do
      t <- genTx am txid
      return $ Just (t, succ txid)


genTx :: AutoMempool -> Word64 -> IO ChainwebTransaction
genTx AutoMempool{..} txid = do
  let AutoMempoolConf{..} = _amConf
      msg = Exec $ ExecMsg _amcCmd _amcData
  kps <- mkKeyPairs [_amcSenderKeyPair]
  let meta = PublicMeta (fromString (show _amChainId)) _amcSender _amcGasLimit _amcGasPrice
      nonce = pack $ show _amNodeId ++ ":" ++ show _amChainId ++ ":" ++ show txid
  cmd <- mkCommand kps meta nonce msg
  let toPWT t bs = PayloadWithText bs (_cmdPayload t)
  case verifyCommand cmd of
    ProcSucc t -> return $ fmap (toPWT t) cmd
    ProcFail e -> throwM $ userError $ "genTx: failure verifying command " ++ show (cmd,e)


-- Repl Testing
_setup :: IO AutoMempool
_setup = do
  (senders :: HM.HashMap Text ApiKeyPair) <-
    either (fail.show) return =<< Y.decodeFileEither "pact/genesis/testnet00/keys.yaml"
  let lkp k = maybe (fail "didn't find sender02 keys") return $ HM.lookup k senders
      s02 = "sender02"
  s02kp <- lkp s02
  let conf = AutoMempoolConf
        { _amcTxPerSecond = 1
        , _amcMaxTxPerBlock = 10
        , _amcGasPrice = 0.0001
        , _amcSenderKeyPair = s02kp
        , _amcSender = s02
        , _amcGasLimit = 100
        , _amcCmd = "(coin.transfer (read-msg 'from) (read-msg 'to) (read-keyset 'addy) (read-decimal 'amount))"
        , _amcData = object
          [ "from" .= s02
          , "to" .= ("sender03" :: Text)
          , "addy" .= ["43f2adb1de192000cb3777bacc7f983b6614fd9c1715cd44cd484b6d3a0d34c8" :: Text]
          , "amount" .= (0.000001 :: ParsedDecimal)
          ]
        }
      cid = someChainId Testnet00
  AutoMempool conf cid "node" <$> initAMState

-- use this to test in repl as the output of `autoGetBlock` is huge
_go :: AutoMempool -> IO ()
_go a = autoGetBlock a >>= BS8.putStrLn . Y.encode
