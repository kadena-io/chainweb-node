{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module TXG.ReplInternals where

import Control.Lens
import Control.Monad
import Control.Monad.State

import Data.Aeson
import Data.Decimal
import Data.Default
import qualified Data.List.NonEmpty as NEL
import Data.Text (Text)
import qualified Data.Text as T

import Network.HTTP.Client hiding (Proxy(..))
import Network.HTTP.Client.TLS
import Network.X509.SelfSigned

import Servant.Client

import System.Random

import Text.Printf

-- pact imports

import Pact.ApiReq
import Pact.Types.API
import Pact.Types.ChainId (NetworkId(..))
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Hash

-- chainweb imports

import Chainweb.ChainId
import Chainweb.HostAddress
import Chainweb.Pact.RestAPI.Client
import Chainweb.Version

import TXG.Simulate.Contracts.CoinContract
import TXG.Simulate.Contracts.HelloWorld
import TXG.Simulate.Contracts.SimplePayments
import TXG.Simulate.Utils

-- for ghci

data Network = Network
  { networkVersion :: ChainwebVersion
  , networkHost :: HostAddress
  , networkChainId :: ChainId
  } deriving (Eq,Ord,Show)

send
    :: Network
    -> [Command Text]
    -> IO (Either ClientError RequestKeys)
send (Network v h cid) xs = do
    cenv <- genClientEnv h
    putStrLn $ "Sending: version=" ++ show v ++ ", cid=" ++ show cid ++ ", SubmitBatch=" ++ show (SubmitBatch(NEL.fromList xs))

    runClientM (pactSendApiClient v cid (SubmitBatch (NEL.fromList xs))) cenv

poll
    :: Network
    -> RequestKeys
    -> IO (Either ClientError PollResponses)
poll (Network v h cid) rkeys = do
    ce <- genClientEnv h
    runClientM (pactPollApiClient v cid . Poll $ _rkRequestKeys rkeys) ce

local
    :: Network
    -> Command Text
    -> IO (Either ClientError (CommandResult Hash))
local (Network v h cid) cmdText = do
    ce <- genClientEnv h
    runClientM (pactLocalApiClient v cid cmdText) ce

listen
    :: Network
    -> RequestKey
    -> IO (Either ClientError ListenResponse)
listen (Network v h cid) rk = do
    ce <- genClientEnv h
    runClientM (pactListenApiClient v cid (ListenerRequest rk)) ce

cmd
    :: String
    -- ^ Code
    -> Value
    -- ^ Env data
    -> PublicMeta
    -> [SomeKeyPairCaps]
    -> Maybe NetworkId
    -> Maybe String
    -- ^ Transaction nonce.  If Nothing, then getCurrentTime is used.
    -> IO (Command Text)
cmd = mkExec

cmdStr :: String -> IO (Command Text)
cmdStr str = cmd str Null defPubMeta [] Nothing Nothing

-- Structured transactions

data Builtin = Hello | Payments

type Account = String

type SenderName = String

type ReceiverName = String

type Balance = Decimal

type Amount = Double

data CallBuiltIn'
    = CC CoinContractRequest
    | SP SimplePaymentRequest (Maybe (NEL.NonEmpty SomeKeyPairCaps))
    | HelloCode Text

data TxContent
    = PactCode String
    | Define Builtin
    | CallBuiltin CallBuiltIn'

easyTxToCommand :: TxContent -> IO (Command Text)
easyTxToCommand txContent = do
    ks <- testSomeKeyPairs
    txToCommand defChainwebVersion defPubMeta ks txContent

txToCommand
    :: ChainwebVersion
    -> PublicMeta
    -> NEL.NonEmpty SomeKeyPairCaps
    -> TxContent
    -> IO (Command Text)
txToCommand v pubmeta ks = \case
    PactCode str -> cmdStr str
    Define Hello -> helloWorldContractLoader v pubmeta ks
    Define Payments -> simplePaymentsContractLoader v pubmeta ks
    CallBuiltin (CC coinReq) -> createCoinContractRequest v pubmeta ks coinReq
    CallBuiltin (SP spReq mkeyset) -> simplePayReq v pubmeta spReq mkeyset
    CallBuiltin (HelloCode helloname) -> helloRequest v $ Name helloname

defChainwebVersion :: ChainwebVersion
defChainwebVersion = Development

defChainId :: ChainId
defChainId = foldr const err $ chainIds defChainwebVersion
  where
    err = error "You shouldn't have a chainweb version with 0 chains"

defPubMeta :: PublicMeta
defPubMeta = def
    & set pmChainId "0"
    & set pmSender "sender00"
    & set pmGasLimit 1000
    & set pmGasPrice 0.0000001
    & set pmTTL 28800


generateDefaultSimpleCommands :: Int -> IO [Command Text]
generateDefaultSimpleCommands batchsize =
    replicateM batchsize $ getStdRandom (runState go) >>= cmdStr
  where
    go = do
        a <- state $ randomR (1, 100 :: Integer)
        b <- state $ randomR (1, 100 :: Integer)
        opIndex <- state $ randomR (0, 2 :: Int)
        return $ printf "(%s %u %u)" ["+-*" !! opIndex] a b

genClientEnv :: HostAddress -> IO ClientEnv
genClientEnv hostaddress = do
    mgrSettings <- certificateCacheManagerSettings TlsInsecure Nothing
    let timeout = responseTimeoutMicro $ 1000000 * 60 * 4
    mgr <- newTlsManagerWith $ mgrSettings { managerResponseTimeout = timeout }
    let url = BaseUrl Https
              (T.unpack . hostnameToText $ _hostAddressHost hostaddress)
              (fromIntegral $ _hostAddressPort hostaddress)
              ""
    pure $! mkClientEnv mgr url
