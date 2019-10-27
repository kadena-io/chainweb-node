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
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T

import Network.HTTP.Client hiding (Proxy(..))
import Network.HTTP.Client.TLS
import Network.X509.SelfSigned

import Servant.API
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
import Chainweb.Pact.RestAPI
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
    runClientM (sendClient v cid (SubmitBatch (NEL.fromList xs))) cenv

poll
    :: Network
    -> RequestKeys
    -> IO (Either ClientError PollResponses)
poll (Network v h cid) rkeys = do
    ce <- genClientEnv h
    runClientM (pollClient v cid . Poll $ _rkRequestKeys rkeys) ce

local
    :: Network
    -> Command Text
    -> IO (Either ClientError (CommandResult Hash))
local (Network v h cid) cmdText = do
    ce <- genClientEnv h
    runClientM (localClient v cid cmdText) ce

listen
    :: Network
    -> RequestKey
    -> IO (Either ClientError ListenResponse)
listen (Network v h cid) rk = do
    ce <- genClientEnv h
    runClientM (listenClient v cid (ListenerRequest rk)) ce

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
    & set pmGasPrice 0.00000000001
    & set pmTTL 3600

api version chainid =
    case someChainwebVersionVal version of
      SomeChainwebVersionT (_ :: Proxy cv) ->
        case someChainIdVal chainid of
          SomeChainIdT (_ :: Proxy cid) ->
            client
              (Proxy :: Proxy (PactApi cv cid))

sendClient :: ChainwebVersion -> ChainId -> SubmitBatch -> ClientM RequestKeys
sendClient version chainid = go
  where
    go :<|> _ :<|> _ :<|> _ = api version chainid

pollClient
    :: ChainwebVersion
    -> ChainId
    -> Poll
    -> ClientM PollResponses
pollClient version chainid = go
  where
    _ :<|> go :<|> _ :<|> _ = api version chainid

listenClient
    :: ChainwebVersion
    -> ChainId
    -> ListenerRequest
    -> ClientM ListenResponse
listenClient version chainid = go
  where
    _ :<|> _ :<|> go :<|> _ = api version chainid

localClient
    :: ChainwebVersion
    -> ChainId
    -> Command Text
    -> ClientM (CommandResult Hash)
localClient version chainid = go
  where
    _ :<|> _ :<|> _ :<|> go = api version chainid

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
