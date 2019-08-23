{-# LANGUAGE CPP                        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

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
import qualified Data.Text as T
import Data.Text (Text)

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
import Pact.Types.Command
import Pact.Types.ChainMeta
import Pact.Types.Crypto
import Pact.Types.Hash

-- chainweb imports

import Chainweb.ChainId
import Chainweb.HostAddress
import Chainweb.Version
import Chainweb.Pact.RestAPI
#if !MIN_VERSION_servant(0,15,0)
import Chainweb.RestAPI.Utils
#endif

import TXG.Simulate.Contracts.HelloWorld
import TXG.Simulate.Contracts.SimplePayments
import TXG.Simulate.Contracts.CoinContract
import TXG.Simulate.Utils

-- for ghci

primSend
    :: ChainwebVersion
    -> HostAddress
    -> ChainId
    -> [Command Text]
    -> IO (Either ClientError RequestKeys)
primSend v h cid xs = do
    cenv <- genClientEnv h
    runClientM (send v cid (SubmitBatch (NEL.fromList xs))) cenv

primPoll
    :: ChainwebVersion
    -> HostAddress
    -> ChainId
    -> RequestKeys
    -> IO (Either ClientError PollResponses)
primPoll v h cid rkeys = do
    ce <- genClientEnv h
    runClientM (poll v cid . Poll $ _rkRequestKeys rkeys) ce

primLocal
    :: ChainwebVersion
    -> HostAddress
    -> ChainId
    -> Command Text
    -> IO (Either ClientError (CommandResult Hash))
primLocal v h cid cmd = do
    ce <- genClientEnv h
    runClientM (local v cid cmd) ce

primCommand
    :: String
    -> Value
    -> PublicMeta
    -> [SomeKeyPair]
    -> Maybe String -> IO (Command Text)
primCommand = mkExec

easyCmd :: String -> IO (Command Text)
easyCmd str = primCommand str Null defPubMeta [] Nothing

-- Structured transactions

data Builtin = Hello | Payments

type Keyset = NEL.NonEmpty SomeKeyPair

type Guard = Keyset

type Account = String

type SenderName = String

type ReceiverName = String

type Balance = Decimal

type Amount = Decimal

data CallBuiltIn'
    = CC CoinContractRequest
    | SP SimplePaymentRequest (Maybe Keyset)
    | HelloCode Text

data TxContent
    = PactCode String
    | Define Builtin
    | CallBuiltin CallBuiltIn'

easyTxToCommand :: TxContent -> IO (Command Text)
easyTxToCommand txContent = do
    ks <- testSomeKeyPairs
    txToCommand defPubMeta ks txContent

txToCommand :: PublicMeta -> Keyset -> TxContent -> IO (Command Text)
txToCommand pubmeta ks = \case
    PactCode str -> easyCmd str
    Define Hello -> helloWorldContractLoader pubmeta ks
    Define Payments -> simplePaymentsContractLoader pubmeta ks
    CallBuiltin (CC coinReq) -> createCoinContractRequest pubmeta coinReq
    CallBuiltin (SP spReq mkeyset) -> simplePayReq pubmeta spReq mkeyset
    CallBuiltin (HelloCode helloname) -> helloRequest $ Name helloname

defChainwebVersion :: ChainwebVersion
defChainwebVersion = Development

defChainId :: ChainId
defChainId = foldr const err $ chainIds defChainwebVersion
  where
    err = error "You shouldn't have a chainweb version with 0 chains"

defPubMeta :: PublicMeta
defPubMeta = def
    & set pmChainId "0"
    & set pmSender "0"
    & set pmGasLimit 1000
    & set pmGasPrice 0.00000000001

api version chainid =
    case someChainwebVersionVal version of
      SomeChainwebVersionT (_ :: Proxy cv) ->
        case someChainIdVal chainid of
          SomeChainIdT (_ :: Proxy cid) ->
            client
              (Proxy :: Proxy (PactApi cv cid))

send :: ChainwebVersion -> ChainId -> SubmitBatch -> ClientM RequestKeys
send version chainid = go
  where
    go :<|> _ :<|> _ :<|> _ = api version chainid

poll
    :: ChainwebVersion
    -> ChainId
    -> Poll
    -> ClientM PollResponses
poll version chainid = go
  where
    _ :<|> go :<|> _ :<|> _ = api version chainid

listen
    :: ChainwebVersion
    -> ChainId
    -> ListenerRequest
    -> ClientM ListenResponse
listen version chainid = go
  where
    _ :<|> _ :<|> go :<|> _ = api version chainid

local
    :: ChainwebVersion
    -> ChainId
    -> Command Text
    -> ClientM (CommandResult Hash)
local version chainid = go
  where
    _ :<|> _ :<|> _ :<|> go = api version chainid

generateDefaultSimpleCommands :: Int -> IO [Command Text]
generateDefaultSimpleCommands batchsize =
    replicateM batchsize $ getStdRandom (runState go) >>= easyCmd
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
