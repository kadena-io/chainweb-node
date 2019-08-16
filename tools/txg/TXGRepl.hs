{-# LANGUAGE CPP                        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module TXGRepl where

import Control.Monad
import Control.Monad.State

import Data.Aeson
import Data.Decimal
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
import qualified Pact.Types.ChainId as Pact
import Pact.Types.Crypto

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

easyTxToCommand :: TxContent -> IO (Command Text)
easyTxToCommand txContent = do
  ks <- testSomeKeyPairs
  txToCommand defPubMeta ks txContent

txToCommand :: PublicMeta -> Keyset -> TxContent -> IO (Command Text)
txToCommand pubmeta ks = \case
  PactCode str -> easyCmd str
  Define Hello -> helloWorldContractLoader pubmeta ks
  Define Payments -> simplePaymentsContractLoader pubmeta ks
  CallBuiltin (CC coinReq) ->
    createCoinContractRequest pubmeta coinReq
  CallBuiltin (SP spReq mkeyset) ->
    simplePayReq pubmeta spReq mkeyset
  CallBuiltin (HelloCode helloname) ->
    helloRequest (Name helloname)

defChainwebVersion :: ChainwebVersion
defChainwebVersion = Development

defChainId :: ChainId
defChainId = foldr const err $ chainIds defChainwebVersion
  where
    err = error "You shouldn't have a chainweb version with 0 chains"

defHostAddressText :: Text
defHostAddressText = "us2.testnet.chainweb.com:443"

defPubMeta :: PublicMeta
defPubMeta = PublicMeta (Pact.ChainId "0") "sender00" 100 1.0

primSend
    :: HostAddress
    -> ChainwebVersion
    -> ChainId
    -> [Command Text]
    -> IO (Either ClientError RequestKeys)
primSend h v cid xs = do
    cenv <- genClientEnv h
    runClientM (send v cid (SubmitBatch (NEL.fromList xs))) cenv

easySend :: [Command Text] -> IO (Either ClientError RequestKeys)
easySend xs = do
  hostaddress <- hostAddressFromText defHostAddressText
  primSend hostaddress defChainwebVersion defChainId xs

easyCmd :: String -> IO (Command Text)
easyCmd str = primCommand str Null defPubMeta [] Nothing

primCommand
    :: String
    -> Value
    -> PublicMeta
    -> [SomeKeyPair]
    -> Maybe String -> IO (Command Text)
primCommand = mkExec

simplePoll :: RequestKeys -> IO (Either ClientError PollResponses)
simplePoll rkeys = do
  h <- hostAddressFromText defHostAddressText
  primPoll defChainwebVersion defChainId h rkeys

primPoll
    :: ChainwebVersion
    -> ChainId
    -> HostAddress
    -> RequestKeys
    -> IO (Either ClientError PollResponses)
primPoll v cid h rkeys = do
  ce <- genClientEnv h
  runClientM (poll v cid . Poll $ _rkRequestKeys rkeys) ce

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

generateDefaultSimpleCommands :: Int -> IO [Command Text]
generateDefaultSimpleCommands batchsize =
    replicateM batchsize $ getStdRandom (runState go) >>= easyCmd
  where
    go = do
      a <- state $ randomR (1, 100 :: Integer)
      b <- state $ randomR (1, 100 :: Integer)
      opIndex <- state $ randomR (0, 2 :: Int)
      return $ printf "(%s %s %s)" ["+-*" !! opIndex] a b

sendSimpleBatch :: Int -> IO (Either ClientError RequestKeys)
sendSimpleBatch batchsize =
    generateDefaultSimpleCommands batchsize >>= easySend
