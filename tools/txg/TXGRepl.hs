{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# language GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module TXGRepl where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens hiding ((.=))

import Data.Aeson
import Data.Aeson.Types
-- import Data.ByteString (ByteString)
import Data.Decimal
import Data.Foldable
import qualified Data.List.NonEmpty as NEL
import Data.Proxy
import qualified Data.Text as T
import Data.Text (Text)

import GHC.Generics

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

import TXG.Simulate.Utils

-- for ghci

data Builtin
    = Coin
    | Hello
    | Payments
    deriving (Show, Eq, Ord, Bounded, Enum, Generic)
    deriving anyclass (FromJSON, ToJSON)

type Keyset = [SomeKeyPair]

type Guard = Keyset

type Account = String

type SenderName = String

type ReceiverName = String

type Balance = Decimal

type Amount = Decimal

data CoinCode
    = CoinCreateAccount Account Guard
    | CoinAccountBalance Account
    | CoinTransfer SenderName ReceiverName Amount Keyset

data SimplePaymentsCode =
    SPGetBalance Account
    | SPPay Account Account Amount
    | SPCreateAccount Account Balance [Keyset]

data CallBuiltIn'
    = CC CoinCode
    | SP SimplePaymentsCode
    | HelloCode String

data TxContent
    = PactCode String
    | Define Builtin
    | CallBuiltin CallBuiltIn'

data TxConfig = TxConfig
    { txcfgVersion :: ChainwebVersion
    , txcfgHostAddress :: HostAddress
    , txcfgChainId :: ChainId
    }
    deriving Show

defaultTxConfig :: MonadThrow m => m TxConfig
defaultTxConfig =
    TxConfig Testnet02
    <$> hostAddressFromText "us2.testnet.chainweb.com:443"
    <*> chainIdFromText "0"

mkTxConfig :: MonadThrow f => T.Text -> T.Text -> f TxConfig
mkTxConfig hostaddress cid =
    TxConfig Testnet02
    <$> hostAddressFromText hostaddress
    <*> chainIdFromText cid

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

newtype Txg a = Txg {runTxg' :: ReaderT TxConfig IO a}
  deriving newtype
    (Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader TxConfig)

runTxg :: TxConfig -> Txg a -> IO a
runTxg = flip $ runReaderT . runTxg'

type PactBatch a = (a, [Pair], Text, Keyset)
type PactBatch' a = (a, [Pair], PublicMeta, Keyset)

defaultPublicMeta :: PublicMeta
defaultPublicMeta = PublicMeta (Pact.ChainId "0") "sender00" 100 1.0

sendPactCodeDefaultSender
    :: [(String, [Pair], PublicMeta)]
    -> Txg (Either ClientError RequestKeys)
sendPactCodeDefaultSender xs = do
    kps <- toList <$> liftIO testSomeKeyPairs
    sendPactCode' (addAdmin kps . f kps <$> xs)
  where
    f d (a,b,c) = (a,b,c,d)
    -- I'm not sure of addAdmin is necessary any longer.
    addAdmin kps = over _2 (("test-admin-keyset" .= fmap formatB16PubKey kps) :)

sendPactCode :: [PactBatch String] -> Txg (Either ClientError RequestKeys)
sendPactCode [] = error "come back to this"
sendPactCode xs = do
    cmds <-
      liftIO $
        forM xs $ \(code, thepairs, sender, keyset) ->
           mkExec code (object thepairs) (toPubMeta sender) keyset Nothing
    TxConfig v h cid <- ask
    cenv <- liftIO $ genClientEnv h
    liftIO $ runClientM (send v cid (SubmitBatch $ NEL.fromList cmds)) cenv
  where
    toPubMeta sender =
      defaultPublicMeta { _pmSender = sender }

-- This is the more general version which lets you set the gas price
-- and gas limit
sendPactCode' :: [PactBatch' String] -> Txg (Either ClientError RequestKeys)
sendPactCode' [] = error "come back to this"
sendPactCode' xs = do
    cmds <-
      liftIO $
        forM xs $ \(code, thepairs, pubMeta, keyset) ->
           mkExec code (object thepairs) pubMeta keyset Nothing
    TxConfig v h cid <- ask
    cenv <- liftIO $ genClientEnv h
    liftIO $ runClientM (send v cid (SubmitBatch $ NEL.fromList cmds)) cenv

coinTransfer :: CoinCode -> Maybe (String, Keyset)
coinTransfer (CoinTransfer sender receiver amount guard') = Just (stmt, guard')
    where
    stmt = printf
        "(coin.transfer \"%s\" \"%s\" (read-keyset \"%s\") %s)"
        sender
        receiver
        ("receiver-guard" :: String)
        (show amount)
coinTransfer _ = Nothing

coinAccountBalance :: CoinCode -> Maybe String
coinAccountBalance (CoinAccountBalance account) = Just $
    printf "(coin.account-balance \"%s\")"
      account
coinAccountBalance _ = Nothing

coinCreateAccount :: CoinCode -> Maybe (String, Keyset)
coinCreateAccount (CoinCreateAccount account guard') =
    Just (stmt, guard')
  where
    stmt = printf "(coin.create-account \"%s\" (read-keyset \"%s\"))"
        account
coinCreateAccount _ = Nothing

handleCoin :: CoinCode -> Maybe (Either String (String, Keyset))
handleCoin = undefined

handleSP :: SimplePaymentsCode -> Maybe (Either String (String, Keyset))
handleSP = undefined

handleHello :: String -> Maybe (Either String (String, Keyset))
handleHello = undefined

txContentToString :: TxContent -> String
txContentToString = \case
    PactCode str -> undefined $ str
    Define _ -> undefined
    CallBuiltin (CC coincode) -> undefined $ coincode
    CallBuiltin (SP spcode) -> undefined $ spcode
    CallBuiltin (HelloCode personName) -> undefined $ personName

sendTx :: [PactBatch TxContent] -> Txg (Either ClientError RequestKeys)
sendTx = sendPactCode . map (over _1 txContentToString)

sendTx' :: [PactBatch' TxContent] -> Txg (Either ClientError RequestKeys)
sendTx' = sendPactCode' . map (over _1 txContentToString)

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

-- This is how you might send some "simple arithmetic transactions" programmatically.
generateDefaultSimpleBatch :: Int -> IO [(String,[Pair],PublicMeta)]
generateDefaultSimpleBatch batchsize =
    replicateM batchsize $ getStdRandom (runState go)
  where
    go = do
        a <- state $ randomR (1, 100 :: Integer)
        b <- state $ randomR (1, 100 :: Integer)
        opIndex <- state $ randomR (0,2 :: Int)
        return (toCode ("+-*" !! opIndex) a b,[] :: [Pair],defaultPublicMeta)
      where
        toCode operation operandA operandB =
          "(" ++ [operation] ++ " " ++ show operandA ++ " " ++ show operandB ++ ")"


sendSimpleBatch
    :: Text
    -> Text
    -> Int
    -> IO (Either ClientError RequestKeys)
sendSimpleBatch hostaddress cid batchsize = do
    batch <- syncChainId <$> generateDefaultSimpleBatch batchsize
    conf <- mkTxConfig hostaddress cid
    runTxg conf (sendPactCodeDefaultSender batch)
  where
    syncChainId = traverse . _3 . pmChainId .~ (Pact.ChainId cid)
