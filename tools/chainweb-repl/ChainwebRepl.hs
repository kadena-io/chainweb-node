{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: ChainwebRepl
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module ChainwebRepl
(
-- * Pact API
  send
, sendOne
, listen
, poll
, local
, spv

-- * High-level Pact operations
, accountInfo
, accountBalance
, transfer
, crossChainTransfer

-- * Environment
, Env(..)
, envVersion
, envChain
, envNode
, envManager
, envKeys
, envGasLimit
, envGasPrice
, envSender
, envTtl
, envPublicMeta
, envCid
, _envCid
, envUrl
, _envUrl

-- * Predefined Environments
, devEnv

-- * Misc
, mgr
, prettyJ
, singlePacts
, jsonSecrets
) where


import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Retry

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Decimal
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import GHC.Generics hiding (to)

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS

import Pact.ApiReq
import Pact.Types.API
import qualified Pact.Types.ChainId as Pact
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Crypto
import Pact.Types.Gas
import Pact.Types.Hash
import Pact.Types.PactValue
import Pact.Types.Runtime (toPactId)
import Pact.Types.SPV

import Servant.Client

import System.IO.Unsafe

-- internal modules

import Chainweb.HostAddress
import Chainweb.Pact.RestAPI.Client
import Chainweb.Pact.Service.Types
import Chainweb.Utils
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Environment

data Env = Env
    { _envVersion :: !ChainwebVersion
    , _envChain :: !ChainId
    , _envNode :: !HostAddress
    , _envManager :: !HTTP.Manager
    , _envKeys :: ![SomeKeyPair]
    , _envSender :: !T.Text
    , _envGasLimit :: !GasLimit
    , _envGasPrice :: !GasPrice
    , _envTtl :: !TTLSeconds
    }

makeLenses ''Env

instance HasChainwebVersion Env where
    _chainwebVersion = _envVersion

instance HasChainId Env where
    _chainId = _envChain

_envPublicMeta :: Env -> PublicMeta
_envPublicMeta env = PublicMeta
    { _pmChainId = Pact.ChainId $ toText $ _envChain env
    , _pmSender = _envSender env
    , _pmGasLimit = _envGasLimit env
    , _pmGasPrice = _envGasPrice env
    , _pmTTL = _envTtl env
    , _pmCreationTime = 0
    }

envPublicMeta :: Getter Env PublicMeta
envPublicMeta = to _envPublicMeta

_envCid :: Integral i => Env -> i
_envCid = chainIdInt . _envChain

envCid :: Lens' Env Int
envCid = lens _envCid (\e c -> e { _envChain = cid e c })

_envUrl :: Env -> BaseUrl
_envUrl env = BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = T.unpack . toText . _hostAddressHost . _envNode $ env
    , baseUrlPort = int . _hostAddressPort . _envNode $ env
    , baseUrlPath = ""
    }

envUrl :: Getter Env BaseUrl
envUrl = to _envUrl

_envClientEnv :: Env -> ClientEnv
_envClientEnv env = mkClientEnv (_envManager env) (_envUrl env)

mgr :: HTTP.Manager
mgr = unsafePerformIO $ HTTPS.newTlsManager
{-# NOINLINE mgr #-}

devEnv
    :: T.Text
        -- secret key in hex encoding
    -> T.Text
        -- sender account
    -> Int
        -- chain
    -> Env
devEnv key sender c = Env
    { _envVersion = Development
    , _envChain = cid Development c
    , _envNode = unsafeHostAddressFromText "us1.tn1.chainweb.com:443"
    , _envManager = mgr
    , _envKeys = either (error . show) pure $ plainSecret key
    , _envSender = sender
    , _envGasLimit = 1000
    , _envGasPrice = 0.001
    , _envTtl = 600
    }

cid
    :: (HasChainwebVersion v, Integral i)
    => v
    -> i
    -> ChainId
cid env = either (error . sshow) id . mkChainId env

run :: Env -> ClientM b -> IO b
run env req = runClientM req (_envClientEnv env) >>= \case
    Left e -> error (show e)
    Right x -> return x

-- -------------------------------------------------------------------------- --
-- Pact API

-- Usage:
--
-- > let key = "hex encoded key"
-- > let c = 0
-- > let sender = "us1"
-- > let env = devEnv key sender c
-- > ks <- send env (pure "(coin.account-info \"us1\")")
-- > listen env (NE.head ks)

mkCmd :: Env -> Value -> T.Text -> IO (Command T.Text)
mkCmd env dat pact
    = mkExec (T.unpack pact) dat (_envPublicMeta env) (_envKeys env) Nothing

noData :: Value
noData = object []

singlePacts :: T.Text -> NE.NonEmpty (Value, T.Text)
singlePacts t = pure (noData, t)

local :: Env -> Value -> T.Text -> IO (CommandResult Hash)
local env dat pact = do
    cmd <- mkCmd env dat pact
    run env $ pactLocalApiClient (_envVersion env) (_envChain env) cmd

send :: Env -> NE.NonEmpty (Value, T.Text) -> IO (NE.NonEmpty RequestKey)
send env pacts = _rkRequestKeys <$> do
    cmds <- SubmitBatch <$> traverse (uncurry $ mkCmd env) pacts
    run env $ pactSendApiClient (_envVersion env) (_envChain env) cmds

sendOne :: Env -> Value -> T.Text -> IO RequestKey
sendOne env dat pact = NE.head <$> send env (pure (dat, pact))

poll :: Env -> NE.NonEmpty RequestKey -> IO PollResponses
poll env reqKeys = run env
    $ pactPollApiClient (_envVersion env) (_envChain env) $ Poll reqKeys

listen :: Env -> RequestKey -> IO (CommandResult Hash)
listen env reqKey = do
    r <- run env
        $ pactListenApiClient (_envVersion env) (_envChain env)
        $ ListenerRequest reqKey
    case r of
        ListenTimeout _ -> error "listen timeout"
        ListenResponse x -> return x

-- -------------------------------------------------------------------------- --
-- High level Pact commands

newtype PactDecimal = PactDecimal Decimal
    deriving (Eq, Ord, Num, Real, RealFrac, Fractional)

instance Show PactDecimal where
    show (PactDecimal d)
        | realToFrac (round d) == d = show d <> ".0"
        | otherwise = show d

accountInfo :: Env -> T.Text -> IO PactValue
accountInfo env account = do
    result <$> local env noData ("(coin.account-info " <> sshow account <> ")")

accountBalance :: Env -> T.Text -> IO PactValue
accountBalance env account = do
    result <$> local env noData ("(coin.account-balance " <> sshow account <> ")")

transfer :: Env -> T.Text -> T.Text -> Decimal -> IO PactValue
transfer env acc0 acc1 amount = do
    rk <- sendOne env noData $ pactFun "coin.transfer"
            [ StringArg acc0
            , StringArg acc1
            , DecArg amount
            ]
    result <$> listen env rk

-- -------------------------------------------------------------------------- --
-- SPV

-- env is target chain env!
spvCont :: Env -> RequestKey -> ContProof -> IO (Command T.Text)
spvCont env rk proof
    = mkCont pid 1 False noData (_envPublicMeta env) (_envKeys env) Nothing (Just proof)
  where
    pid = toPactId $ unRequestKey rk

spvRollBackCmd :: Env -> RequestKey -> IO (Command T.Text)
spvRollBackCmd env rk
    = mkCont pid 0 True noData (_envPublicMeta env) (_envKeys env) Nothing Nothing
  where
    pid = toPactId $ unRequestKey rk

sendSpvCont :: Env -> RequestKey -> ContProof -> IO RequestKey
sendSpvCont env rk proof = NE.head . _rkRequestKeys <$> do
    cmds <- SubmitBatch . pure <$> spvCont env rk proof
    run env $ pactSendApiClient (_envVersion env) (_envChain env) cmds

spvRollBack :: Env -> RequestKey -> IO RequestKey
spvRollBack env rk = NE.head . _rkRequestKeys <$> do
    cmds <- SubmitBatch . pure <$> spvRollBackCmd env rk
    run env $ pactSendApiClient (_envVersion env) (_envChain env) cmds

spv
    :: Env
    -> RequestKey
        -- source transaction
    -> ChainId
        -- ^ target chain
    -> IO ContProof
spv env srcTxKey trgCid = do
    (TransactionOutputProofB64 p) <- run env
        $ pactSpvApiClient (_envVersion env) (_envChain env)
        $ SpvRequest srcTxKey (toPactCid trgCid)
    return $ ContProof $ T.encodeUtf8 p

crossChainTransfer :: Env -> Env -> T.Text -> T.Text -> T.Text -> Decimal -> IO PactValue
crossChainTransfer srcEnv trgEnv srcAcc trgAcc pubKey amount = do

    -- delet coins
    rk <- sendOne srcEnv dat $ pactFun "coin.cross-chain-transfer"
        [ StringArg srcAcc
        , ChainArg trgCid
        , StringArg trgAcc
        , GuardArg ("(read-keyset \"receiver-keyset\")")
        , DecArg amount
        ]

    let redeem = do

            -- await result
            !_ <- retryListen "delete" srcEnv rk

            -- create spv proof
            proof <- retryProof srcEnv rk trgCid

            -- Redeem coins
            trgRk <- sendSpvCont trgEnv rk proof
            retryListen "redeem" trgEnv trgRk

    -- FIXME: In some cases we have to retry the rollback, too!
    redeem `onException` do
        putStrLn $ "try to rollback spv transaction " <> show rk
        rk2 <- spvRollBack srcEnv rk
        putStrLn $ "rollback tx: " <> show rk2
        retryListen "rollback" srcEnv rk2
  where
    trgCid = _envChain trgEnv

    -- Create Account Guard
    dat = object
        [ "receiver-keyset" .= [ pubKey ]
        ]

    retryProof env rk c =
        recoverAll (exponentialBackoff 2000000 <> limitRetries 6) $ \s -> do
            putStrLn
                $ "await proof for target chain " <> T.unpack (toText c)
                <> " [" <> show (view rsIterNumberL s) <> "]"
            spv env rk c

    retryListen label env key = result <$> do
        recoverAll (constantDelay 50000 <> limitRetries 2) $ \s -> do
            putStrLn
                $ "await " <> label
                <> " on chain " <> T.unpack (toText (_envChain env))
                <> " [" <> show (view rsIterNumberL s) <> "]: "
                <> show key
            listen env key

-- -------------------------------------------------------------------------- --
-- Pact Code

pactFun :: T.Text -> [Arg] -> T.Text
pactFun name args = "(" <> name <> " " <> T.intercalate " " (sshow <$> args) <> ")"

data Arg
    = StringArg T.Text
    | DecArg Decimal
    | PactArg T.Text
    | GuardArg T.Text
    | ChainArg ChainId
    deriving (Eq, Ord, Generic)

instance Show Arg where
    show (StringArg t) = show t
    show (DecArg d) = show (PactDecimal d)
    show (PactArg p) = T.unpack p
    show (GuardArg g) = T.unpack g
    show (ChainArg c) = show $ toText c

-- -------------------------------------------------------------------------- --
-- Utils

prettyJ :: ToJSON a => a -> IO ()
prettyJ = BL8.putStrLn . encodePretty

result :: CommandResult l -> PactValue
result r = case _crResult r of
    PactResult (Left e) -> error $ "pact command failure: " <> show e
    PactResult (Right x) -> x

toPactCid :: ChainId -> Pact.ChainId
toPactCid = Pact.ChainId . toText

-- -------------------------------------------------------------------------- --
-- Keys

newtype SecretKeyConf = SecretKeyConf PrivateKeyBS
    deriving (Show, Eq)

instance FromJSON SecretKeyConf where
    parseJSON = withObject "SecretKeyConfig" $ \o -> SecretKeyConf
        <$> o .: "secret"

plainSecret :: MonadThrow m => T.Text -> m SomeKeyPair
plainSecret hexText = do
    x <- decodeStrictOrThrow' $ "\"" <> T.encodeUtf8 hexText <> "\""
    go x
  where
    go x = case importKeyPair defaultScheme Nothing x of
        Left e -> throwM (DecodeException $ T.pack e)
        Right k -> return k

jsonSecrets
    :: MonadThrow m
    => T.Text
    -> m [SomeKeyPair]
jsonSecrets jsonText = do
    x <- decodeStrictOrThrow' $ T.encodeUtf8 jsonText
    mapM go x
  where
    go x = case importKeyPair defaultScheme Nothing x of
        Left e -> throwM (DecodeException $ T.pack e)
        Right k -> return k

