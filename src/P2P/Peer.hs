{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: P2P.Peer
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A peer in a Chainweb P2P network.
--
module P2P.Peer
(
-- * Peer Id
  PeerId(..)
, peerIdToText
, shortPeerId
, peerIdFromText
, unsafePeerIdFromText
, pPeerId
, peerIdToFingerprint
, peerIdFromFingerprint

-- * Peer Info
, PeerInfo(..)
, peerId
, peerAddr
, pPeerInfo
, pPeerInfoCompact
, peerInfoClientEnv

-- * Peer Configuration
, PeerConfig(..)
, peerConfigAddr
, peerConfigInterface
, peerConfigCertificateChain
, peerConfigKey
, defaultPeerConfig
, _peerConfigPort
, peerConfigPort
, _peerConfigHost
, peerConfigHost
, pPeerConfig
, peerInfoHostname
, peerInfoPort
, shortPeerInfo

-- * Peer
, Peer(..)
, peerInfo
, peerInterface
, peerCertificateChain
, peerKey
, unsafeCreatePeer
, getPeerCertificate

-- * Bootstrap Peer Infos
, bootstrapPeerInfos

) where

import Configuration.Utils hiding (Lens')

import Control.DeepSeq
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch
import Control.Exception (evaluate)

import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Hashable
import Data.Streaming.Network
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as T

import GHC.Generics (Generic)
import GHC.Stack

import qualified Network.HTTP.Client as HTTP

import Servant.Client

-- internal modules

import Chainweb.HostAddress
import Chainweb.Utils hiding (check)
import Chainweb.Version

import Network.X509.SelfSigned

-- -------------------------------------------------------------------------- --
-- Peer Id

newtype PeerId = PeerId B.ByteString
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData)
    -- deriving newtype (ToJSONKey, FromJSONKey)

peerIdToText :: PeerId -> T.Text
peerIdToText (PeerId b) = encodeB64UrlNoPaddingText b
{-# INLINE peerIdToText #-}

shortPeerId :: PeerId -> T.Text
shortPeerId = T.take 6 . toText

peerIdFromText :: MonadThrow m => T.Text -> m PeerId
peerIdFromText t = do
    !bytes <- decodeB64UrlNoPaddingText t
    unless (B.length bytes == int fingerprintByteCount) $ throwM
        $ TextFormatException
        $ "wrong peer-id length: expected "
        <> sshow fingerprintByteCount <> " bytes, got "
        <> sshow (B.length bytes) <> " bytes."
    return $! PeerId bytes
{-# INLINE peerIdFromText #-}

unsafePeerIdFromText :: HasCallStack => String -> PeerId
unsafePeerIdFromText = fromJuste . peerIdFromText . T.pack
{-# INLINE unsafePeerIdFromText #-}

instance HasTextRepresentation PeerId where
    toText = peerIdToText
    {-# INLINE toText #-}
    fromText = peerIdFromText
    {-# INLINE fromText #-}

instance ToJSON PeerId where
    toJSON = toJSON . toText
    {-# INLINE toJSON #-}

instance FromJSON PeerId where
    parseJSON = parseJsonFromText "PeerId"
    {-# INLINE parseJSON #-}

pPeerId :: Maybe String -> OptionParser PeerId
pPeerId service = textOption
    % prefixLong service "peer-id"
{-# INLINE pPeerId #-}

peerIdToFingerprint :: PeerId -> Fingerprint
peerIdToFingerprint (PeerId b) = Fingerprint b
{-# INLINE peerIdToFingerprint #-}

peerIdFromFingerprint :: Fingerprint -> PeerId
peerIdFromFingerprint (Fingerprint b) = PeerId b
{-# INLINE peerIdFromFingerprint #-}

-- -------------------------------------------------------------------------- --
-- Peer Info

-- | TODO: eventually this should have more information, like, for instance,
-- the API version that the peer supports, public key, etc.
--
data PeerInfo = PeerInfo
    { _peerId :: !(Maybe PeerId)
        -- ^ The peer id wraps the SHA256 fingerprint of the X509 certificate.
        --
        -- If this is 'Nothing' we assume that the HostAddress fully identifies
        -- the peer and it has certificate that can be validated from the system
        -- CA store.

    , _peerAddr :: !HostAddress
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData)

makeLenses ''PeerInfo

instance ToJSON PeerInfo where
    toJSON a = object
        [ "id" .= _peerId a
        , "address" .= _peerAddr a
        ]
    {-# INLINE toJSON #-}

instance FromJSON PeerInfo where
    parseJSON = withObject "PeerInfo" $ \o -> PeerInfo
        <$> o .: "id"
        <*> o .: "address"
    {-# INLINE parseJSON #-}

instance FromJSON (PeerInfo -> PeerInfo) where
    parseJSON = withObject "PeerInfo" $ \o -> id
        <$< peerId ..: "id" % o
        <*< peerAddr ..: "address" % o
    {-# INLINE parseJSON #-}

peerInfoToText :: PeerInfo -> T.Text
peerInfoToText pinf
    = maybe "" ((<> "@") . toText) (_peerId pinf) <> toText (_peerAddr pinf)

peerInfoFromText :: MonadThrow m => T.Text -> m PeerInfo
peerInfoFromText = parseM $ PeerInfo <$> parsePeerId <*> parseAddr
  where
    parsePeerId = Just <$> parseText (A.takeTill (== '@') <* "@") <|> pure Nothing
    parseAddr = parseText A.takeText

peerInfoPort :: Lens' PeerInfo Port
peerInfoPort = peerAddr . hostAddressPort

peerInfoHostname :: Lens' PeerInfo Hostname
peerInfoHostname = peerAddr . hostAddressHost

shortPeerInfo :: PeerInfo -> T.Text
shortPeerInfo pinf =
    toText (_peerAddr pinf) <> "#" <> maybe "" shortPeerId (_peerId pinf)

instance HasTextRepresentation PeerInfo where
    toText = peerInfoToText
    {-# INLINE toText #-}
    fromText = peerInfoFromText
    {-# INLINE fromText #-}

pPeerInfo :: Maybe String -> MParser PeerInfo
pPeerInfo service = id
    <$< peerId .:: fmap Just % pPeerId service
    <*< peerAddr %:: pHostAddress service
{-# INLINE pPeerInfo #-}

-- | Parser Peer Id as a single option
--
pPeerInfoCompact :: Maybe String -> OptionParser PeerInfo
pPeerInfoCompact service = textOption
    % prefixLong service "peer-info"
    <> suffixHelp service "peer info"
    <> metavar "[<PEERID>@]<HOSTADDRESS>"

-- | Create a ClientEnv for querying HTTP API of a PeerInfo
--
peerInfoClientEnv :: HTTP.Manager -> PeerInfo -> ClientEnv
peerInfoClientEnv mgr = mkClientEnv mgr . peerBaseUrl . _peerAddr
  where
    peerBaseUrl a = BaseUrl Https
        (B8.unpack . hostnameBytes $ view hostAddressHost a)
        (int $ view hostAddressPort a)
        ""
-- -------------------------------------------------------------------------- --
-- Peer Configuration

data PeerConfig = PeerConfig
    { _peerConfigAddr :: !HostAddress
        -- ^ The public host address of the peer.
        -- A port number of 0 means that a free port is assigned by the system.

    , _peerConfigInterface :: !HostPreference
        -- ^ The network interface that the peer binds to. Default is to
        -- bind to all available interfaces ('*').

    , _peerConfigCertificateChain :: !(Maybe X509CertChainPem)
        -- ^ The X509 certificate chain of the peer. If this is Nothing a new ephemeral
        -- certificate is generated on startup and discarded on exit.

    , _peerConfigCertificateChainFile :: !(Maybe FilePath)
        -- ^ A file with the X509 certificate chain of the peer. If this is
        -- Nothing a new ephemeral certificate is generated on startup and
        -- discarded on exit.
        --
        -- If '_peerConfigCertificateChain' is not 'Nothing' it has precedence.

    , _peerConfigKey :: !(Maybe X509KeyPem)
        -- ^ The key for the X509 certificate. If no certificate is provided the
        -- key is ignored. It is an error if a certificate is provided but no key.
        --

    , _peerConfigKeyFile :: !(Maybe FilePath)
        -- ^ A file with key for the X509 certificate. If no certificate is
        -- provided the key is ignored. It is an error if a certificate is
        -- provided but no key.
        --
        -- If '_peerConfigKey' is not 'Nothing' it has precedence.
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''PeerConfig

_peerConfigPort :: PeerConfig -> Port
_peerConfigPort = _hostAddressPort . _peerConfigAddr

peerConfigPort :: Lens' PeerConfig Port
peerConfigPort = peerConfigAddr . hostAddressPort

_peerConfigHost :: PeerConfig -> Hostname
_peerConfigHost = _hostAddressHost . _peerConfigAddr

peerConfigHost :: Lens' PeerConfig Hostname
peerConfigHost = peerConfigAddr . hostAddressHost

defaultPeerConfig :: PeerConfig
defaultPeerConfig = PeerConfig
    { _peerConfigAddr = HostAddress localhost 0
    , _peerConfigInterface = fromString "*"
    , _peerConfigCertificateChain = Nothing
    , _peerConfigCertificateChainFile = Nothing
    , _peerConfigKey = Nothing
    , _peerConfigKeyFile = Nothing
    }

instance ToJSON PeerConfig where
    toJSON o = object
        [ "hostaddress" .= _peerConfigAddr o
        , "interface" .= hostPreferenceToText (_peerConfigInterface o)
        , "certificateChain" .= _peerConfigCertificateChain o
        , "certificateChainFile" .= _peerConfigCertificateChainFile o
        , "key" .= _peerConfigKey o
        , "keyFile" .= _peerConfigKeyFile o
        ]

instance FromJSON PeerConfig where
    parseJSON = withObject "PeerConfig" $ \o -> PeerConfig
        <$> o .: "hostaddress"
        <*> (parseJsonFromText "interface" =<< o .: "interface")
        <*> o .: "certificateChain"
        <*> o .: "certificateChainFile"
        <*> o .: "key"
        <*> o .: "keyFile"

instance FromJSON (PeerConfig -> PeerConfig) where
    parseJSON = withObject "PeerConfig" $ \o -> id
        <$< peerConfigAddr %.: "hostaddress" % o
        <*< setProperty peerConfigInterface "interface" (parseJsonFromText "interface") o
        <*< peerConfigCertificateChain ..: "certificateChain" % o
        <*< peerConfigCertificateChainFile ..: "certificateChainFile" % o
        <*< peerConfigKey ..: "key" % o
        <*< peerConfigKeyFile ..: "keyFile" % o

pPeerConfig :: Maybe String -> MParser PeerConfig
pPeerConfig service = id
    <$< peerConfigAddr %:: pHostAddress service
    <*< peerConfigInterface .:: textOption
        % prefixLong service "interface"
        <> suffixHelp service "interface that the Rest API binds to (see HostPreference documentation for details)"
    <*< peerConfigCertificateChain .:: fmap Just % pX509CertChainPem service
    <*< peerConfigCertificateChainFile .:: fmap Just % fileOption
        % prefixLong service "certificate-chain-file"
        <> suffixHelp service "file with the PEM encoded certificate chain. A textually provided certificate chain has precedence over a file."
    <*< peerConfigKey .:: fmap Just % pX509KeyPem service
    <*< peerConfigKeyFile .:: fmap Just % fileOption
        % prefixLong service "certificate-key-file"
        <> suffixHelp service "file with the PEM encoded certificate key. A textually provided certificate key has precedence over a file."
{-# INLINE pPeerConfig #-}

-- -------------------------------------------------------------------------- --
-- Peer

-- | A peer in a P2P network
--
data Peer = Peer
    { _peerInfo :: !PeerInfo
        -- ^ The peer id is the SHA256 fingerprint of the certificate
    , _peerInterface :: !HostPreference
    , _peerCertificateChain :: !X509CertChainPem
    , _peerKey :: !X509KeyPem
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''Peer

getPeerCertificate
    :: PeerConfig
    -> IO (Fingerprint, X509CertChainPem, X509KeyPem)
getPeerCertificate conf = do
    maybeChain <- case _peerConfigCertificateChain conf of
        Nothing -> do
            bytes <- traverse T.readFile $ _peerConfigCertificateChainFile conf
            traverse x509CertChainPemFromText bytes
        x -> evaluate x

    maybeKey <- case _peerConfigKey conf of
        Nothing -> do
            bytes <- traverse T.readFile $ _peerConfigKeyFile conf
            traverse x509KeyPemFromText bytes
        x -> evaluate x

    case (maybeChain, maybeKey) of
        (Nothing, _) -> do
            (!fp, !c, !k) <- generateSelfSignedCertificate @DefCertType 365 dn Nothing
            return (fp, X509CertChainPem c [], k)
        (Just !c@(X509CertChainPem !a _), (Just !k)) ->
            return (unsafeFingerprintPem a, c, k)
        _ -> throwM $ ConfigurationException "missing certificate key in peer config"
  where
    dn = name . B8.unpack . hostnameBytes . _hostAddressHost . _peerConfigAddr $ conf

unsafeCreatePeer :: PeerConfig -> IO Peer
unsafeCreatePeer conf = do
    (!fp, !certs, !key) <- getPeerCertificate conf
    return $! Peer
        { _peerInfo = PeerInfo
            { _peerId = Just $! peerIdFromFingerprint fp
            , _peerAddr = _peerConfigAddr conf
            }
        , _peerInterface = _peerConfigInterface conf
        , _peerCertificateChain = certs
        , _peerKey = key
        }

instance ToJSON Peer where
    toJSON p = object
        [ "info" .= _peerInfo p
        , "interface" .= hostPreferenceToText (_peerInterface p)
        , "certifcateChain" .= _peerCertificateChain p
        , "key" .= _peerKey p
        ]
    {-# INLINE toJSON #-}

instance FromJSON Peer where
    parseJSON = withObject "Peer" $ \o -> Peer
        <$> o .: "info"
        <*> (parseJsonFromText "interface" =<< o .: "interface")
        <*> o .: "certificate"
        <*> o .: "key"
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Bootstrap Peer Info

-- | For each chainweb version there is a hardcoded set of bootstrap nodes for
-- the P2P network.
--
-- If a bootstrap node has an public DNS name with an official TLS certificate
-- the peer-id should be omitted. For bootstrap nodes without an proper
-- certificate, the peer id is the SHA256 hash of the X509 certificate.
--
bootstrapPeerInfos :: ChainwebVersion -> [PeerInfo]
bootstrapPeerInfos Test{} = [testBootstrapPeerInfos]
bootstrapPeerInfos TimedConsensus{} = [testBootstrapPeerInfos]
bootstrapPeerInfos PowConsensus{} = [testBootstrapPeerInfos]
bootstrapPeerInfos TimedCPM{} = [testBootstrapPeerInfos]
bootstrapPeerInfos FastTimedCPM{} = [testBootstrapPeerInfos]
bootstrapPeerInfos Development = productionBootstrapPeerInfo
bootstrapPeerInfos Testnet04 = productionBootstrapPeerInfo
bootstrapPeerInfos Mainnet01 = productionBootstrapPeerInfo

testBootstrapPeerInfos :: PeerInfo
testBootstrapPeerInfos =
    PeerInfo
#if WITH_ED25519
        { _peerId = Just $ unsafeFromText "BMe2hSdSEGCzLwvoYXPuB1BqYEH5wiV5AvacutSGWmg"
#else
        { _peerId = Just $ unsafeFromText "9LkpIG95q5cs0YJg0d-xdR2YLeW_puv1PjS2kEfmEuQ"
#endif
            -- this is the fingerprint of the certificate and key that is stored
            -- in ./scripts/test-bootstrap-node.config". For programatic use of
            -- the same certificate is also available at
            -- "Chainweb.Test.P2P.Peer.BootstrapConfig". It is intended for
            -- testing purposes only.

        , _peerAddr = HostAddress
            { _hostAddressHost = localhost
            , _hostAddressPort = 1789
            }
        }

productionBootstrapPeerInfo :: [PeerInfo]
productionBootstrapPeerInfo = map f testnetBootstrapHosts
  where
    f hn = PeerInfo
        { _peerId = Nothing
        , _peerAddr = HostAddress
            { _hostAddressHost = hn
            , _hostAddressPort = 443
            }
        }

-- | Official TestNet bootstrap nodes.
--
testnetBootstrapHosts :: [Hostname]
testnetBootstrapHosts = map unsafeHostnameFromText []

