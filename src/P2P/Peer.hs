{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
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
, peerConfigCertificateChainFile
, peerConfigKey
, peerConfigKeyFile
, defaultPeerConfig
, validatePeerConfig
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

) where

import Configuration.Utils hiding (Lens')
import Configuration.Utils.Validation

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Writer

import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Hashable
import Data.Maybe
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
    toEncoding = toEncoding . toText
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

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

peerInfoProperties :: KeyValue e kv => PeerInfo -> [kv]
peerInfoProperties a =
    [ "id" .= _peerId a
    , "address" .= _peerAddr a
    ]
{-# INLINE peerInfoProperties #-}

instance ToJSON PeerInfo where
    toJSON  = object . peerInfoProperties
    toEncoding  = pairs . mconcat . peerInfoProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

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
    parsePeerId = optional $ parseText (A.takeTill (== '@') <* "@")
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
        --
        -- A port number of 0 means that a free port is assigned by the system.
        -- An IP address of 0.0.0.0 means that the node discovers its
        -- external IP address itself.

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
    { _peerConfigAddr = HostAddress anyIpv4 1789
    , _peerConfigInterface = fromString "*"
    , _peerConfigCertificateChain = Nothing
    , _peerConfigCertificateChainFile = Nothing
    , _peerConfigKey = Nothing
    , _peerConfigKeyFile = Nothing
    }

validatePeerConfig :: Applicative a => ConfigValidation PeerConfig a
validatePeerConfig c = do
    when (_peerConfigHost c /= anyIpv4 && isReservedHostAddress (_peerConfigAddr c)) $ tell
        $ pure "The configured hostname is a localhost name or from a reserved IP address range. Please use a public hostname or IP address, or '0.0.0.0' (automatic configuration)."

    when (_peerConfigInterface c == "localhost" || _peerConfigInterface c == "localnet") $ tell
        $ pure "The node is configured to listen only on a private network. Please use a public network as interface configuration, e.g. '*' or '0.0.0.0'"

    mapM_ (validateFileReadable "certificateChainFile") (_peerConfigCertificateChainFile c)

    mapM_ (validateFileReadable "certificateChainFile") (_peerConfigKeyFile c)

    when (isJust (_peerConfigCertificateChainFile c) && isJust (_peerConfigCertificateChain c)) $
        tell $ pure "The configuration provides both 'certificateChain' and 'certificateChainFile'. The 'certificateChain' setting will be used."

    when (isJust (_peerConfigKeyFile c) && isJust (_peerConfigKey c)) $
        tell $ pure "The configuration provides both 'key' and 'keyFile'. The 'key' setting will be used."

peerConfigProperties :: KeyValue e kv => PeerConfig -> [kv]
peerConfigProperties o =
    [ "hostaddress" .= _peerConfigAddr o
    , "interface" .= hostPreferenceToText (_peerConfigInterface o)
    , "certificateChain" .= _peerConfigCertificateChain o
    , "certificateChainFile" .= _peerConfigCertificateChainFile o
    , "key" .= _peerConfigKey o
    , "keyFile" .= _peerConfigKeyFile o
    ]
{-# INLINE peerConfigProperties #-}

instance ToJSON PeerConfig where
    toJSON = object . peerConfigProperties
    toEncoding = pairs . mconcat . peerConfigProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

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
        <> suffixHelp service ("interface that the " <> fromMaybe "" service <> "  Rest API binds to (see HostPreference documentation for details)")
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
        (Just c@(X509CertChainPem !a _), Just !k) ->
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

peerProperties :: KeyValue e kv => Peer -> [kv]
peerProperties p =
    [ "info" .= _peerInfo p
    , "interface" .= hostPreferenceToText (_peerInterface p)
    , "certificateChain" .= _peerCertificateChain p
    , "key" .= _peerKey p
    ]
{-# INLINE peerProperties #-}

instance ToJSON Peer where
    toJSON = object . peerProperties
    toEncoding = pairs. mconcat . peerProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON Peer where
    parseJSON = withObject "Peer" $ \o -> Peer
        <$> o .: "info"
        <*> (parseJsonFromText "interface" =<< o .: "interface")
        <*> o .: "certificateChain"
        <*> o .: "key"
    {-# INLINE parseJSON #-}

