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
-- TODO
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
, arbitraryPeerInfo
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

import Test.QuickCheck

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

instance Arbitrary PeerId where
    arbitrary = PeerId . B.pack <$> vector (int fingerprintByteCount)

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

arbitraryPeerInfo :: Gen PeerInfo
arbitraryPeerInfo = PeerInfo <$> arbitrary <*> arbitrary

instance Arbitrary PeerInfo where
    arbitrary = arbitraryPeerInfo

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
        <*< peerConfigCertificateChain ..: "certificateChainFile" % o
        <*< peerConfigKey ..: "key" % o
        <*< peerConfigKey ..: "keyFile" % o

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
            return $! (fp, X509CertChainPem c [], k)
        (Just !c@(X509CertChainPem !a _), (Just !k)) ->
            return $! (unsafeFingerprintPem a, c, k)
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
bootstrapPeerInfos Testnet02 = productionBootstrapPeerInfo
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

-- -------------------------------------------------------------------------- --
-- Arbitrary Instances

instance Arbitrary PeerConfig where
    arbitrary = do
        (c, k) <- oneof
            [ return (Just (X509CertChainPem certRsa []), Just keyRsa)
            , return (Just (X509CertChainPem certEd25519 []), Just keyEd25519)
            , return (Nothing, Nothing)
            ]
        PeerConfig
            <$> arbitrary
            <*> oneof (return <$> ["0.0.0.0", "127.0.0.1", "::1", "*", "*4", "!4", "*6", "!6"])
            <*> return c
            <*> pure Nothing
            <*> return k
            <*> pure Nothing
      where
        certRsa = X509CertPem $ B8.intercalate "\n"
            [ "-----BEGIN CERTIFICATE-----"
            , "MIIFBDCCAuygAwIBAgIBATANBgkqhkiG9w0BAQ0FADAUMRIwEAYDVQQDDAlsb2Nh"
            , "bGhvc3QwHhcNMTgxMjIyMDM1NzM2WhcNMzAwMzEwMDM1NzM2WjAUMRIwEAYDVQQD"
            , "DAlsb2NhbGhvc3QwggIiMA0GCSqGSIb3DQEBAQUAA4ICDwAwggIKAoICAQDo0L/M"
            , "d7FBuCLxJH4TebYCw/2FCoVO6Wuf0cDSDmePxoGk2dzBuT/0+Qh+PTp/Y7ran9pv"
            , "ss71OKb5/PtRAiQL0EFXtyYAOLz3kMpAxHOsoEMyzkWCPqIinSDJm6UI9UXcbvOu"
            , "dk4zqXbGrsryd2AH+/ti+5U+dupoOTks/d6pySbv+rSL9XL9dnTgQcEHzF7IdboS"
            , "crxGeCGRlHyL2YHii8X395Sk58gZ+fb84HMBM2AA9EUpU6DRSmgmj2opiu4UkbQ0"
            , "vl+r0UrZi3vVBcF1CCE2gru2BEkIFbZDgDO1aCpejMl3z5sUfdpuQNnYKdEgOcZS"
            , "+QzdpKOCdXQ4SsxTzIykKW9dp2h8OBpKAPAW929Gc3Ksa6Q1i4l/t+cvvGtvtPMJ"
            , "U1iPd3/O89RvZBpLdOwjjXIZOU86MgJBdByoZs4LJniatnkLxhWoUMZCNC+BzoON"
            , "qbg1cMj2p6u3A/HkWWFUhVxyNa5RmykHHEW3yJg0xIR8jomHTzpsWbotMS3axR7K"
            , "69IoEOI64b+1wR5Dyxh+2TGBVbHIahVrPLWaoXjfqTtTwlHjuu/MWyAQgWHK9XrH"
            , "R/LO5NfQH7emRqpY+rFdo83Bfq6YN5PPRZitrQYrnKFHnsFTwRyUaqbivevOFVFw"
            , "U5lVhr5gpEzyZObqp3qdFuZLh21YayHk30vBCwIDAQABo2EwXzAMBgNVHQ8EBQMD"
            , "B/wAMB0GA1UdJQQWMBQGCCsGAQUFBwMBBggrBgEFBQcDAjAPBgNVHRMECDAGAQH/"
            , "AgEAMB8GA1UdEQQYMBaCCWxvY2FsaG9zdIcJMTI3LjAuMC4xMA0GCSqGSIb3DQEB"
            , "DQUAA4ICAQBHE6MG3bIHLyaRCMgocwVTNyPf1Q00n099R5RIwZF4CNHODdLFywMW"
            , "dxBOWqhHmfXacSgiQBatyW1/B2NDagELAcmZvDnb5MPjY/cuBL4tHnv9tYNsk4zP"
            , "YM2zWdGiZv7E59GZaisFNogaLFw1ncYJk9AVQAJEDJRJ16qIWatPMoJ20McUfMRM"
            , "r8jVsTK7cgNTAnnH1xLXy+LtTqYsKpW82L9oksJgiYnYOAiG7zSg+W1ZsjO7NRiH"
            , "0CBs+Rcm2RIbPJYu16xGZ3V0ABoDX20r/xAlL03NQLkgEF6TETAdNhxhemoNRPNA"
            , "EoDFkJjGe8vbdTY+O/hnqWQ12atXSlsLvafKU9n9sU1LGv3qiv/AevPtanSA5eyw"
            , "XbVmEK35Wl85lEngnrw1mJIy5iS4pnnZAIWYfutvKlUeaO7u2cxSsjkRKYRc1M4M"
            , "niFHt0uw4UfW6b4UFuCEOyvUv5rl75qj0iJ8IfHmHYigUuel0xpSFEoRLmvxkWwf"
            , "vHc2IlMI186Di8ATXiLzIWzZxk0miK15IV7iiCx7OANHUrFpZePaKz5aI/5y8dNF"
            , "FWYZdHVPVklL12sVMybb7/CZWHkpHBjJkeROea0Ht/3laDTOeef2YGiTpjK8/KTJ"
            , "sF9RMi6wZ9BNEWcs4gsfd0mgss3J32aCk7cQve8isqjFpmGrTx23iA=="
            , "-----END CERTIFICATE-----"
            ]
        keyRsa = X509KeyPem $ B8.intercalate "\n"
            [ "-----BEGIN PRIVATE KEY-----"
            , "MIIJKAIBAAKCAgEA6NC/zHexQbgi8SR+E3m2AsP9hQqFTulrn9HA0g5nj8aBpNnc"
            , "wbk/9PkIfj06f2O62p/ab7LO9Tim+fz7UQIkC9BBV7cmADi895DKQMRzrKBDMs5F"
            , "gj6iIp0gyZulCPVF3G7zrnZOM6l2xq7K8ndgB/v7YvuVPnbqaDk5LP3eqckm7/q0"
            , "i/Vy/XZ04EHBB8xeyHW6EnK8RnghkZR8i9mB4ovF9/eUpOfIGfn2/OBzATNgAPRF"
            , "KVOg0UpoJo9qKYruFJG0NL5fq9FK2Yt71QXBdQghNoK7tgRJCBW2Q4AztWgqXozJ"
            , "d8+bFH3abkDZ2CnRIDnGUvkM3aSjgnV0OErMU8yMpClvXadofDgaSgDwFvdvRnNy"
            , "rGukNYuJf7fnL7xrb7TzCVNYj3d/zvPUb2QaS3TsI41yGTlPOjICQXQcqGbOCyZ4"
            , "mrZ5C8YVqFDGQjQvgc6Djam4NXDI9qertwPx5FlhVIVccjWuUZspBxxFt8iYNMSE"
            , "fI6Jh086bFm6LTEt2sUeyuvSKBDiOuG/tcEeQ8sYftkxgVWxyGoVazy1mqF436k7"
            , "U8JR47rvzFsgEIFhyvV6x0fyzuTX0B+3pkaqWPqxXaPNwX6umDeTz0WYra0GK5yh"
            , "R57BU8EclGqm4r3rzhVRcFOZVYa+YKRM8mTm6qd6nRbmS4dtWGsh5N9LwQsCAwEA"
            , "AQKCAgAZ0ayznEKIK0eUvptMxZ6tx9aBvpzyF2jcHILpyLcrdrRB9/dHdKfnHwcW"
            , "GOw33HOIc1niMU5KW7II30lvvnEI7luQwrrwI1qxidkkPaUma8+H/uAuhYutkwdj"
            , "1GMFD05kF9nmazBZAl4twoy3Pw3jVMqYO5ZdiwqL7Gyu1Kiw46Hx0zfananW+iAU"
            , "Dra9iqioSoYMLbU7i28Nmg4F5GVHHoEh8s8FSr8iQlplyO54H55x/KT+5EtdaocK"
            , "X7VCTioBFfeKMeKiR1+pqJJfGXhO5kvoo671jJwczWudKMaTQ64PNkXRh7CHIT3s"
            , "JD0Ef/61ODMKWJxYqD2J5kSxSXpHSqkA8lx5XRNWoxBEQuCLvTkpYRSp0WiVjCCR"
            , "EA9GsYtAltstnGh8V3LYEGT7qmCuHuHpkwuDvZFdKBAwahHGJUYJf8kpGFcVWuua"
            , "GteIjNVhRHyLbVjkqo48YZD3pjtyMV6OVNPkcf5TilwCWucLAigA9y5w1UnPiZTP"
            , "nygHMVnIS497fUbAe1hR6zt6sLpT9j6c5IhwnbTMI3rTeN/Mx1hgfNCL3TtNrzVf"
            , "ltkA8QHSx30tM4jQqZS+mF8J5HBnBoztobqfK6so3wO3/cB8HW2UxK+yMRegbZe6"
            , "RJ/ggJi7EqB9u8NIOMd0zRmsAHRftwPYXT5I+0pkAyzPQ/XrwQKCAQEA7cVWL7zK"
            , "qjKvrOZlp0lZB24Cz/nB0UpcNQNym4XVUVwB2E0jHg84psUUoAG0AoDUlmK2hziB"
            , "ts9UYdLYe+YDG59vEC0vSByCOKx49XYId/oRo8ga1/dCi4c+zoDC+bGxmZciePC0"
            , "Pl8+miaEqPoFLl+Ak/NqQcsuRKy4VhUhrBj19wwNa7T1cNjSrT52LWWYr5iAQaI5"
            , "/ZCUZ48VWEBst5IzyGNCjbo+WGLlN0XPikfUHg6fFd60Aq9W6M6EZkMFkgJsFCQ1"
            , "0NXcQH4qyuvslmnaUC1jMgQbDwhYetPVepDQ9PfGOVwado2Jc8pfyq4pS9z82n/T"
            , "ypaIPNyaS/ivuQKCAQEA+qonZLmg4f5MP3EQDocXD0BKvru84Y64WCPO0JGVAA4W"
            , "Ax6gezxZxsE53HN8tP6/caBgB1BRctJdPTAZASdH6oYliHbIgfBp7A5qKc1/4uLN"
            , "OwMGRCV5d2ZjLe/ge/ttnPaWPn/8UIlZbPLYbrzoPWijT7mBwSryNje3iaUaZ+H5"
            , "U+2UDTVegmvYgu6nRBTtloxsYGyUMgoILs0ZmfOkSu9FjI3vMBz3t9KvwwoGAHGb"
            , "Nzr2J3DOA1vkiNoeO1a4LuthMdHPFeInq6/QGpHrSdXdZ+kDQJN2yS4FrmALajK0"
            , "lv8O+M/mPo4yL5PdDyYCwMqXmmwqeTu3VCQ8mZ9w4wKCAQEA0HAqOfcbA1EjyELB"
            , "4oDeVzENrhyY2EyDwMWg9s3AT26E/4W/v15An5ItnodfssIVmzA2KxxSOlxO8OLR"
            , "fOwK1XBTAK9bv6+eSOiJsUUj72l9hUKqpaDQ6VhKquUeB57KvoZLZhI9MGB2lNQn"
            , "f6qOWDTIlurQv5TKntF7V5t2xVOsbEJYkY81ixPIvK8BEpL54xWheegpRiw0rQQ2"
            , "BgOUSukUFBtQAMbAWON5ZZcBdYu6XA8dTyLIU4I6giRo1Oz0OSd+b7m+o7CXQMJZ"
            , "6UIhSUrKY3cmenY28lqylhqWDrhdNiLv1uDzIfK6CJPknd1HvoWf9DwTAoosLI6H"
            , "hQohoQKCAQB8DJL8PQVKch/n0ZtBpUcbLBUReTVLZucB746dFz7mzRHFr3V6J27C"
            , "dsOcvqZ7qAq0P8rmM7BrI4IzEsd9E6R6grK2axple8XfpW6Si3l7ngQ07RU/yk/n"
            , "cAsrI6gzk0U2xa2q9om2jZJs4W2Mt/4yzcLsZIC7SPafMEmrW8LKoEFUcKGxO7Uu"
            , "1BJNEbBDeBvwQkQsH+/jJ6XPC39c+f9XejwwB9nUNLG/DgwfeKlptM88sPg6oo/B"
            , "h8t8kn0dnpf92Ey8a/mQ+d/qDlLto0MOwkEU+HK8lX3Wp+j11+AuSTOn+ke9iJ2I"
            , "XbSmaQWgRo5cPKZ0LI2Ee7IJePloiUhDAoIBABqvBtwtSBAYSLbr3wRe9pb1e5St"
            , "KBFvlArGMgEBXyiWL51obgM84wfygm4uFOHa1hVCdSuXO/pQ4oUMvondt3yBXSJC"
            , "EbumsEcmuXazW44UTB04TtYaWffTxwGfiKzR02D3R3++1l2sjODIzTdTUSrnXpPY"
            , "PDIdzBooRc8ImJdXoGAF/H21wifRO2gE7IqQKyTbjO9NHVloy0Rk1IQ1SJkS+8ar"
            , "tpgD+I8sb6nAgZzIIpH602XDykCoTzI/uAOvtf1VuKNK6DLJK2z+d1vUAOxhzqAr"
            , "QM/ThFn9xv3RUppF4aGrnfCsldXfrxmwgLvOw3qxLOCk6mHOcInRjw4Qdpk="
            , "-----END PRIVATE KEY-----"
            ]
        certEd25519 = X509CertPem $ B8.intercalate "\n"
            [ "-----BEGIN CERTIFICATE-----"
            , "MIIBOzCB7KADAgECAgEBMAcGAytlcAUAMBQxEjAQBgNVBAMMCWxvY2FsaG9zdDAe"
            , "Fw0xOTAxMTEyMDE4MjVaFw0xOTAxMjEyMDE4MjVaMBQxEjAQBgNVBAMMCWxvY2Fs"
            , "aG9zdDAqMAUGAytlcAMhAIxryJq0NofN67ugnkRQIE/MQqml2hgWOfDg3XCb1/Z0"
            , "o2EwXzAMBgNVHQ8EBQMDB/wAMB0GA1UdJQQWMBQGCCsGAQUFBwMBBggrBgEFBQcD"
            , "AjAPBgNVHRMECDAGAQH/AgEAMB8GA1UdEQQYMBaCCWxvY2FsaG9zdIcJMTI3LjAu"
            , "MC4xMAcGAytlcAUAA0EA3IceiC7mDYX4HmFUyCHzip5tNvkQMJ7eDwXuod0NjeW7"
            , "u7HU1s1AZ8yCqkIm9E9p7SmFehytX38qmSk5KxvCAQ=="
            , "-----END CERTIFICATE-----"
            ]
        keyEd25519 = X509KeyPem $ B8.intercalate "\n"
            [ "-----BEGIN PRIVATE KEY-----"
            , "MC4CAQAwBQYDK2VwBCIEIPQZCpPI8qgkU/HlsIwQBC48QuXOl036aReJF6DFLLjR"
            , "-----END PRIVATE KEY-----"
            ]
