{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Network.X509.SelfSigned
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Kadena Chainweb Team <chainweb-dev@kadena.io>
-- Stability: experimental
--
-- Tools for generating self-signed certificates and using them with warp-tls
-- and http-client-tls.
--
module Network.X509.SelfSigned
(
-- * Supported X509 Certificate Types
  Ed25519Cert
, Ed448Cert
, RsaCert
, P256Cert
, DefCertType
, X509Key

-- * Generate PEM encoded self-signed X509 Certificates
, generateSelfSignedCertificate
, generateLocalhostCertificate
, makeCertificate

-- * Client Support
, TlsPolicy(..)
, ServiceID
, unsafeMakeCredential
, certificateCacheManagerSettings
, isCertificateMismatchException

-- * Server Settings
, tlsServerSettings
, tlsServerChainSettings

-- * Low level Utils

-- ** Distinguished Name and Alt Names
, name
, org
, AltName(..)

-- ** Certificate Fingerprints
, FingerprintByteCount
, fingerprintByteCount
, Fingerprint(..)
, fingerprint
, fingerprintToText
, fingerprintFromText
, unsafeFingerprintFromText
, fingerprintPem
, unsafeFingerprintPem

-- ** PEM encoded Certificate
, X509CertPem(..)
, x509CertPemToText
, x509CertPemFromText
, pX509CertPem
, unsafeX509CertPemFromText
, validateX509CertPem
, decodePemX509Cert

-- ** PEM encoded Key
, X509KeyPem(..)
, x509KeyPemToText
, x509KeyPemFromText
, pX509KeyPem
, unsafeX509KeyPemFromText
, validateX509KeyPem
, decodePemX509Key

-- ** PEM encode certificate chain
, X509CertChainPem(..)
, x509CertChainPemToText
, x509CertChainPemFromText
, pX509CertChainPem
, unsafeX509CertChainPemFromText
, validateX509CertChainPem
) where

import Configuration.Utils

import Control.DeepSeq
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Error.Class

import Crypto.Hash.Algorithms (SHA512(..))
import qualified Crypto.Number.Serialize as EC (i2ospOf_)
import qualified Crypto.PubKey.ECC.ECDSA as EC
import qualified Crypto.PubKey.ECC.Generate as EC (generate)
import qualified Crypto.PubKey.ECC.Types as EC
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Crypto.PubKey.Ed448 as Ed448
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as RSA (signSafer)
import qualified Crypto.PubKey.RSA.Types as RSA (KeyPair(..), toPublicKey)
import Crypto.Random.Types (MonadRandom)

import Data.ASN1.BinaryEncoding (DER(..))
import Data.ASN1.Encoding (decodeASN1', encodeASN1')
import Data.ASN1.Types
import Data.Bifunctor
import Data.ByteArray (ByteArray, convert)
import qualified Data.ByteString as B (ByteString, length, pack)
import qualified Data.ByteString.Char8 as B8
import Data.Foldable
import Data.Hashable
import Data.Hourglass (DateTime, durationHours, timeAdd)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.PEM (PEM(..), pemParseBS, pemWriteBS)
import Data.Proxy
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.X509
import Data.X509.Validation (Fingerprint(..), ServiceID, getFingerprint)

import GHC.Generics
import GHC.Stack
import GHC.TypeNats

import Network.Connection (TLSSettings(..))
import Network.HTTP.Client (ManagerSettings, HttpException(HttpExceptionRequest), HttpExceptionContent(InternalException))
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.TLS hiding (HashSHA256, HashSHA512, SHA512)
import Network.TLS.Extra (ciphersuite_default)
import Network.Wai.Handler.WarpTLS as WARP
    (TLSSettings(..), tlsSettingsChainMemory, tlsSettingsMemory)

import System.Hourglass (dateCurrent)
import System.X509 (getSystemCertificateStore)

-- internal modules

import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- X509 Certificate Types

#if WITH_ED25519
type DefCertType = Ed25519Cert
#else
type DefCertType = RsaCert
#endif

-- | The signature algorithm Ed25519. EdDSA signature algorithms are not yet
-- supported by most clients. It is part of TLSv1.3 and supported in openssl-1.1
-- (with TLSv1.2 and TLS1.3) and the master branch (as of 2019-01-10) of Haskell
-- tls (with TLSv1.2 and TLSv1.3). Also, the under high load the performance of
-- the Haskell tls package is better with 4096 bit RSA certificates than with
-- ED25519 certificates.
--
type Ed25519Cert = Ed25519.SecretKey

-- | The signature algorithm Ed25519. EdDSA signature algorithms are not yet
-- supported by most clients. It is part of TLSv1.3 and supported in openssl-1.1
-- (with TLSv1.2 and TLS1.3) and the master branch (as of 2019-01-10) of Haskell
-- tls (with TLSv1.2 and TLSv1.3).
--
type Ed448Cert = Ed448.SecretKey

-- | RSA signature algorithm. Generating RSA keys is somewhat slow and the
-- (sufficiently secure) public keys are large. But currently it is still the
-- most widely supported certificate type. For the Haskell tls <= 1.4.1 it seems
-- that this is the only certificate type that is fully supported on the server
-- side.
--
type RsaCert = RSA.PrivateKey

-- | ECDSA signature algorithm. p-256 (or prime256v1 or secp256r1) is used with
-- ecdsa. it is supported by most standard clients, but disabled as signing
-- algorithm on the server-side of the Haskell tls package due to an timing
-- attack vulnerability in the generic ECDSA implementation.
--
-- https://github.com/ocheron/cryptonite/commits/tc-ecdsa
-- https://github.com/ocheron/hs-tls/commits/ecdsa-signing
--
-- The former resolves the timing attack vulnerability for P-256 (SEC_p521r1) in
-- the cryptonite package. The latter implements signing with ECDSA certificates
-- in the tls package. With the increasing support for Ed25519 and Ed448 accross
-- the internet it is not clear if support for ECDSA with NIST curves will ever
-- be added to the Haskell tls package.
--
type P256Cert = EC.KeyPair

-- -------------------------------------------------------------------------- --
-- EC Utils

serializePoint :: HasCallStack => EC.Curve -> EC.Point -> SerializedPoint
serializePoint _ EC.PointO = error "can't serialize EC point at infinity"
serializePoint curve (EC.Point x y) = SerializedPoint
    $ B.pack [4] <> EC.i2ospOf_ bytes x <> EC.i2ospOf_ bytes y
  where
    bits  = EC.curveSizeBits curve
    bytes = (bits + 7) `div` 8

-- | The ECDSA implementation in cryptonite as of version 0.25 is vulnerable to
-- timing attacks. It is thus not supported server-side in the Haskell tls
-- package.
--
-- The PR https://github.com/haskell-crypto/cryptonite/pull/226 addresses this
-- for curve P-256 / SEC_p256r1.
--
ecCurveName :: EC.CurveName
ecCurveName = EC.SEC_p256r1
-- ecCurveName = EC.SEC_p384r1
-- ecCurveName = EC.SEC_p521r1

-- -------------------------------------------------------------------------- --
-- X509 Certificate Type instances

class X509Key k where
    type PK k

    generateKey :: IO k
    publicKey :: k -> PK k
    getPubKey :: k -> PubKey
    getPrivKey :: k -> PrivKey
    sigAlg :: SignatureALG

    signIO
        :: MonadRandom m
        => ByteArray a
        => k
        -> B.ByteString
        -> m (a, SignatureALG)

    pemKeyHeader :: String

-- | EC
--
-- The ECDSA implementation in cryptonite as of version 0.25 is vulnerable to
-- timing attacks. It is thus not supported in the Haskell tls package.
--
-- The PR https://github.com/haskell-crypto/cryptonite/pull/226 addresses this
-- for curve P-256 / SEC_p256r1.
--
instance X509Key EC.KeyPair where
    type PK EC.KeyPair = EC.PublicPoint

    generateKey = uncurry f <$> EC.generate (EC.getCurveByName ecCurveName)
      where
        f pk sk = EC.KeyPair
            (EC.getCurveByName ecCurveName)
            (EC.public_q pk)
            (EC.private_d sk)

    publicKey (EC.KeyPair _ pk _) = pk

    getPubKey (EC.KeyPair _ pk _) = PubKeyEC
        $ PubKeyEC_Named ecCurveName
        $ serializePoint (EC.getCurveByName ecCurveName) pk

    getPrivKey (EC.KeyPair _ _ sk) = PrivKeyEC
        $ PrivKeyEC_Named ecCurveName sk

    sigAlg = SignatureALG HashSHA512 PubKeyALG_EC

    pemKeyHeader = "EC PRIVATE KEY"

    signIO sk bytes = do
        sig <- encodeEcSignatureDer <$> EC.sign (EC.toPrivateKey sk) SHA512 bytes
        return (convert sig, sigAlg @EC.KeyPair)

-- | Ed25519
--
instance X509Key Ed25519.SecretKey where
    type PK Ed25519.SecretKey = Ed25519.PublicKey

    generateKey = Ed25519.generateSecretKey
    publicKey = Ed25519.toPublic
    getPubKey = PubKeyEd25519 . publicKey
    getPrivKey = PrivKeyEd25519
    sigAlg = SignatureALG_IntrinsicHash PubKeyALG_Ed25519
    pemKeyHeader = "PRIVATE KEY"

    signIO sk bytes = return (convert sig, sigAlg @Ed25519.SecretKey)
      where
        sig = Ed25519.sign sk (publicKey sk) bytes

-- | Ed448
--
instance X509Key Ed448.SecretKey where
    type PK Ed448.SecretKey = Ed448.PublicKey

    generateKey = Ed448.generateSecretKey
    publicKey = Ed448.toPublic
    getPubKey = PubKeyEd448 . publicKey
    getPrivKey = PrivKeyEd448
    sigAlg = SignatureALG_IntrinsicHash PubKeyALG_Ed448
    pemKeyHeader = "PRIVATE KEY"

    signIO sk bytes = return (convert sig, sigAlg @Ed448.SecretKey)
      where
        sig = Ed448.sign sk (publicKey sk) bytes

-- | RSA (4096 bit keys size)
--
instance X509Key RSA.PrivateKey where
    type PK RSA.PrivateKey = RSA.PublicKey

    generateKey = snd <$> RSA.generate 512 0x10001
    publicKey = RSA.toPublicKey . RSA.KeyPair
    getPubKey = PubKeyRSA . publicKey
    getPrivKey = PrivKeyRSA
    pemKeyHeader = "PRIVATE KEY"

    signIO sk bytes = do
        sig <- RSA.signSafer (Just SHA512) sk bytes >>= \case
            Left e -> error $ "Network.X509.SelfSigned: X509Key instance for RSA.PrivateKey: signIO: " <>  show e
            Right x -> return $! x
        return (convert sig, sigAlg @RSA.PrivateKey)

    sigAlg = SignatureALG HashSHA512 PubKeyALG_RSA

-- -------------------------------------------------------------------------- --
-- Unsigned tbs Certificate

makeCertificate
    :: forall k
    . X509Key k
    => DateTime
    -> DateTime
    -> DistinguishedName
    -> DistinguishedName
    -> Maybe (NE.NonEmpty AltName)
        -- ^ the list of altnames must include the CN name from the subject
        -- distinguished name.
    -> PubKey
    -> Certificate
makeCertificate start end issuer subject altNames pk = Certificate
    { certVersion = 2
    , certSerial = 1
    , certSignatureAlg = sigAlg @k
    , certIssuerDN = issuer
    , certValidity = (start, end)
    , certSubjectDN = subject
    , certPubKey = pk
    , certExtensions = Extensions $ Just $ rawExtensions altNames
    }

name :: String -> DistinguishedName
name n = DistinguishedName [(getObjectID DnCommonName, fromString n)]

org :: String -> DistinguishedName
org n = DistinguishedName [(getObjectID DnOrganization, fromString n)]

-- -------------------------------------------------------------------------- --
-- Signed Certificate

signedCertIO :: X509Key k => k -> Certificate -> IO (SignedExact Certificate)
signedCertIO sk = objectToSignedExactF (signIO sk)

-- -------------------------------------------------------------------------- --
-- Extensions

keyUsage :: ExtKeyUsage
keyUsage = ExtKeyUsage
    [ KeyUsage_digitalSignature
    , KeyUsage_keyCertSign
    , KeyUsage_keyAgreement
    , KeyUsage_keyEncipherment
    , KeyUsage_dataEncipherment
    , KeyUsage_nonRepudiation
    ]

keyPurpose :: ExtExtendedKeyUsage
keyPurpose = ExtExtendedKeyUsage
    [ KeyUsagePurpose_ServerAuth
    , KeyUsagePurpose_ClientAuth
    ]

{-
subjectKeyId :: B.ByteString -> ExtSubjectKeyId
subjectKeyId k = ExtSubjectKeyId k
-}

basicConstraints :: ExtBasicConstraints
basicConstraints = ExtBasicConstraints
    True {- CA flag -}
    (Just 0) {- path length -}

rawExtensions :: Maybe (NE.NonEmpty AltName) -> [ExtensionRaw]
rawExtensions altNames =
    [ extensionEncode False keyUsage
    , extensionEncode False keyPurpose
    , extensionEncode False basicConstraints
    ]
    <>
    maybe [] (return . extensionEncode False . ExtSubjectAltName . toList) altNames

-- -------------------------------------------------------------------------- --
-- Certificate Fingerprint

type FingerprintByteCount = 32

fingerprintByteCount :: Natural
fingerprintByteCount = natVal (Proxy @FingerprintByteCount)

fingerprint :: SignedExact Certificate -> Fingerprint
fingerprint = flip getFingerprint HashSHA256
    -- The TLS package uses SHA256 for certifcate fingerprints, so we do the
    -- same here. If this changes in the TLS package, we'd have to change
    -- our fingerprint format or compute SHA256 fingerprints in certificate
    -- cache queries from the provided certificate.

fingerprintToText :: Fingerprint -> T.Text
fingerprintToText (Fingerprint b) = encodeB64UrlNoPaddingText b
{-# INLINE fingerprintToText #-}

fingerprintFromText :: MonadThrow m => T.Text -> m Fingerprint
fingerprintFromText t = do
    !bytes <- decodeB64UrlNoPaddingText t
    unless (B.length bytes == int fingerprintByteCount) $ throwM
        $ TextFormatException
        $ "wrong certificate fingerprint length: expected "
        <> sshow fingerprintByteCount <> " bytes, got "
        <> sshow (B.length bytes) <> " bytes."
    return $! Fingerprint bytes
{-# INLINE fingerprintFromText #-}

unsafeFingerprintFromText :: HasCallStack => String -> Fingerprint
unsafeFingerprintFromText = fromJuste . fingerprintFromText . T.pack
{-# INLINE unsafeFingerprintFromText #-}

-- -------------------------------------------------------------------------- --
-- PEM Encoded Certificate

newtype X509CertPem = X509CertPem B.ByteString
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData)

x509CertPemToText :: X509CertPem -> T.Text
x509CertPemToText (X509CertPem b) = T.decodeUtf8 b
{-# INLINE x509CertPemToText #-}

x509CertPemFromText :: MonadThrow m => T.Text -> m X509CertPem
x509CertPemFromText t = return $! X509CertPem $! T.encodeUtf8 t
{-# INLINE x509CertPemFromText #-}

unsafeX509CertPemFromText :: HasCallStack => String -> X509CertPem
unsafeX509CertPemFromText = fromJuste . x509CertPemFromText . T.pack
{-# INLINE unsafeX509CertPemFromText #-}

instance HasTextRepresentation X509CertPem where
    toText = x509CertPemToText
    {-# INLINE toText #-}
    fromText = x509CertPemFromText
    {-# INLINE fromText #-}

instance ToJSON X509CertPem where
    toJSON = toJSON . toText
    toEncoding = toEncoding . toText
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON X509CertPem where
    parseJSON = parseJsonFromText "X509CertPem"
    {-# INLINE parseJSON #-}

pX509CertPem :: Maybe String -> OptionParser X509CertPem
pX509CertPem service = textOption
    % prefixLong service "certificate"
    <> suffixHelp service "PEM encoded X509 certificate of the local peer"
{-# INLINE pX509CertPem #-}

validateX509CertPem :: MonadError T.Text m => X509CertPem -> m ()
validateX509CertPem pemCert =
    case decodePemX509Cert pemCert of
        Left e -> throwError $ sshow e
        Right _ -> return ()

decodePemX509Cert :: MonadThrow m => X509CertPem -> m (SignedExact Certificate)
decodePemX509Cert (X509CertPem bytes) =
    either (throwM . X509CertificateDecodeException . T.pack) return $ do
        der <- pemParseBS bytes >>= \case
            [] -> Left "no PEM encoded object found."
            (h:_) -> Right $ pemContent h
        decodeSignedObject der

fingerprintPem :: MonadThrow m => X509CertPem -> m Fingerprint
fingerprintPem = fmap fingerprint . decodePemX509Cert

unsafeFingerprintPem :: HasCallStack => X509CertPem -> Fingerprint
unsafeFingerprintPem = either (error . sshow) id . fingerprintPem

encodeCertDer :: SignedExact Certificate -> B.ByteString
encodeCertDer = encodeSignedObject

encodeCertPem :: SignedExact Certificate -> X509CertPem
encodeCertPem c = X509CertPem . pemWriteBS $ PEM
    { pemName = "CERTIFICATE"
    , pemHeader = []
    , pemContent = encodeCertDer c
    }

encodeEcSignatureDer :: EC.Signature -> B.ByteString
encodeEcSignatureDer (EC.Signature r s) = encodeASN1' DER
    [ Start Sequence
    , IntVal r
    , IntVal s
    , End Sequence
    ]

-- -------------------------------------------------------------------------- --
-- PEM Encoded Key

newtype X509KeyPem = X509KeyPem B.ByteString
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData)

x509KeyPemToText :: X509KeyPem -> T.Text
x509KeyPemToText (X509KeyPem b) = T.decodeUtf8 b
{-# INLINE x509KeyPemToText #-}

x509KeyPemFromText :: MonadThrow m => T.Text -> m X509KeyPem
x509KeyPemFromText t = return . X509KeyPem $ T.encodeUtf8 t
{-# INLINE x509KeyPemFromText #-}

unsafeX509KeyPemFromText :: HasCallStack => String -> X509KeyPem
unsafeX509KeyPemFromText = fromJuste . x509KeyPemFromText . T.pack
{-# INLINE unsafeX509KeyPemFromText #-}

instance HasTextRepresentation X509KeyPem where
    toText = x509KeyPemToText
    {-# INLINE toText #-}
    fromText = x509KeyPemFromText
    {-# INLINE fromText #-}

instance ToJSON X509KeyPem where
    toJSON = toJSON . toText
    toEncoding = toEncoding . toText
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON X509KeyPem where
    parseJSON = parseJsonFromText "X509KeyPem"
    {-# INLINE parseJSON #-}

pX509KeyPem :: Maybe String -> OptionParser X509KeyPem
pX509KeyPem service = textOption
    % prefixLong service "key"
    <> suffixHelp service "PEM encoded X509 certificate key of the local peer"
{-# INLINE pX509KeyPem #-}

encodeKeyDer :: X509Key k => k -> B.ByteString
encodeKeyDer sk = encodeASN1' DER $ (toASN1 $ getPrivKey sk) []

encodeKeyPem :: forall k . X509Key k => k -> X509KeyPem
encodeKeyPem sk = X509KeyPem . pemWriteBS $ PEM
    { pemName = pemKeyHeader @k
    , pemHeader = []
    , pemContent = encodeKeyDer sk
    }

validateX509KeyPem :: MonadError T.Text m => X509KeyPem -> m ()
validateX509KeyPem pemKey =
    case decodePemX509Key pemKey of
        Left e -> throwError $ sshow e
        Right _ -> return ()

decodePemX509Key :: MonadThrow m => X509KeyPem -> m PrivKey
decodePemX509Key (X509KeyPem bytes) =
    either (throwM . X509KeyDecodeException . T.pack) return $ do
        der <- pemParseBS bytes >>= \case
            [] -> Left "no PEM encoded object found."
            (h:_) -> Right $ pemContent h
        asn1 <- first sshow $ decodeASN1' DER der
        fst <$> fromASN1 asn1

-- -------------------------------------------------------------------------- --
-- Generate Self Signed Certificate

-- | Generate a self-signed Certificate for a given distinguished name and
-- optionally additional alternative names. Note, however, that for a most uses
-- of a self-signed cerificate the names don't matter. Instead the certificate
-- is authenticated by checking its fingerprint.
--
generateSelfSignedCertificate
    :: forall k
    . X509Key k
    => Natural
        -- ^ days of validity
    -> DistinguishedName
        -- ^ the subject of the certificate. This should include at least the CN
        -- name, which can be created with 'name'.

    -> Maybe (NE.NonEmpty AltName)
        -- ^ For self-signed certificates it's usually fine to leave this empty.
        -- If it's not empty it should include the CN value from
        -- the distinguished name.

    -> IO (Fingerprint, X509CertPem, X509KeyPem)
generateSelfSignedCertificate days dn altNames = do
    sk <- generateKey @k
    start <- dateCurrent
    let end = start `timeAdd` mempty { durationHours = 24 * fromIntegral days }
        c = makeCertificate @k start end dn dn altNames (getPubKey sk)
    sc <- signedCertIO sk c
    let !fp = fingerprint sc
    let !cpem = encodeCertPem sc
    let !kpem = encodeKeyPem sk
    return (fp, cpem, kpem)

-- -------------------------------------------------------------------------- --
-- Generate Self Signed Certificate for Localhost

-- | Generate a self-signed Certificate for localhost. Note, however, that for a
-- most uses of a self-signed cerificate the names don't matter. Instead the
-- certificate is authenticated by checking its fingerprint.
--
generateLocalhostCertificate
    :: forall k
    . X509Key k
    => Natural
        -- ^ days of validity
    -> IO (Fingerprint, X509CertPem, X509KeyPem)
generateLocalhostCertificate days
    = generateSelfSignedCertificate @k days (name "localhost") $ Just
        $ AltNameDNS "localhost" NE.:| [AltNameIP "\127\0\0\1"]

-- -------------------------------------------------------------------------- --
-- Client

-- | Create a client credential for a certificate
--
unsafeMakeCredential :: HasCallStack => X509CertChainPem -> X509KeyPem -> Credential
unsafeMakeCredential (X509CertChainPem cert chain) (X509KeyPem keyBytes) =
    case credentialLoadX509ChainFromMemory (x509Bytes cert) (x509Bytes <$> chain) keyBytes of
        Left e -> error $ "failed to read certificate or key: " <> e
        Right x -> x
  where
    x509Bytes (X509CertPem bytes) = bytes

-- | A certificate policy for using self-signed certifcates with a connection
-- manager
--
data TlsPolicy
    = TlsInsecure
    | TlsSecure
        { _tlsPolicyUseSystemStore :: !Bool
            -- ^ whether to use the system certificate store.
        , _tlsPolicyCallback :: !(ServiceID -> IO (Maybe Fingerprint))
            -- ^ a callback for looking up certificate fingerprints for known
            -- service endpoints.
        }

-- | Create a connection manager for a given 'TlsPolicy'.
--
certificateCacheManagerSettings
    :: TlsPolicy
    -> IO ManagerSettings
certificateCacheManagerSettings policy = do
    certstore <- getCertStore policy
    return $ mkManagerSettings
        (TLSSettings (settings certstore))
        Nothing
  where
    -- It is safe to pass empty strings for host and port since 'connectFromHandle'
    -- and 'connectTo' are going to overwrite this anyways.
    --
    settings certstore = (defaultParamsClient "" "")
        { clientSupported = defSupported
            { supportedCiphers = ciphersuite_default
            , supportedVersions = [TLS13, TLS12, TLS11, TLS10]
            }
        , clientShared = defShared
            { sharedCAStore = certstore
            , sharedValidationCache = validationCache policy
            }
        }
      where
        defSupported = clientSupported (defaultParamsClient "" "")
        defShared = clientShared (defaultParamsClient "" "")

    validationCache TlsInsecure = ValidationCache
        (\_ _ _ -> return ValidationCachePass)
        (\_ _ _ -> return ())

    validationCache (TlsSecure _ query) = certificateCache query

    getCertStore TlsInsecure = return mempty
    getCertStore (TlsSecure False _) = return mempty
    getCertStore (TlsSecure True _) = getSystemCertificateStore

-- -------------------------------------------------------------------------- --
-- Certifcate Fingerprint Cache

-- NOTE: the validation cache supports only a single peer-id/fingerprint per
-- host-address/serviceid.
--
certificateCache :: (ServiceID -> IO (Maybe Fingerprint)) -> ValidationCache
certificateCache query = ValidationCache queryCallback (\_ _ _ -> return ())
  where
    queryCallback :: ValidationCacheQueryCallback
    queryCallback serviceID fp _cert = query serviceID >>= \case
        Nothing -> return ValidationCacheUnknown
        Just f
            | fp == f -> return ValidationCachePass
            | otherwise -> return $ ValidationCacheDenied
                $ "for host: " <> fst serviceID <> ":" <> B8.unpack (snd serviceID)
                <> " expected fingerprint: " <> T.unpack (fingerprintToText f)
                <> " but got fingerprint: " <> T.unpack (fingerprintToText fp)

-- | Check whether a connection failed due to an certificate missmatch
--
isCertificateMismatchException :: HttpException -> Bool
isCertificateMismatchException (HttpExceptionRequest _ (InternalException e)) =
    case fromException e of
        Just (HandshakeFailed (Error_Protocol _msg CertificateUnknown)) -> True
        _ -> False
    -- _msg looks something like:
    --
    -- "certificate rejected: [
    --   CacheSaysNo \"for host: 54.93.103.7:443 expected fingerprint: aXv-A9r5FUbg7R9hQHG0huvVIuS0_HQacvMn-wIUS-M but got fingerprint: GqZr-fWw0zj68xll-HW9RcL46e1mq6wwwDjRuXPlrcA\"
    -- ]"
isCertificateMismatchException _ = False


-- -------------------------------------------------------------------------- --
-- Server Settings

-- | TLS server settings
--
tlsServerSettings
    :: X509CertPem
    -> X509KeyPem
    -> WARP.TLSSettings
tlsServerSettings (X509CertPem certBytes) (X509KeyPem keyBytes)
    = (tlsSettingsMemory certBytes keyBytes)
        { tlsCiphers = ciphersuite_default
        , tlsAllowedVersions = [TLS13, TLS12, TLS11, TLS10]
        }

-- | TLS server settings
--
tlsServerChainSettings
    :: X509CertChainPem
    -> X509KeyPem
    -> WARP.TLSSettings
tlsServerChainSettings (X509CertChainPem cert chain) (X509KeyPem keyBytes)
    = (tlsSettingsChainMemory (x509Bytes cert) (x509Bytes <$> chain) keyBytes)
        { tlsCiphers = ciphersuite_default
        , tlsAllowedVersions = [TLS13, TLS12, TLS11, TLS10]
        }
  where
    x509Bytes (X509CertPem bytes) = bytes

-- -------------------------------------------------------------------------- --
-- Split Certificate Chain into the head and the remaining certificats

pattern CertHeader :: B8.ByteString
pattern CertHeader = "-----BEGIN CERTIFICATE-----"

takeCert :: MonadThrow m => [B8.ByteString] -> m ([B8.ByteString], [B8.ByteString])
takeCert (CertHeader : t) = return $ first (CertHeader :) $ L.break (== CertHeader) t
takeCert _ = throwM $ DecodeException "failed to decode X509 PEM certificate. Missing header."

parseCerts :: MonadThrow m => B8.ByteString -> m [B8.ByteString]
parseCerts bytes = go (B8.lines bytes)
  where
    go [] = return []
    go l = do
        (h, t) <- takeCert l
        (B8.intercalate "\n" h :) <$> go t

parseCertChain :: MonadThrow m => B8.ByteString -> m X509CertChainPem
parseCertChain bytes = parseCerts bytes >>= \case
    [] -> throwM $ DecodeException "certificate must have at least one certificate"
    (h : t) -> return $ X509CertChainPem (X509CertPem h) (X509CertPem <$> t)

data X509CertChainPem = X509CertChainPem X509CertPem ![X509CertPem]
    deriving (Show, Eq, Ord, Generic, NFData)

x509CertChainPemToText :: X509CertChainPem -> T.Text
x509CertChainPemToText (X509CertChainPem a b) = T.intercalate "\n"
    $ x509CertPemToText a
    : (x509CertPemToText <$> b)
{-# INLINE x509CertChainPemToText #-}

x509CertChainPemFromText :: MonadThrow m => T.Text -> m X509CertChainPem
x509CertChainPemFromText = parseCertChain . T.encodeUtf8
{-# INLINE x509CertChainPemFromText #-}

unsafeX509CertChainPemFromText :: HasCallStack => String -> X509CertChainPem
unsafeX509CertChainPemFromText = unsafeFromText . T.pack
{-# INLINE unsafeX509CertChainPemFromText #-}

instance HasTextRepresentation X509CertChainPem where
    toText = x509CertChainPemToText
    {-# INLINE toText #-}
    fromText = x509CertChainPemFromText
    {-# INLINE fromText #-}

instance ToJSON X509CertChainPem where
    toJSON = toJSON . toText
    toEncoding = toEncoding . toText
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON X509CertChainPem where
    parseJSON = parseJsonFromText "X509CertChainPem"
    {-# INLINE parseJSON #-}

pX509CertChainPem :: Maybe String -> OptionParser X509CertChainPem
pX509CertChainPem service = textOption
    % prefixLong service "certificate-chain"
    <> suffixHelp service "PEM encoded X509 certificate or certificate chain of the local peer"
{-# INLINE pX509CertChainPem #-}

validateX509CertChainPem :: MonadError T.Text m => X509CertChainPem -> m ()
validateX509CertChainPem (X509CertChainPem a b) =
    case traverse_ decodePemX509Cert (a : b) of
        Left e -> throwError $ sshow e
        Right () -> return ()
