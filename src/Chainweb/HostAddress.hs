{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Chainweb.HostAddress
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Port numbers must be within the range @[0,2^16-1]@.
--
-- All hostnames are considered fully qualified and thus the final dot is
-- omitted.
--
-- For hostnames we follow the specification for "Server-based Naming Authority"
-- for URIs from RFC2396 section 3.2.2.:
--
-- @
--      hostport      = host [ ":" port ]
--      host          = hostname | IPv4address
--      hostname      = *( domainlabel "." ) toplabel [ "." ]
--      domainlabel   = alphanum | alphanum *( alphanum | "-" ) alphanum
--      toplabel      = alpha | alpha *( alphanum | "-" ) alphanum
--
--      IPv4address   = 1*digit "." 1*digit "." 1*digit "." 1*digit
--      port          = *digit
-- @
--
-- @1*digit@ designates the decimal representation of an octet. The specification
-- takes the form of hostnames from section 2.1 RFC1123, but limiting the
-- rightmost (top-most) label to the from given in section 3 of RFC1034, which
-- allows to disambiguate domain names and IPv4 addresses. IPv6 Addresses are
-- not supported.
--
-- Additional restriction for hostname apply from RFC1123: labels must have not
-- more than 63 octets, letters are case-insenstive. The maximum length must not
-- exceed 254 octets, excluding the (optional) terminating dot.
--
-- See <https://cs.uwaterloo.ca/twiki/view/CF/HostNamingRules> for an extensive
-- overview of different standards for host names.
--
-- Non-ascii characters are encoded via Punycode and are of no concern in this
-- implementation.
--
module Chainweb.HostAddress
(
-- * Port Numbers
  Port
, portToText
, portFromText
, pPort

-- * Hostnames
, Hostname
, hostnameBytes
, localhost
, readHostnameBytes
, hostnameToText
, hostnameFromText
, unsafeHostnameFromText
, pHostname

-- * HostAddresses
, HostAddress(..)
, hostAddressPort
, hostAddressHost
, hostAddressBytes
, readHostAddressBytes
, hostAddressToText
, hostAddressFromText
, unsafeHostAddressFromText
, arbitraryHostAddress
, pHostAddress

-- * Arbitrary Values
, arbitraryPort
, arbitraryDomainName
, arbitraryHostname
, arbitraryIpV4
, arbitraryIpV6

-- * HostPreference Utils
, hostPreferenceToText
, hostPreferenceFromText

-- * Properties
, properties
) where

import Configuration.Utils

import Control.DeepSeq
import Control.Lens.TH
import Control.Monad
import Control.Monad.Catch

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B8
import qualified Data.CaseInsensitive as CI
import Data.Hashable
import qualified Data.List as L
import Data.Maybe
import Data.Streaming.Network.Internal
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word (Word8, Word16)

import GHC.Generics

import Test.QuickCheck

-- internal modules
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- Internal Parsers

hostParser :: Parser ()
hostParser = ()
    <$ (hostNameParser <|> () <$ ipV4Parser)
    <?> "host"

hostNameParser :: Parser ()
hostNameParser = ()
    <$ many' (domainlabel <* ".") <* toplabel
    <?> "hostname"
  where
    domainlabel = ()
        <$ alphanum <* optional labelTail
        <?> "domainlabel"

    toplabel = ()
        <$ alpha <* optional labelTail
        <?> "toplabel"

    labelTail = alphanumhyphen >>= \case
        '-' -> labelTail
        _ -> () <$ optional labelTail

    alpha = satisfy isAlpha_ascii
        <?> "alpha"

    alphanum = satisfy (\c -> isAlpha_ascii c || isDigit c)
        <?> "alphanum"

    alphanumhyphen = satisfy (\c -> isAlpha_ascii c || isDigit c || c == '-')
        <?> "alphahumhypen"

ipV4Parser :: Parser (Word8, Word8, Word8, Word8)
ipV4Parser = (,,,)
    <$> (octet <* ".") <*> (octet <* ".") <*> (octet <* ".") <*> octet
    <?> "ipv4address"
  where
    octet :: Parser Word8
    octet = (decimal >>= \(d :: Integer) -> int d <$ guard (d < 256))
        <?> "octet"

portParser :: Parser Port
portParser = Port
    <$> (decimal >>= \(d :: Integer) -> int d <$ guard (d < 2^(16 :: Int) -1))
    <?> "port"

-- -------------------------------------------------------------------------- --
-- Arbitrary Values

-- | TODO should we exclude network, broadcast, otherwise special values?
--
arbitraryIpV4 :: Gen Hostname
arbitraryIpV4 = Hostname . CI.mk . B8.intercalate "." . fmap sshow
    <$> replicateM 4 (arbitrary :: Gen Word8)

arbitraryIpV6 :: Gen Hostname
arbitraryIpV6 = Hostname . CI.mk . B8.intercalate "." . fmap sshow
    <$> replicateM 8 (arbitrary :: Gen Word8)

arbitraryDomainName :: Gen Hostname
arbitraryDomainName = sized $ \n -> resize (min n 254)
    . fmap (Hostname . mconcat . L.intersperse ".")
    $ (<>)
        <$> listOf (arbitraryDomainLabel False)
        <*> vectorOf 1 (arbitraryDomainLabel True)

-- TODO add frequency or used sized to yield a better distribution
--
arbitraryDomainLabel :: Bool -> Gen (CI.CI B8.ByteString)
arbitraryDomainLabel isTop = sized $ \n -> resize (min n 63)
    $ CI.mk . B8.pack <$> oneof
        [ vectorOf 1 (if isTop then letter else letterOrDigit)
        , foldM (\l a -> (l <>) <$> a) []
            [ vectorOf 1 (if isTop then letter else letterOrDigit)
            , listOf letterOrDigitOrHyphen
            , vectorOf 1 letterOrDigit
            ]
        ]
  where
    letter = elements $ ['a'..'z'] <> ['A'..'Z']
    letterOrDigit = elements $ ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']
    letterOrDigitOrHyphen = elements $ ['a'..'z'] <> ['A'..'Z'] <> ['-']

-- -------------------------------------------------------------------------- --
-- Port Numbers

newtype Port = Port Word16
    deriving (Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData)
    deriving newtype (Show, Real, Integral, Num, Bounded, Enum, ToJSON, FromJSON)

readPortBytes :: MonadThrow m => B8.ByteString -> m Port
readPortBytes = either (throwM . TextFormatException . T.pack) return
    . parseOnly (portParser <* endOfInput)
{-# INLINE readPortBytes #-}

arbitraryPort :: Gen Port
arbitraryPort = Port <$> arbitrary

instance Arbitrary Port where
    arbitrary = arbitraryPort

portToText :: Port -> T.Text
portToText = sshow
{-# INLINE portToText #-}

portFromText :: MonadThrow m => T.Text -> m Port
portFromText = readPortBytes . T.encodeUtf8
{-# INLINE portFromText #-}

instance HasTextRepresentation Port where
    toText = portToText
    {-# INLINE toText #-}
    fromText = portFromText
    {-# INLINE fromText #-}

pPort :: Maybe String -> OptionParser Port
pPort service = textOption
    % prefixLong service "port"
    <> suffixHelp service "port number"
{-# INLINE pPort #-}

-- -------------------------------------------------------------------------- --
-- Hostnames

newtype Hostname = Hostname (CI.CI B8.ByteString)
    deriving (Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData)
    deriving newtype (Show)

readHostnameBytes :: MonadThrow m => B8.ByteString -> m Hostname
readHostnameBytes b = Hostname
    <$> either (throwM . TextFormatException . T.pack) return (parseOnly parser b)
  where
    parser = CI.mk b <$ hostParser <* endOfInput
{-# INLINE readHostnameBytes #-}

localhost :: Hostname
localhost = Hostname "localhost"
{-# INLINE localhost #-}

hostnameBytes :: Hostname -> B8.ByteString
hostnameBytes (Hostname b) = CI.original b
{-# INLINE hostnameBytes #-}

arbitraryHostname :: Gen Hostname
arbitraryHostname = oneof
    [ arbitraryIpV4
    , arbitraryDomainName
        --  Note that not every valid domain name is also a valid host name.
        --  Generally, a hostname has at least one associated IP address.
        --  Also, syntactic restriction apply for certain top-level domains.
    , pure localhost
    ]

hostnameToText :: Hostname -> T.Text
hostnameToText = T.decodeUtf8 . hostnameBytes
{-# INLINE hostnameToText #-}

hostnameFromText :: MonadThrow m => T.Text -> m Hostname
hostnameFromText = readHostnameBytes . T.encodeUtf8
{-# INLINE hostnameFromText #-}

unsafeHostnameFromText :: T.Text -> Hostname
unsafeHostnameFromText = fromJust . hostnameFromText
{-# INLINE unsafeHostnameFromText #-}

instance ToJSON Hostname where
    toJSON = toJSON . hostnameToText
    {-# INLINE toJSON #-}

instance FromJSON Hostname where
    parseJSON = parseJsonFromText "Hostname"
    {-# INLINE parseJSON #-}

instance HasTextRepresentation Hostname where
    toText = hostnameToText
    {-# INLINE toText #-}
    fromText = hostnameFromText
    {-# INLINE fromText #-}

pHostname :: Maybe String -> OptionParser Hostname
pHostname service = textOption
    % prefixLong service "hostname"
    <> suffixHelp service "hostname"
{-# INLINE pHostname #-}

instance Arbitrary Hostname where
    arbitrary = arbitraryHostname

prop_readHostnameBytes :: Hostname -> Property
prop_readHostnameBytes h = readHostnameBytes (hostnameBytes h) === Just h

-- -------------------------------------------------------------------------- --
-- Host Addresses

data HostAddress = HostAddress
    { _hostAddressHost :: !Hostname
    , _hostAddressPort :: !Port
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData)

makeLenses ''HostAddress

hostAddressBytes :: HostAddress -> B8.ByteString
hostAddressBytes a = hostnameBytes (_hostAddressHost a)
    <> ":" <> sshow (_hostAddressPort a)
{-# INLINE hostAddressBytes #-}

readHostAddressBytes :: MonadThrow m => B8.ByteString -> m HostAddress
readHostAddressBytes bytes = do
    let (h,p) = B8.break (== ':') bytes
    HostAddress <$> readHostnameBytes h <*> readPortBytes (B8.drop 1 p)

hostAddressToText :: HostAddress -> T.Text
hostAddressToText = T.decodeUtf8 . hostAddressBytes
{-# INLINE hostAddressToText #-}

hostAddressFromText :: MonadThrow m => T.Text -> m HostAddress
hostAddressFromText = readHostAddressBytes . T.encodeUtf8
{-# INLINE hostAddressFromText #-}

unsafeHostAddressFromText :: T.Text -> HostAddress
unsafeHostAddressFromText = fromJust . hostAddressFromText
{-# INLINE unsafeHostAddressFromText #-}

instance HasTextRepresentation HostAddress where
    toText = hostAddressToText
    {-# INLINE toText #-}
    fromText = hostAddressFromText
    {-# INLINE fromText #-}

instance ToJSON HostAddress where
    toJSON o = object
        [ "hostname" .= _hostAddressHost o
        , "port" .= _hostAddressPort o
        ]
    {-# INLINE toJSON #-}

instance FromJSON HostAddress where
    parseJSON = withObject "HostAddress" $ \o -> HostAddress
        <$> o .: "hostname"
        <*> o .: "port"
    {-# INLINE parseJSON #-}

instance FromJSON (HostAddress -> HostAddress) where
    parseJSON = withObject "HostAddress" $ \o -> id
        <$< hostAddressHost ..: "hostname" % o
        <*< hostAddressPort ..: "port" % o
    {-# INLINE parseJSON #-}

pHostAddress :: Maybe String -> MParser HostAddress
pHostAddress service = id
    <$< hostAddressHost .:: pHostname service
    <*< hostAddressPort .:: pPort service

arbitraryHostAddress :: Gen HostAddress
arbitraryHostAddress = HostAddress <$> arbitrary <*> arbitrary

instance Arbitrary HostAddress where
    arbitrary = arbitraryHostAddress

prop_readHostAddressBytes :: HostAddress -> Property
prop_readHostAddressBytes a = readHostAddressBytes (hostAddressBytes a) === Just a

-- -------------------------------------------------------------------------- --
-- Host Preference Utils

hostPreferenceToText :: HostPreference -> T.Text
hostPreferenceToText HostAny = "*"
hostPreferenceToText HostIPv4 = "*4"
hostPreferenceToText HostIPv4Only = "!4"
hostPreferenceToText HostIPv6 = "*6"
hostPreferenceToText HostIPv6Only = "!6"
hostPreferenceToText (Host s) = T.pack s

hostPreferenceFromText :: MonadThrow m => T.Text -> m HostPreference
hostPreferenceFromText "*" = return HostAny
hostPreferenceFromText "*4" = return HostIPv4
hostPreferenceFromText "!4" = return HostIPv4Only
hostPreferenceFromText "*6" = return HostIPv6
hostPreferenceFromText "!6" = return HostIPv6Only
hostPreferenceFromText s = Host . T.unpack . toText <$> hostnameFromText s

-- Orphan instance
--
instance HasTextRepresentation HostPreference where
    toText = hostPreferenceToText
    {-# INLINE toText #-}
    fromText = hostPreferenceFromText
    {-# INLINE fromText #-}

-- -------------------------------------------------------------------------- --
-- Properties

properties :: [(String, Property)]
properties =
    [ ("readHostnameBytes", property prop_readHostnameBytes)
    , ("readHostAddressBytes", property prop_readHostAddressBytes)
    ]

