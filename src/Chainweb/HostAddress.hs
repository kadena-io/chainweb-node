{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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
-- allows to disambiguate domain names and IPv4 addresses.
--
-- IPv6 Addresses are partially supported. IPv6 address are parsed as described
-- in RFC4291, but embedding of IPv4 addresses is not supported. IPv6 addresses
-- are printed exactly as they where parsed. No normalization is performed. In
-- particular the recommendations from RFC5952 are not considered. For host
-- addresses RFC3986 and RFC 5952 are followed by requiring that IPv6 literals
-- are enclosed in square brackets. Anything else from RFC3986, which is
-- concerning URIs is ignored.
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
, portFromInt
, portToText
, portFromText
, pPort
, readPortBytes

-- * Hostnames
, Hostname
, hostnameBytes
, localhost
, localhostIPv4
, localhostIPv6
, anyIpv4
, broadcast
, loopback
, readHostnameBytes
, hostnameToText
, hostnameFromText
, unsafeHostnameFromText
, pHostname
, isReservedHostname
, isPrivateHostname
, isLocalIp

-- * HostAddresses
, HostAddress(..)
, hostAddressPort
, hostAddressHost
, hostAddressBytes
, readHostAddressBytes
, hostAddressToText
, hostAddressFromText
, unsafeHostAddressFromText
, pHostAddress
, pHostAddress'
, hostAddressToBaseUrl
, isPrivateHostAddress
, isReservedHostAddress

-- * HostPreference Utils
, hostPreferenceToText
, hostPreferenceFromText
) where

import Configuration.Utils hiding ((<?>))

import Control.DeepSeq (NFData)
import Control.Lens.TH (makeLenses)
import Control.Monad (guard)
import Control.Monad.Catch (MonadThrow(..), Exception)

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B8
import qualified Data.CaseInsensitive as CI
import Data.Hashable (Hashable(..))
import Data.IP
import Data.Streaming.Network.Internal
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word (Word16, Word8)

import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

import Servant.Client (BaseUrl(..), Scheme(..))

-- internal modules
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- Internal Parsers

data HostType = HostTypeName | HostTypeIPv4 | HostTypeIPv6
    deriving (Show, Eq, Ord, Generic, Hashable)

hostParser :: Parser HostType
hostParser
    = HostTypeName <$ hostNameParser
    <|> HostTypeIPv4 <$ ipV4Parser
    <|> HostTypeIPv6 <$ ipV6Parser
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

ipV6Parser :: Parser [Maybe Word16]
ipV6Parser = p0
  where
    p0 = l1 <$> elision <* endOfInput
        <|> l3 <$> elision <*> h16 <*> p2 6
        <|> l2 <$> h16 <*> p1 7
        <?> "IPv6address"

    p1 :: Int -> Parser [Maybe Word16]
    p1 0 = l0 <$ endOfInput <?> "IPv6 prefix: too many segments"
    p1 i = l1 <$> elision <* endOfInput
        <|> l3 <$> elision <*> h16 <*> p2 (i - 2)
        <|> l2 <$ ":" <*> h16 <*> p1 (i - 1)
        <?> "IPv6 prefix"

    p2 :: Int -> Parser [Maybe Word16]
    p2 0 = l0 <$ endOfInput <?> "IPv6 suffix: too many segments"
    p2 i = l2 <$ ":" <*> h16 <*> p2 (i - 1)
        <|> l0 <$ endOfInput
        <?> "IPv6 suffix"

    elision :: Parser (Maybe Word16)
    elision = Nothing <$ "::"

    h16 :: Parser (Maybe Word16)
    h16 = Just <$> do
        h <- hexadecimal @Integer
        guard $ h < int (maxBound @Word16)
        return (int h)
        <?> "h16"

    l0 = []
    l1 = pure
    l2 = (:)
    l3 a b t = a:b:t

portParser :: Parser Port
portParser = Port
    <$> (decimal >>= \(d :: Integer) -> int d <$ guard (d <= 2^(16 :: Int)-1))
    <?> "port"

parseBytes :: MonadThrow m => T.Text -> Parser a -> B8.ByteString -> m a
parseBytes name parser b = either (throwM . TextFormatException . msg) return
    $ parseOnly (parser <* endOfInput) b
  where
    msg e = "Failed to parse " <> sshow b <> " as " <> name <> ": "
        <> T.pack e

-- -------------------------------------------------------------------------- --
-- Port Numbers

newtype InvalidPortException = InvalidPortException Integer
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (NFData)

instance Exception InvalidPortException

newtype Port = Port Word16
    deriving (Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData)
    deriving newtype (Show, Real, Integral, Num, Bounded, Enum, ToJSON, FromJSON)

portFromInt :: MonadThrow m => Integral a => a -> m Port
portFromInt n
    | n >= 0 && n <= 2^(16 :: Int)-1 = return $ Port (int n)
    | otherwise = throwM $ InvalidPortException (int n)
{-# INLINE portFromInt #-}

readPortBytes :: MonadThrow m => B8.ByteString -> m Port
readPortBytes = parseBytes "port" portParser
{-# INLINE readPortBytes #-}

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

data Hostname
    = HostnameName (CI.CI B8.ByteString)
    | HostnameIPv4 (CI.CI B8.ByteString)
    | HostnameIPv6 (CI.CI B8.ByteString)
    deriving (Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData)

instance Show Hostname where
    show = B8.unpack . hostnameBytes

readHostnameBytes :: MonadThrow m => B8.ByteString -> m Hostname
readHostnameBytes b = parseBytes "hostname" parser b
  where
    parser = hostParser <* endOfInput >>= \case
        HostTypeName -> return $! HostnameName (CI.mk b)
        HostTypeIPv4 -> return $! HostnameIPv4 (CI.mk b)
        HostTypeIPv6 -> return $! HostnameIPv6 (CI.mk b)
{-# INLINE readHostnameBytes #-}

localhost :: Hostname
localhost = HostnameName "localhost"
{-# INLINE localhost #-}

-- | Using explicit IP addresses and not to "localhost" greatly improves
-- networking performance and Mac OS X.
--
localhostIPv4 :: Hostname
localhostIPv4 = HostnameIPv4 "127.0.0.1"
{-# INLINE localhostIPv4 #-}

-- | Using explicit IP addresses and not to "localhost" greatly improves
-- networking performance and Mac OS X.
--
localhostIPv6 :: Hostname
localhostIPv6 = HostnameIPv6 "::1"
{-# INLINE localhostIPv6 #-}

anyIpv4 :: Hostname
anyIpv4 = HostnameIPv4 "0.0.0.0"
{-# INLINE anyIpv4 #-}

loopback :: Hostname
loopback = HostnameIPv4 "127.0.0.1"
{-# INLINE loopback #-}

broadcast :: Hostname
broadcast = HostnameIPv4 "255.255.255.255"
{-# INLINE broadcast #-}

isPrivateHostname :: Hostname -> Bool
isPrivateHostname (HostnameIPv4 ip) = isPrivateIp (read $ B8.unpack $ CI.original ip)
isPrivateHostname h
    | h == localhost = True
    | h == localhostIPv4 = True
    | h == localhostIPv6 = True
    | otherwise = False

isReservedHostname :: Hostname -> Bool
isReservedHostname (HostnameIPv4 ip) = isReservedIp (read $ B8.unpack $ CI.original ip)
isReservedHostname h = isPrivateHostname h

isLocalIp :: IPv4 -> Bool
isLocalIp ip =
    isMatchedTo ip $ makeAddrRange (toIPv4 [127,0,0,0]) 8

isPrivateIp :: IPv4 -> Bool
isPrivateIp ip = or
    [ isMatchedTo ip $ makeAddrRange (toIPv4 [10,0,0,0]) 8
    , isMatchedTo ip $ makeAddrRange (toIPv4 [172,16,0,0]) 12
    , isMatchedTo ip $ makeAddrRange (toIPv4 [192,168,0,0]) 16
    ]

isReservedIp :: IPv4 -> Bool
isReservedIp ip = isLocalIp ip || isPrivateIp ip || or
    [ isMatchedTo ip $ makeAddrRange (toIPv4 [0,0,0,0]) 8
    , isMatchedTo ip $ makeAddrRange (toIPv4 [100,64,0,0]) 10
    , isMatchedTo ip $ makeAddrRange (toIPv4 [169,254,0,0]) 16
    , isMatchedTo ip $ makeAddrRange (toIPv4 [192,0,0,0]) 24
    , isMatchedTo ip $ makeAddrRange (toIPv4 [192,0,2,0]) 24
    , isMatchedTo ip $ makeAddrRange (toIPv4 [192,88,99,0]) 24
    , isMatchedTo ip $ makeAddrRange (toIPv4 [192,168,0,0]) 15
    , isMatchedTo ip $ makeAddrRange (toIPv4 [198,51,100,0]) 24
    , isMatchedTo ip $ makeAddrRange (toIPv4 [203,0,113,0]) 24
    , isMatchedTo ip $ makeAddrRange (toIPv4 [224,0,0,0]) 4
    , isMatchedTo ip $ makeAddrRange (toIPv4 [240,0,0,0]) 4
    , isMatchedTo ip $ makeAddrRange (toIPv4 [255,255,255,255]) 32
    ]

hostnameBytes :: Hostname -> B8.ByteString
hostnameBytes (HostnameName b) = CI.original b
hostnameBytes (HostnameIPv4 b) = CI.original b
hostnameBytes (HostnameIPv6 b) = CI.original b
{-# INLINE hostnameBytes #-}

hostnameToText :: Hostname -> T.Text
hostnameToText = T.decodeUtf8 . hostnameBytes
{-# INLINE hostnameToText #-}

hostnameFromText :: MonadThrow m => T.Text -> m Hostname
hostnameFromText = readHostnameBytes . T.encodeUtf8
{-# INLINE hostnameFromText #-}

unsafeHostnameFromText :: HasCallStack => T.Text -> Hostname
unsafeHostnameFromText = fromJuste . hostnameFromText
{-# INLINE unsafeHostnameFromText #-}

instance ToJSON Hostname where
    toJSON = toJSON . hostnameToText
    toEncoding = toEncoding. hostnameToText
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

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
hostAddressBytes a = host <> ":" <> sshow (_hostAddressPort a)
  where
    ha = _hostAddressHost a
    host = case ha of
        HostnameIPv6 _ -> "[" <> hostnameBytes ha <> "]"
        _ -> hostnameBytes ha
{-# INLINE hostAddressBytes #-}

readHostAddressBytes :: MonadThrow m => B8.ByteString -> m HostAddress
readHostAddressBytes bytes = parseBytes "hostaddress" (hostAddressParser bytes) bytes

-- | Parser a host address. The input bytestring isn't used for parsing but for
-- the constructing the reslt HostAddress.
--
hostAddressParser :: B8.ByteString -> Parser HostAddress
hostAddressParser b = HostAddress
    <$> hostnameParser'
    <* ":"
    <*> portParser
  where
    host = B8.init $ fst $ B8.breakEnd (== ':') b
    hostnameParser'
        = HostnameName (CI.mk host) <$ hostNameParser
        <|> HostnameIPv4 (CI.mk host) <$ ipV4Parser
        <|> HostnameIPv6 (CI.mk $ B8.init $ B8.tail host) <$ "[" <* ipV6Parser <* "]"
        <?> "host"

hostAddressToText :: HostAddress -> T.Text
hostAddressToText = T.decodeUtf8 . hostAddressBytes
{-# INLINE hostAddressToText #-}

hostAddressFromText :: MonadThrow m => T.Text -> m HostAddress
hostAddressFromText = readHostAddressBytes . T.encodeUtf8
{-# INLINE hostAddressFromText #-}

unsafeHostAddressFromText :: HasCallStack => T.Text -> HostAddress
unsafeHostAddressFromText = fromJuste . hostAddressFromText
{-# INLINE unsafeHostAddressFromText #-}

instance HasTextRepresentation HostAddress where
    toText = hostAddressToText
    {-# INLINE toText #-}
    fromText = hostAddressFromText
    {-# INLINE fromText #-}

hostAddressProperties :: KeyValue e kv => HostAddress -> [kv]
hostAddressProperties o =
    [ "hostname" .= _hostAddressHost o
    , "port" .= _hostAddressPort o
    ]
{-# INLINE hostAddressProperties #-}

instance ToJSON HostAddress where
    toJSON = object. hostAddressProperties
    toEncoding = pairs . mconcat . hostAddressProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

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

pHostAddress' :: Maybe String -> OptionParser HostAddress
pHostAddress' service = HostAddress <$> pHostname service <*> pPort service

hostAddressToBaseUrl :: Scheme -> HostAddress -> BaseUrl
hostAddressToBaseUrl s (HostAddress hn p) = BaseUrl s hn' p' ""
  where
    hn' = T.unpack $ hostnameToText hn
    p'  = fromIntegral p

isPrivateHostAddress :: HostAddress -> Bool
isPrivateHostAddress (HostAddress n _) = isPrivateHostname n
{-# INLINE isPrivateHostAddress #-}

isReservedHostAddress :: HostAddress -> Bool
isReservedHostAddress (HostAddress n _) = isReservedHostname n
{-# INLINE isReservedHostAddress #-}

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

