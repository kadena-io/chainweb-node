{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Chainweb.Test.HostAddress
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.Test.HostAddress
( properties

-- * Arbitrary Values
, arbitraryPort
, arbitraryDomainName
, arbitraryHostname
, arbitraryIpV4
, arbitraryIpV6
, arbitraryHostAddress

) where

import Control.Monad

import qualified Data.ByteString.Char8 as B8
import qualified Data.CaseInsensitive as CI
import qualified Data.List as L
import qualified Data.Text as T
import Data.Word

import Test.QuickCheck

-- internal modules
import Chainweb.HostAddress
import Chainweb.Utils


-- -------------------------------------------------------------------------- --
-- Arbitrary Values

arbitraryPort :: Gen Port
arbitraryPort = fromJuste . portFromInt <$> arbitrary @Word16

arbitraryHostname :: Gen Hostname
arbitraryHostname = oneof
    [ arbitraryIpV4
    , arbitraryIpV4
    , arbitraryDomainName
        --  Note that not every valid domain name is also a valid host name.
        --  Generally, a hostname has at least one associated IP address.
        --  Also, syntactic restriction apply for certain top-level domains.
    , pure (unsafeHostnameFromText "localhost")
    , pure localhost
    ]

arbitraryHostAddress :: Gen HostAddress
arbitraryHostAddress = HostAddress <$> arbitrary <*> arbitrary

-- | TODO should we exclude network, broadcast, otherwise special values?
--
arbitraryIpV4 :: Gen Hostname
arbitraryIpV4 = unsafeHostnameFromText . T.intercalate "." . fmap sshow
    <$> replicateM 4 (arbitrary :: Gen Word8)

arbitraryIpV6 :: Gen Hostname
arbitraryIpV6 = unsafeHostnameFromText . T.intercalate ":" . fmap sshow
    <$> replicateM 8 (arbitrary :: Gen Word8)

arbitraryDomainName :: Gen Hostname
arbitraryDomainName = sized $ \n -> resize (min n 254)
    . fmap (fromJuste . readHostnameBytes . CI.original .  mconcat . L.intersperse ".")
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
-- Arbitrary Instances

instance Arbitrary Port where
    arbitrary = arbitraryPort

instance Arbitrary Hostname where
    arbitrary = arbitraryHostname

instance Arbitrary HostAddress where
    arbitrary = arbitraryHostAddress

-- -------------------------------------------------------------------------- --
-- Properties

prop_readHostnameBytes :: Hostname -> Property
prop_readHostnameBytes h = readHostnameBytes (hostnameBytes h) === Just h

prop_readHostAddressBytes :: HostAddress -> Property
prop_readHostAddressBytes a = readHostAddressBytes (hostAddressBytes a) === Just a

properties :: [(String, Property)]
properties =
    [ ("readHostnameBytes", property prop_readHostnameBytes)
    , ("readHostAddressBytes", property prop_readHostAddressBytes)
    ]
