{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Chainweb.RestAPI.NetworkID
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.RestAPI.NetworkID
( NetworkId(..)
, networkIdToText
, networkIdFromText
, unsafeNetworkIdFromText
, pNetworkId

-- * Typelevel NetworkId
, NetworkIdT(..)
, SomeNetworkIdT(..)
, someNetworkIdVal
, KnownNetworkId(..)

-- * Singletons
, Sing(SChainNetwork, SCutNetwork)
, type SNetwork
) where

import Configuration.Utils

import Control.DeepSeq
import Control.Monad.Catch

import Data.Hashable
import Data.Maybe
import Data.Proxy
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup hiding (option)
#endif
import qualified Data.Text as T

import GHC.Generics (Generic)

import Test.QuickCheck

import Test.QuickCheck.Instances ({- Arbitrary V4.UUID -})

-- Internal imports

import Chainweb.ChainId
import Chainweb.Utils hiding (check)

import Data.Singletons

-- -------------------------------------------------------------------------- --
-- Network ID

data NetworkId
    = ChainNetwork ChainId
    | CutNetwork
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData)

networkIdToText :: NetworkId -> T.Text
networkIdToText CutNetwork = "cut"
networkIdToText (ChainNetwork cid) = "chain/" <> chainIdToText cid
{-# INLINE networkIdToText #-}

networkIdFromText :: MonadThrow m => T.Text -> m NetworkId
networkIdFromText "cut" = return CutNetwork
networkIdFromText t = case T.break (== '/') t of
    (a, b)
        | a == "chain" -> ChainNetwork <$> chainIdFromText (T.drop 1 b)
        | T.null b -> throwM . TextFormatException $ "missing '/' in network id: \"" <> t <> "\"."
        | otherwise -> throwM $ TextFormatException $ "unrecognized network id: \"" <> t <> "\"."

unsafeNetworkIdFromText :: T.Text -> NetworkId
unsafeNetworkIdFromText = fromJust . networkIdFromText
{-# INLINE unsafeNetworkIdFromText #-}

instance ToJSON NetworkId where
    toJSON = toJSON . networkIdToText
    {-# INLINE toJSON #-}

instance FromJSON NetworkId where
    parseJSON = parseJsonFromText "NetworkId"
    {-# INLINE parseJSON #-}

instance HasTextRepresentation NetworkId where
    toText = networkIdToText
    {-# INLINE toText #-}
    fromText = networkIdFromText
    {-# INLINE fromText #-}

pNetworkId :: OptionParser NetworkId
pNetworkId = textOption
    % long "network-id"

instance Arbitrary NetworkId where
    arbitrary = frequency
        [ (1, pure CutNetwork)
        , (5, ChainNetwork <$> arbitrary)
        ]

-- -------------------------------------------------------------------------- --
-- Type Level Network Id

data NetworkIdT
    = ChainNetworkT ChainIdT
    | CutNetworkT


-- | SomeNetworkIdT encapsulates a known NetworkIdT, such that it's type is
-- hidden from the compiler.
--
data SomeNetworkIdT where
    SomeChainNetworkT
        :: forall (a :: ChainIdT) (n :: NetworkIdT)
        . (KnownChainIdSymbol a, n ~ 'ChainNetworkT a)
        => Proxy n
        -> SomeNetworkIdT
    SomeCutNetworkT
        :: Proxy 'CutNetworkT
        -> SomeNetworkIdT

class KnownNetworkId (n :: NetworkIdT) where
    networkIdVal :: Proxy n -> SomeNetworkIdT

instance KnownChainIdSymbol c => KnownNetworkId ('ChainNetworkT c) where
    networkIdVal _ = SomeChainNetworkT (Proxy @('ChainNetworkT c))

instance KnownNetworkId 'CutNetworkT where
    networkIdVal _ = SomeCutNetworkT (Proxy @'CutNetworkT)

someNetworkIdVal :: NetworkId -> SomeNetworkIdT
someNetworkIdVal (ChainNetwork c) = case someChainIdVal c of
    SomeChainIdT (Proxy :: Proxy cid) -> SomeChainNetworkT (Proxy @('ChainNetworkT cid))
someNetworkIdVal CutNetwork = SomeCutNetworkT (Proxy @'CutNetworkT)

-- -------------------------------------------------------------------------- --
-- Singletons

data instance Sing (n :: NetworkIdT) where
    SChainNetwork :: SingI c => Sing c -> Sing ('ChainNetworkT c)
    SCutNetwork :: Sing 'CutNetworkT

type SNetwork (n :: NetworkIdT) = Sing n

instance SingI c => SingI ('ChainNetworkT c :: NetworkIdT) where
    sing = SChainNetwork sing

instance SingI 'CutNetworkT where
    sing = SCutNetwork

instance SingKind NetworkIdT where
    type Demote NetworkIdT = NetworkId

    fromSing (SChainNetwork c) = ChainNetwork (fromSing c)
    fromSing SCutNetwork = CutNetwork

    toSing (ChainNetwork (FromSing c)) = withSingI c $ SomeSing (SChainNetwork c)
    toSing CutNetwork = SomeSing SCutNetwork

