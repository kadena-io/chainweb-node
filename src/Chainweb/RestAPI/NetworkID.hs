{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Chainweb.RestAPI.NetworkID
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Identifiers for Chainweb P2P networks.
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
, Sing(SChainNetwork, SMempoolNetwork, SCutNetwork)
, type SNetwork
, pattern FromSingNetworkId
) where

import Configuration.Utils

import Control.DeepSeq
import Control.Monad.Catch

import Data.Hashable
import Data.Proxy
import qualified Data.Text as T

import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

-- Internal imports

import Chainweb.ChainId
import Chainweb.Utils hiding (check)

import Data.Singletons

-- -------------------------------------------------------------------------- --
-- Network ID

data NetworkId
    = ChainNetwork ChainId
    | MempoolNetwork ChainId
    | CutNetwork
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData)

-- | Textual representation of NetworkId.
--
-- This is expected to match the respective routes generated for
-- 'NetworkEndpointApi'.
--
networkIdToText :: NetworkId -> T.Text
networkIdToText CutNetwork = "cut"
networkIdToText (ChainNetwork cid) = "chain/" <> chainIdToText cid
networkIdToText (MempoolNetwork cid) = "chain/" <> chainIdToText cid <> "/mempool"
{-# INLINE networkIdToText #-}

networkIdFromText :: MonadThrow m => T.Text -> m NetworkId
networkIdFromText t = case T.split (== '/') t of
    ["cut"] -> return CutNetwork
    ["chain", a, "mempool"] -> MempoolNetwork <$> chainIdFromText a
    ["chain", a] -> ChainNetwork <$> chainIdFromText a
    _ -> throwM $ TextFormatException $ "unrecognized network id: \"" <> t <> "\"."

unsafeNetworkIdFromText :: HasCallStack => T.Text -> NetworkId
unsafeNetworkIdFromText = fromJuste . networkIdFromText
{-# INLINE unsafeNetworkIdFromText #-}

instance ToJSON NetworkId where
    toJSON = toJSON . networkIdToText
    toEncoding = toEncoding . networkIdToText
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

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

-- -------------------------------------------------------------------------- --
-- Type Level Network Id

data NetworkIdT
    = ChainNetworkT ChainIdT
    | MempoolNetworkT ChainIdT
    | CutNetworkT


-- | SomeNetworkIdT encapsulates a known NetworkIdT, such that its type is
-- hidden from the compiler.
--
data SomeNetworkIdT where
    SomeChainNetworkT
        :: forall (a :: ChainIdT) (n :: NetworkIdT)
        . (KnownChainIdSymbol a, n ~ 'ChainNetworkT a)
        => Proxy n
        -> SomeNetworkIdT
    SomeMempoolNetworkT
        :: forall (a :: ChainIdT) (n :: NetworkIdT)
        . (KnownChainIdSymbol a, n ~ 'MempoolNetworkT a)
        => Proxy n
        -> SomeNetworkIdT
    SomeCutNetworkT
        :: Proxy 'CutNetworkT
        -> SomeNetworkIdT

class KnownNetworkId (n :: NetworkIdT) where
    networkIdVal :: Proxy n -> SomeNetworkIdT

instance KnownChainIdSymbol c => KnownNetworkId ('ChainNetworkT c) where
    networkIdVal _ = SomeChainNetworkT (Proxy @('ChainNetworkT c))

instance KnownChainIdSymbol c => KnownNetworkId ('MempoolNetworkT c) where
    networkIdVal _ = SomeMempoolNetworkT (Proxy @('MempoolNetworkT c))

instance KnownNetworkId 'CutNetworkT where
    networkIdVal _ = SomeCutNetworkT (Proxy @'CutNetworkT)

someNetworkIdVal :: NetworkId -> SomeNetworkIdT
someNetworkIdVal (ChainNetwork c) = case someChainIdVal c of
    SomeChainIdT (Proxy :: Proxy cid) -> SomeChainNetworkT (Proxy @('ChainNetworkT cid))
someNetworkIdVal (MempoolNetwork c) = case someChainIdVal c of
    SomeChainIdT (Proxy :: Proxy cid) -> SomeMempoolNetworkT (Proxy @('MempoolNetworkT cid))
someNetworkIdVal CutNetwork = SomeCutNetworkT (Proxy @'CutNetworkT)

-- -------------------------------------------------------------------------- --
-- Singletons

data instance Sing (n :: NetworkIdT) where
    SChainNetwork :: SingI c => Sing c -> Sing ('ChainNetworkT c)
    SMempoolNetwork :: SingI c => Sing c -> Sing ('MempoolNetworkT c)
    SCutNetwork :: Sing 'CutNetworkT

type SNetwork (n :: NetworkIdT) = Sing n

instance SingI c => SingI ('ChainNetworkT c :: NetworkIdT) where
    sing = SChainNetwork sing

instance SingI c => SingI ('MempoolNetworkT c :: NetworkIdT) where
    sing = SMempoolNetwork sing

instance SingI 'CutNetworkT where
    sing = SCutNetwork

instance SingKind NetworkIdT where
    type Demote NetworkIdT = NetworkId

    fromSing (SChainNetwork c) = ChainNetwork (fromSing c)
    fromSing (SMempoolNetwork c) = MempoolNetwork (fromSing c)
    fromSing SCutNetwork = CutNetwork

    toSing (ChainNetwork (FromSingChainId c)) = withSingI c $ SomeSing (SChainNetwork c)
    toSing (MempoolNetwork (FromSingChainId c)) = withSingI c $ SomeSing (SMempoolNetwork c)
    toSing CutNetwork = SomeSing SCutNetwork

    {-# INLINE fromSing #-}
    {-# INLINE toSing #-}

pattern FromSingNetworkId :: Sing (n :: NetworkIdT) -> NetworkId
pattern FromSingNetworkId sng <- ((\cid -> withSomeSing cid SomeSing) -> SomeSing sng)
  where FromSingNetworkId sng = fromSing sng
{-# COMPLETE FromSingNetworkId #-}
