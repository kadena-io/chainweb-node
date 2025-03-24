{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Chainweb.Test.Orphans.Pact
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Orphand Arbitrary Instances for Pact Types
--
module Chainweb.Test.Orphans.Pact
-- ( arbitraryJsonValue
( arbitraryCommandResultWithEvents
, arbitraryMaybe
) where

-- import qualified Data.Aeson as A
-- import qualified Data.Vector as V

import Pact.Core.Capabilities
import Pact.Core.Command.Types
import Pact.Core.Hash
import Pact.Core.PactValue

import Test.QuickCheck
import Pact.Core.Errors
-- import Pact.Core.Gas
import Pact.Core.Persistence
import Pact.Core.Names
import qualified Pact.Core.ChainData as Pact
import qualified Pact.Core.Guards as Pact

-- -------------------------------------------------------------------------- --
--

-- arbitraryJsonValue :: Gen A.Value
-- arbitraryJsonValue = go (3 :: Int)
--   where
--     go 0 = oneof
--         [ pure A.Null
--         , A.String <$> arbitrary
--         , A.Number <$> arbitrary
--         ]
--     go i = oneof
--         [ A.object <$> listOf ((,) <$> arbitrary <*> go (i - 1))
--         , A.Array . V.fromList <$> listOf (go (i - 1))
--         , pure A.Null
--         , A.String <$> arbitrary
--         , A.Number <$> arbitrary
--         ]

-- | Generates only successful results without continuations.
--
arbitraryCommandResultWithEvents :: Gen (PactEvent PactValue) -> Gen (CommandResult Hash PactErrorI)
arbitraryCommandResultWithEvents genEvent = CommandResult
    <$> undefined -- (RequestKey <$> arbitrary) -- _crReqKey
    <*> (fmap TxId <$> arbitrary) -- _crTxId
    <*> undefined -- arbitrary -- _crResult -- TODO: PP
    <*> undefined -- (Gas <$> arbitrary) -- _crGas
    <*> undefined -- arbitrary -- _crLogs
    <*> pure Nothing -- _crContinuation
    <*> undefined -- arbitraryMaybe (resize 5 arbitraryJsonValue) -- _crMetaData
    <*> (resize 10 (listOf genEvent)) -- _crEvents

instance Arbitrary ModuleName where
    -- TODO: PP
    arbitrary = undefined

instance Arbitrary ModuleHash where
    -- TODO: PP
    arbitrary = undefined

instance Arbitrary RequestKey where
    -- TODO: PP
    arbitrary = undefined

instance Arbitrary Pact.ChainId where
    -- TODO: PP
    arbitrary = undefined

instance Arbitrary QualifiedName where
    -- TODO: PP
    arbitrary = undefined

instance Arbitrary PactValue where
    -- TODO: PP
    arbitrary = undefined

instance (Arbitrary n, Arbitrary v) => Arbitrary (Pact.Guard n v) where
    -- TODO: PP
    arbitrary = undefined

arbitraryMaybe :: Gen a -> Gen (Maybe a)
arbitraryMaybe gen = arbitrary >>= mapM (const @_ @() gen)
