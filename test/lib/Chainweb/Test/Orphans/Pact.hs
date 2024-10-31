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
( arbitraryJsonValue
, arbitraryCommandResultWithEvents
, arbitraryMaybe
) where

import qualified Data.Aeson as A
import qualified Data.Vector as V

import Pact.Types.Command
import Pact.Types.Runtime

import Test.QuickCheck

-- -------------------------------------------------------------------------- --
--

arbitraryJsonValue :: Gen A.Value
arbitraryJsonValue = go (3 :: Int)
  where
    go 0 = oneof
        [ pure A.Null
        , A.String <$> arbitrary
        , A.Number <$> arbitrary
        ]
    go i = oneof
        [ A.object <$> listOf ((,) <$> arbitrary <*> go (i - 1))
        , A.Array . V.fromList <$> listOf (go (i - 1))
        , pure A.Null
        , A.String <$> arbitrary
        , A.Number <$> arbitrary
        ]

-- | Generates only successful results without continuations.
--
arbitraryCommandResultWithEvents :: Gen PactEvent -> Gen (CommandResult Hash)
arbitraryCommandResultWithEvents genEvent = CommandResult
    <$> arbitrary -- _crReqKey
    <*> arbitrary -- _crTxId
    <*> arbitrary -- _crResult
    <*> arbitrary -- _crGas
    <*> arbitrary -- _crLogs
    <*> pure Nothing -- _crContinuation
    <*> arbitraryMaybe (resize 5 arbitraryJsonValue) -- _crMetaData
    <*> (resize 10 (listOf genEvent)) -- _crEvents

arbitraryMaybe :: Gen a -> Gen (Maybe a)
arbitraryMaybe gen = arbitrary >>= mapM (const @_ @() gen)

