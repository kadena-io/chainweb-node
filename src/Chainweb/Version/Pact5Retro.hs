{-# language LambdaCase #-}
{-# language ImportQualifiedPost #-}
{-# language MultiWayIf #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language TypeApplications #-}
{-# language ViewPatterns #-}

module Chainweb.Version.Pact5Retro (pact5Retro) where

import Chainweb.Pact4.Transaction qualified as Pact4
import Chainweb.Pact5.Transaction qualified as Pact5
import Chainweb.Version
import Control.Lens
import Data.Aeson qualified as Aeson
import Data.ByteString.Short qualified as SBS
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Pact.Core.Command.Types qualified as Pact5
import Pact.Core.Pretty qualified as Pact5
import Pact.JSON.Encode qualified as J
import Data.Bits (complement)

pact5Retro :: ChainwebVersion -> ChainwebVersion
pact5Retro v = v
    & versionName .~ pact5VersionName
    & versionCode .~ pact5VersionCode
    & versionForks .~ pact5Forks
    & versionUpgrades .~ pact5Upgrades
    where

        pact5VersionCode = case _versionCode v of
            ChainwebVersionCode n -> ChainwebVersionCode $ complement n

        pact5VersionName = case _versionName v of
            ChainwebVersionName n -> ChainwebVersionName $ "pact5-retro-" <> n

        -- take everything after Chainweb217Pact and throw it out.
        -- then insert the Pact5 at the same height as Chainweb217Pact
        pact5Forks = flip HM.mapWithKey (_versionForks v) $ \fork height ->
            if  | fork < Chainweb217Pact -> height
                | fork == Pact5Fork -> case HM.lookup Chainweb217Pact (_versionForks v) of
                    Nothing -> error "pact5Retro: Chainweb 2.17 fork missing."
                    -- When it's available, use the same height as Chainweb217. Pact5
                    -- is only guaranteed to replay from Chainweb 2.17 onward.
                    Just h -> h
                | otherwise -> AllChains ForkNever

        -- Migrate all upgrades to Pact5
        pact5Upgrades = _versionUpgrades v
            <&> HM.map turnIntoPact5Upgrade

pactTxFrom4To5 :: Pact4.Transaction -> Pact5.Transaction
pactTxFrom4To5 tx =
    let
        e = do
            let json = J.encode (fmap (Text.decodeUtf8 . SBS.fromShort . Pact4.payloadBytes) tx)
            cmdWithPayload <- Aeson.eitherDecode @(Pact5.Command Text) json
            over _Left Pact5.renderCompactString $ Pact5.parseCommand cmdWithPayload
    in
    case e of
        Left err -> error err
        Right cmds -> cmds

turnIntoPact5Upgrade :: ForSomePactVersion PactUpgrade -> ForSomePactVersion PactUpgrade
turnIntoPact5Upgrade =
    forAnyPactVersion (\case
        Pact4Upgrade{..} -> ForPact5 $ Pact5Upgrade
            { _pact5UpgradeTransactions = List.map pactTxFrom4To5 _pact4UpgradeTransactions
            }
        Pact5Upgrade{..} -> ForPact5 $ Pact5Upgrade{..}
    )