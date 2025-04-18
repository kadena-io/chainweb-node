{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- |
-- Module      :  Chainweb.Pact.Templates
-- Copyright   :  Copyright © 2010 Kadena LLC.
-- License     :  (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy
-- Stability   :  experimental
--
-- Prebuilt Term templates for automated operations (coinbase, gas buy)
--
module Chainweb.Pact.Templates
( mkFundTxTerm
, mkBuyGasTerm
, mkRedeemGasTerm
, mkCoinbaseTerm
) where

import Data.Decimal
import Data.Text (Text)

-- internal modules

import Chainweb.Miner.Pact

import Pact.Core.Literal qualified as Pact
import Pact.Core.Names qualified as Pact
import Pact.Core.Syntax.ParseTree qualified as Pact
import Pact.Core.PactValue qualified as Pact
import Data.Map as Map
import Chainweb.Pact.Types

fundTxTemplate :: Text -> Text -> Pact.Expr ()
fundTxTemplate sender mid =
  let senderTerm = strLit sender
      midTerm = strLit mid
      varApp = qn "fund-tx" "coin"
      -- TODO PP: fork this so that guards are supported, by using read-msg instead.
      -- instead of forking here, in theory we could detect the type of guard
      -- and use different code if it's a keyset.
      rks = app (bn "read-keyset") [strLit "miner-keyset"]
      rds = app (bn "read-decimal") [strLit "total"]
  in app varApp [senderTerm, midTerm, rks, rds]

buyGasTemplate :: Text -> Pact.Expr ()
buyGasTemplate sender =
  let senderTerm = strLit sender
      varApp = qn "buy-gas" "coin"
      rds = app (bn "read-decimal") [strLit "total"]
  in app varApp [senderTerm, rds]

redeemGasTemplate :: Text -> Text -> Pact.Expr ()
redeemGasTemplate mid sender =
  let midTerm = strLit mid
      senderTerm = strLit sender
      varApp = qn "redeem-gas" "coin"
      -- TODO PP: fork this so that guards are supported, by using read-msg instead
      rks = app (bn "read-keyset") [strLit "miner-keyset"]
      rds = app (bn "read-decimal") [strLit "total"]
  in app varApp [midTerm, rks, senderTerm, rds]

app :: Pact.Expr () -> [Pact.Expr ()] -> Pact.Expr ()
app arg args = Pact.App arg args ()

strLit :: Text -> Pact.Expr ()
strLit txt = Pact.Constant (Pact.LString txt) ()

qn :: Text -> Text -> Pact.Expr ()
qn name modname = Pact.Var (Pact.QN (Pact.QualifiedName name (Pact.ModuleName modname Nothing))) ()

bn :: Text -> Pact.Expr ()
bn name = Pact.Var (Pact.BN (Pact.BareName name)) ()

mkFundTxTerm
  :: MinerId   -- ^ Id of the miner to fund
  -> MinerGuard
  -> Text      -- ^ Address of the sender from the command
  -> GasSupply
  -> (Pact.Expr (), Map.Map Pact.Field Pact.PactValue)
mkFundTxTerm (MinerId mid) (MinerGuard ks) sender total =
  let
    term = fundTxTemplate sender mid
    buyGasData = Map.fromList
      [ ("miner-keyset", Pact.PGuard ks)
      , ("total", Pact.PDecimal $ _pact5GasSupply total)
      ]
  in (term, buyGasData)

mkBuyGasTerm
  :: Text      -- ^ Address of the sender from the command
  -> GasSupply
  -> (Pact.Expr (), Map.Map Pact.Field Pact.PactValue)
mkBuyGasTerm sender total = (buyGasTemplate sender, buyGasData)
  where
    buyGasData = Map.fromList
      [ ("total", Pact.PDecimal $ _pact5GasSupply total) ]
{-# INLINABLE mkBuyGasTerm #-}

mkRedeemGasTerm
  :: MinerId    -- ^ Id of the miner to fund
  -> MinerGuard -- ^ Miner guard
  -> Text       -- ^ Address of the sender from the command
  -> GasSupply  -- ^ The gas limit total * price
  -> GasSupply  -- ^ The gas used * price
  -> (Pact.Expr (), Pact.PactValue)
mkRedeemGasTerm (MinerId mid) (MinerGuard g) sender total fee =
  (redeemGasTemplate mid sender, redeemGasData)
  where
    redeemGasData = Pact.PObject $ Map.fromList
      [ ("total", Pact.PDecimal $ _pact5GasSupply total)
      , ("fee", Pact.PDecimal $ _pact5GasSupply fee)
      , ("miner-keyset", Pact.PGuard g)
      ]
{-# INLINABLE mkRedeemGasTerm #-}

coinbaseTemplate :: Text -> Pact.Expr ()
coinbaseTemplate mid =
  let midTerm = strLit mid
      varApp = qn "coinbase" "coin"
      -- TODO PP: fork this so that guards are supported, by using read-msg instead
      rks = app (bn "read-keyset") [strLit "miner-keyset"]
      rds = app (bn "read-decimal") [strLit "reward"]
  in app varApp [midTerm, rks, rds]

mkCoinbaseTerm :: MinerId -> MinerGuard -> Decimal -> (Pact.Expr (), Pact.PactValue)
mkCoinbaseTerm (MinerId mid) (MinerGuard g) reward = (coinbaseTemplate mid, coinbaseData)
  where
    coinbaseData = Pact.PObject $ Map.fromList
      [ ("miner-keyset", Pact.PGuard g)
      , ("reward", Pact.PDecimal reward)
      ]
{-# INLINABLE mkCoinbaseTerm #-}
