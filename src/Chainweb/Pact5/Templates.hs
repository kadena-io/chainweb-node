{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Chainweb.Pact5.Templates
-- Copyright   :  Copyright © 2010 Kadena LLC.
-- License     :  (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy
-- Stability   :  experimental
--
-- Prebuilt Term templates for automated operations (coinbase, gas buy)
--
module Chainweb.Pact5.Templates
( mkFundTxTerm
, mkBuyGasTerm
, mkRedeemGasTerm
, mkCoinbaseTerm
) where

import Data.Decimal
import Data.Text (Text)

-- internal modules

import qualified Pact.JSON.Encode as J

import Chainweb.Miner.Pact

import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.Syntax.ParseTree
import Pact.Core.PactValue
import qualified Data.Map as Map
import Chainweb.Utils (decodeOrThrow)
import Pact.Core.StableEncoding (StableEncoding(_stableEncoding))
import Control.Exception.Safe (impureThrow)
import qualified Pact.Types.KeySet as Pact4
import Chainweb.Pact5.Types

fundTxTemplate :: Text -> Text -> Expr ()
fundTxTemplate sender mid =
  let senderTerm = strLit sender
      midTerm = strLit mid
      varApp = qn "fund-tx" "coin"
      rks = app (bn "read-keyset") [strLit "miner-keyset"]
      rds = app (bn "read-decimal") [strLit "total"]
  in app varApp [senderTerm, midTerm, rks, rds]

buyGasTemplate :: Text -> Expr ()
buyGasTemplate sender =
  let senderTerm = strLit sender
      varApp = qn "buy-gas" "coin"
      rds = app (bn "read-decimal") [strLit "total"]
  in app varApp [senderTerm, rds]

redeemGasTemplate :: Text -> Text -> Expr ()
redeemGasTemplate mid sender =
  let midTerm = strLit mid
      senderTerm = strLit sender
      varApp = qn "redeem-gas" "coin"
      rks = app (bn "read-keyset") [strLit "miner-keyset"]
      rds = app (bn "read-decimal") [strLit "total"]
  in app varApp [midTerm, rks, senderTerm, rds]

app :: Expr () -> [Expr ()] -> Expr ()
app arg args = App arg args ()

strLit :: Text -> Expr ()
strLit txt = Constant (LString txt) ()

qn :: Text -> Text -> Expr ()
qn name modname = Var (QN (QualifiedName name (ModuleName modname Nothing))) ()

bn :: Text -> Expr ()
bn name = Var (BN (BareName name)) ()

mkFundTxTerm
  :: MinerId   -- ^ Id of the miner to fund
  -> MinerKeys
  -> Text      -- ^ Address of the sender from the command
  -> GasSupply
  -> (Expr (), Map.Map Field PactValue)
mkFundTxTerm (MinerId mid) (MinerKeys ks) sender total =
  let
    term = fundTxTemplate sender mid
    buyGasData = Map.fromList
      [ ("miner-keyset", convertKeySet ks)
      , ("total", PDecimal $ _pact5GasSupply total)
      ]
  in (term, buyGasData)

-- we configure the miner keyset as a Pact4 keyset
-- TODO: change this?
convertKeySet :: Pact4.KeySet -> PactValue
convertKeySet =
  either impureThrow _stableEncoding . decodeOrThrow . J.encode
{-# INLINABLE mkFundTxTerm #-}

mkBuyGasTerm
  :: Text      -- ^ Address of the sender from the command
  -> GasSupply
  -> (Expr (), Map.Map Field PactValue)
mkBuyGasTerm sender total = (buyGasTemplate sender, buyGasData)
  where
    buyGasData = Map.fromList
      [ ("total", PDecimal $ _pact5GasSupply total) ]
{-# INLINABLE mkBuyGasTerm #-}

mkRedeemGasTerm
  :: MinerId   -- ^ Id of the miner to fund
  -> MinerKeys -- ^ Miner keyset
  -> Text      -- ^ Address of the sender from the command
  -> GasSupply -- ^ The gas limit total * price
  -> GasSupply -- ^ The gas used * price
  -> (Expr (), PactValue)
mkRedeemGasTerm (MinerId mid) (MinerKeys ks) sender total fee =
  (redeemGasTemplate mid sender, redeemGasData)
  where
    redeemGasData = PObject $ Map.fromList
      [ ("total", PDecimal $ _pact5GasSupply total)
      , ("fee", PDecimal $ _pact5GasSupply fee)
      , ("miner-keyset", convertKeySet ks)
      ]
{-# INLINABLE mkRedeemGasTerm #-}

coinbaseTemplate :: Text -> Expr ()
coinbaseTemplate mid =
  let midTerm = strLit mid
      varApp = qn "coinbase" "coin"
      rks = app (bn "read-keyset") [strLit "miner-keyset"]
      rds = app (bn "read-decimal") [strLit "reward"]
  in app varApp [midTerm, rks, rds]

mkCoinbaseTerm :: MinerId -> MinerKeys -> Decimal -> (Expr (), PactValue)
mkCoinbaseTerm (MinerId mid) (MinerKeys ks) reward = (coinbaseTemplate mid, coinbaseData)
  where
    coinbaseData = PObject $ Map.fromList
      [ ("miner-keyset", convertKeySet ks)
      , ("reward", PDecimal reward)
      ]
{-# INLINABLE mkCoinbaseTerm #-}
