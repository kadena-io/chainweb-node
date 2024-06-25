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


import Control.Lens
import Data.Decimal
import Data.Text (Text, pack)

import Text.Trifecta.Delta (Delta(..))

-- internal modules

import qualified Pact.JSON.Encode as J
import qualified Pact.JSON.Legacy.Value as J

import Chainweb.Miner.Pact
import Chainweb.Pact.Types
import Chainweb.Pact.Service.Types

import Pact.Core.Evaluate
import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.Info
import Pact.Core.Syntax.ParseTree
import Debug.Trace
import Pact.JSON.Legacy.Value
import Pact.Core.PactValue
import qualified Data.Map as Map
import qualified Pact.Types.KeySet
import Data.Aeson (fromJSON, decodeStrictText)
import Chainweb.Utils (decodeStrictOrThrow, decodeOrThrow)
import System.IO.Unsafe (unsafePerformIO)
import Pact.Core.StableEncoding (StableEncoding(_stableEncoding))
import Control.Exception.Safe (throw, SomeException, impureThrow)
import qualified Pact.Types.KeySet as Pact4

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
  -> Pact5GasSupply
  -> (Expr (), PactValue)
mkFundTxTerm (MinerId mid) (MinerKeys ks) sender total =
  let
    term = fundTxTemplate sender mid
    buyGasData = PObject $ Map.fromList
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
  -> Pact5GasSupply
  -> (Expr (), PactValue)
mkBuyGasTerm sender total = (buyGasTemplate sender, buyGasData)
  where
    buyGasData = PObject $ Map.fromList
      [ ("total", PDecimal $ _pact5GasSupply total) ]
{-# INLINABLE mkBuyGasTerm #-}

mkRedeemGasTerm
  :: MinerId   -- ^ Id of the miner to fund
  -> MinerKeys -- ^ Miner keyset
  -> Text      -- ^ Address of the sender from the command
  -> Pact5GasSupply -- ^ The gas limit total * price
  -> Pact5GasSupply -- ^ The gas used * price
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

mkCoinbaseTerm :: MinerId -> MinerKeys -> Pact5GasSupply -> (Expr (), PactValue)
mkCoinbaseTerm (MinerId mid) (MinerKeys ks) reward = (coinbaseTemplate mid, coinbaseData)
  where
    coinbaseData = PObject $ Map.fromList
      [ ("miner-keyset", convertKeySet ks)
      , ("reward", PDecimal $ _pact5GasSupply reward)
      ]
{-# INLINABLE mkCoinbaseTerm #-}
