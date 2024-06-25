{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Chainweb.Pact.Templates
-- Copyright   :  Copyright © 2010 Kadena LLC.
-- License     :  (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy
-- Stability   :  experimental
--
-- Prebuilt Term templates for automated operations (coinbase, gas buy)
--
module Chainweb.Pact.Templates.Pact5
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

import qualified Pact.Types.RPC as Pact4
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
  -> (Expr (), Pact4.ExecMsg RawCode)
mkFundTxTerm (MinerId mid) (MinerKeys ks) sender total =
  let
    term = fundTxTemplate sender mid
    buyGasData = J.object
      [ "miner-keyset" J..= ks
      , "total" J..= total
      ]
    execMsg = Pact4.ExecMsg (RawCode "") (J.toLegacyJsonViaEncode buyGasData)
  in (term, execMsg)
{-# INLINABLE mkFundTxTerm #-}

mkBuyGasTerm
  :: Text      -- ^ Address of the sender from the command
  -> GasSupply
  -> (Expr (), Pact4.ExecMsg RawCode)
mkBuyGasTerm sender total = (buyGasTemplate sender, execMsg)
  where
    execMsg = Pact4.ExecMsg (RawCode "") (J.toLegacyJsonViaEncode buyGasData)
    buyGasData = J.object
      [ "total" J..= total ]
{-# INLINABLE mkBuyGasTerm #-}

mkRedeemGasTerm
  :: MinerId   -- ^ Id of the miner to fund
  -> MinerKeys -- ^ Miner keyset
  -> Text      -- ^ Address of the sender from the command
  -> GasSupply -- ^ The gas limit total * price
  -> GasSupply -- ^ The gas used * price
  -> (Expr (), Pact4.ExecMsg RawCode)
mkRedeemGasTerm (MinerId mid) (MinerKeys ks) sender total fee = (redeemGasTemplate mid sender, execMsg)
  where
    execMsg = Pact4.ExecMsg (RawCode "") (J.toLegacyJsonViaEncode redeemGasData)
    redeemGasData = J.object
      [ "total" J..= total
      , "fee" J..= J.toJsonViaEncode fee
      , "miner-keyset" J..= ks
      ]
{-# INLINABLE mkRedeemGasTerm #-}

coinbaseTemplate :: Text -> Expr ()
coinbaseTemplate mid =
  let midTerm = strLit mid
      varApp = qn "coinbase" "coin"
      rks = app (bn "read-keyset") [strLit "miner-keyset"]
      rds = app (bn "read-decimal") [strLit "reward"]
  in app varApp [midTerm, rks, rds]

mkCoinbaseTerm :: MinerId -> MinerKeys -> GasSupply -> (Expr (), Pact4.ExecMsg RawCode)
mkCoinbaseTerm (MinerId mid) (MinerKeys ks) reward = (coinbaseTemplate mid, execMsg)
  where
    execMsg = Pact4.ExecMsg (RawCode "") (J.toLegacyJsonViaEncode coinbaseData)
    coinbaseData = J.object
      [ "miner-keyset" J..= ks
      , "reward" J..= reward
      ]
{-# INLINABLE mkCoinbaseTerm #-}
