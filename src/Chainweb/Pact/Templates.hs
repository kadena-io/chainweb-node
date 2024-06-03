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
module Chainweb.Pact.Templates
( mkFundTxTerm
, mkBuyGasTerm
, mkRedeemGasTerm
, mkCoinbaseTerm

, mkFundTxCoreTerm
, mkBuyGasCoreTerm
, mkRedeemGasCoreTerm
, mkCoinbaseCoreTerm

, mkCoinbaseCmd
) where


import Control.Lens
import Data.Default (def)
import Data.Text (Text, pack)

import Text.Trifecta.Delta (Delta(..))

-- internal modules

import qualified Pact.JSON.Encode as J
import Pact.JSON.Legacy.Value
import Pact.Parse
import Pact.Types.Command
import Pact.Types.RPC
import Pact.Types.Runtime

import Chainweb.Miner.Pact
import Chainweb.Pact.Types
import Chainweb.Pact.Service.Types

import qualified Pact.Core.Literal as Core
import qualified Pact.Core.Names as Core
import qualified Pact.Core.Info as PCore
import qualified Pact.Core.Syntax.ParseTree as CoreLisp


inf :: Info
inf = Info $ Just (Code "",Parsed (Columns 0 0) 0)
{-# NOINLINE inf #-}

app :: Name -> [Term Name] -> Term Name
app f as = TApp (App (TVar f inf) as inf) inf
{-# INLINE app #-}

qn :: ModuleName -> Text -> Name
qn mn d = QName $ QualifiedName mn d inf
{-# INLINE qn #-}

bn :: Text -> Name
bn n = Name $ BareName n inf
{-# INLINE bn #-}

strLit :: Text -> Term Name
strLit s = TLiteral (LString s) inf
{-# INLINE strLit #-}

strArgSetter :: Int -> ASetter' (Term Name) Text
strArgSetter idx = tApp . appArgs . ix idx . tLiteral . _LString
{-# INLINE strArgSetter #-}

fundTxTemplate :: (Term Name, ASetter' (Term Name) Text, ASetter' (Term Name) Text)
fundTxTemplate =
  ( app (qn "coin" "fund-tx")
      [ strLit "sender"
      , strLit "mid"
      , app (bn "read-keyset") [strLit "miner-keyset"]
      , app (bn "read-decimal") [strLit "total"]
      ]
  , strArgSetter 0
  , strArgSetter 1
  )
{-# NOINLINE fundTxTemplate #-}

buyGasTemplate :: (Term Name, ASetter' (Term Name) Text)
buyGasTemplate =
  ( app (qn "coin" "buy-gas")
      [ strLit "sender"
      , app (bn "read-decimal") [strLit "total"]
      ]
  , strArgSetter 0
  )

fundTxTemplateCore :: Text -> Text -> CoreLisp.Expr PCore.SpanInfo
fundTxTemplateCore sender mid =
  let senderTerm = coreStrLit sender
      midTerm = coreStrLit mid
      varApp = coreQn "fund-tx" "coin"
      rks = coreApp (coreBn "read-keyset") [coreStrLit "miner-keyset"]
      rds = coreApp (coreBn "read-decimal") [coreStrLit "total"]
  in coreApp varApp [senderTerm, midTerm, rks, rds]

buyGasTemplateCore :: Text -> CoreLisp.Expr PCore.SpanInfo
buyGasTemplateCore sender =
  let senderTerm = coreStrLit sender
      varApp = coreQn "buy-gas" "coin"
      rds = coreApp (coreBn "read-decimal") [coreStrLit "total"]
  in coreApp varApp [senderTerm, rds]

redeemGasTemplate :: (Term Name, ASetter' (Term Name) Text, ASetter' (Term Name) Text)
redeemGasTemplate =
  ( app (qn "coin" "redeem-gas")
      [ strLit "mid"
      , app (bn "read-keyset") [strLit "miner-keyset"]
      , strLit "sender"
      , app (bn "read-decimal") [strLit "total"]
      ]
  , strArgSetter 2
  , strArgSetter 0
  )

redeemGasTemplateCore :: Text -> Text -> CoreLisp.Expr PCore.SpanInfo
redeemGasTemplateCore mid sender =
  let midTerm = coreStrLit mid
      senderTerm = coreStrLit sender
      varApp = coreQn "redeem-gas" "coin"
      rks = coreApp (coreBn "read-keyset") [coreStrLit "miner-keyset"]
      rds = coreApp (coreBn "read-decimal") [coreStrLit "total"]
  in coreApp varApp [midTerm, rks, senderTerm, rds]

coreApp :: CoreLisp.Expr PCore.SpanInfo -> [CoreLisp.Expr PCore.SpanInfo] -> CoreLisp.Expr PCore.SpanInfo
coreApp arg args = CoreLisp.App arg args def

coreStrLit :: Text -> CoreLisp.Expr PCore.SpanInfo
coreStrLit txt = CoreLisp.Constant (Core.LString txt) def

coreQn :: Text -> Text -> CoreLisp.Expr PCore.SpanInfo
coreQn name modname = CoreLisp.Var (Core.QN (Core.QualifiedName name (Core.ModuleName modname Nothing))) def

coreBn :: Text -> CoreLisp.Expr PCore.SpanInfo
coreBn name = CoreLisp.Var (Core.BN (Core.BareName name)) def

dummyParsedCode :: ParsedCode
dummyParsedCode = ParsedCode "1" [ELiteral $ LiteralExp (LInteger 1) def]
{-# NOINLINE dummyParsedCode #-}


mkFundTxTerm
  :: MinerId   -- ^ Id of the miner to fund
  -> MinerKeys -- ^ Miner keyset
  -> Text      -- ^ Address of the sender from the command
  -> GasSupply -- ^ The gas limit total * price
  -> (Term Name,ExecMsg ParsedCode)
mkFundTxTerm (MinerId mid) (MinerKeys ks) sender total = (populatedTerm, execMsg)
  where (term, senderS, minerS) = fundTxTemplate
        populatedTerm = set senderS sender $ set minerS mid term
        execMsg = ExecMsg dummyParsedCode (toLegacyJsonViaEncode buyGasData)
        buyGasData = J.object
          [ "miner-keyset" J..= ks
          , "total" J..= total
          ]
{-# INLINABLE mkFundTxTerm #-}

mkBuyGasTerm
  :: Text      -- ^ Address of the sender from the command
  -> GasSupply -- ^ The gas limit total * price
  -> (Term Name,ExecMsg ParsedCode)
mkBuyGasTerm sender total = (populatedTerm, execMsg)
  where (term, senderS) = buyGasTemplate
        populatedTerm = set senderS sender term
        execMsg = ExecMsg dummyParsedCode (toLegacyJsonViaEncode buyGasData)
        buyGasData = J.object
          [ "total" J..= total ]
{-# INLINABLE mkBuyGasTerm #-}

mkRedeemGasTerm
  :: MinerId   -- ^ Id of the miner to fund
  -> MinerKeys -- ^ Miner keyset
  -> Text      -- ^ Address of the sender from the command
  -> GasSupply -- ^ The gas limit total * price
  -> GasSupply -- ^ The gas used * price
  -> (Term Name,ExecMsg ParsedCode)
mkRedeemGasTerm (MinerId mid) (MinerKeys ks) sender total fee = (populatedTerm, execMsg)
  where (term, senderS, minerS) = redeemGasTemplate
        populatedTerm = set senderS sender $ set minerS mid term
        execMsg = ExecMsg dummyParsedCode (toLegacyJsonViaEncode redeemGasData)
        redeemGasData = J.object
          [ "total" J..= total
          , "fee" J..= J.toJsonViaEncode fee
          , "miner-keyset" J..= ks
          ]
{-# INLINABLE mkRedeemGasTerm #-}

mkFundTxCoreTerm
  :: MinerId   -- ^ Id of the miner to fund
  -> Text      -- ^ Address of the sender from the command
  -> CoreLisp.Expr PCore.SpanInfo
mkFundTxCoreTerm (MinerId mid) sender = fundTxTemplateCore sender mid
{-# INLINABLE mkFundTxCoreTerm #-}

mkBuyGasCoreTerm
  :: Text      -- ^ Address of the sender from the command
  -> CoreLisp.Expr PCore.SpanInfo
mkBuyGasCoreTerm sender = buyGasTemplateCore sender
{-# INLINABLE mkBuyGasCoreTerm #-}

mkRedeemGasCoreTerm
  :: MinerId   -- ^ Id of the miner to fund
  -> Text      -- ^ Address of the sender from the command
  -> CoreLisp.Expr PCore.SpanInfo
mkRedeemGasCoreTerm (MinerId mid) sender = redeemGasTemplateCore mid sender
{-# INLINABLE mkRedeemGasCoreTerm #-}

coinbaseTemplate :: (Term Name,ASetter' (Term Name) Text)
coinbaseTemplate =
  ( app (qn "coin" "coinbase")
      [ strLit "mid"
      , app (bn "read-keyset") [strLit "miner-keyset"]
      , app (bn "read-decimal") [strLit "reward"]
      ]
  , strArgSetter 0
  )
{-# NOINLINE coinbaseTemplate #-}

coinbaseTemplateCore :: Text -> CoreLisp.Expr PCore.SpanInfo
coinbaseTemplateCore mid =
  let midTerm = coreStrLit mid
      varApp = coreQn "coinbase" "coin"
      rks = coreApp (coreBn "read-keyset") [coreStrLit "miner-keyset"]
      rds = coreApp (coreBn "read-decimal") [coreStrLit "reward"]
  in coreApp varApp [midTerm, rks, rds]

mkCoinbaseTerm :: MinerId -> MinerKeys -> ParsedDecimal -> (Term Name,ExecMsg ParsedCode)
mkCoinbaseTerm (MinerId mid) (MinerKeys ks) reward = (populatedTerm, execMsg)
  where
    (term, minerS) = coinbaseTemplate
    populatedTerm = set minerS mid term
    execMsg = ExecMsg dummyParsedCode (toLegacyJsonViaEncode coinbaseData)
    coinbaseData = J.object
      [ "miner-keyset" J..= ks
      , "reward" J..= reward
      ]
{-# INLINABLE mkCoinbaseTerm #-}

mkCoinbaseCoreTerm
  :: MinerId   -- ^ Id of the miner to fund
  -> CoreLisp.Expr PCore.SpanInfo
mkCoinbaseCoreTerm (MinerId mid) = coinbaseTemplateCore mid
{-# INLINABLE mkCoinbaseCoreTerm #-}

-- | "Old method" to build a coinbase 'ExecMsg' for back-compat.
--
mkCoinbaseCmd :: MinerId -> MinerKeys -> ParsedDecimal -> IO (ExecMsg ParsedCode)
mkCoinbaseCmd (MinerId mid) (MinerKeys ks) reward =
    buildExecParsedCode $ mconcat
      [ "(coin.coinbase"
      , " \"" <> mid <> "\""
      , " (read-keyset \"miner-keyset\")"
      , " (read-decimal \"reward\"))"
      ]
  where

    coinbaseData = J.object
      [ "miner-keyset" J..= ks
      , "reward" J..= reward
      ]

    -- | Build the 'ExecMsg' for some pact code fed to the function.
    --
    buildExecParsedCode :: Text -> IO (ExecMsg ParsedCode)
    buildExecParsedCode code = case parsePact code of
        Right !t -> pure $! ExecMsg t (toLegacyJsonViaEncode coinbaseData)
        -- if we can't construct coin contract calls, this should
        -- fail fast
        Left err -> internalError $ "buildExecParsedCode: parse failed: " <> pack err

{-# INLINABLE mkCoinbaseCmd #-}
