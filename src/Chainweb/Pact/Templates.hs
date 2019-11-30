{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

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
  ( mkBuyGasTerm
  , mkCoinbaseTerm
  ) where


import Control.Lens
import Data.Aeson hiding ((.=))
import qualified Data.Aeson as A
import Data.Default (def)
import Data.Text (Text)

import Pact.Parse
import Pact.Types.Command
import Pact.Types.RPC
import Pact.Types.Runtime

import Chainweb.Miner.Pact
import Chainweb.Pact.Types

app :: Name -> [Term Name] -> Term Name
app f as = TApp (App (TVar f def) as def) def

qn :: ModuleName -> Text -> Name
qn mn d = QName $ QualifiedName mn d def

bn :: Text -> Name
bn n = Name $ BareName n def

strLit :: Text -> Term Name
strLit s = TLiteral (LString s) def

strArgSetter :: Int -> ASetter' (Term Name) Text
strArgSetter idx = tApp . appArgs . ix idx . tLiteral . _LString

buyGasTemplate :: (Term Name,ASetter' (Term Name) Text,ASetter' (Term Name) Text)
buyGasTemplate =
  (app (qn "coin" "fund-tx")
   [strLit "sender"
   ,strLit "mid"
   ,app (bn "read-keyset") [strLit "miner-keyset"]
   ,app (bn "read-decimal") [strLit "total"]
   ]
  ,strArgSetter 0
  ,strArgSetter 1
  )


dummyParsedCode :: ParsedCode
dummyParsedCode = ParsedCode "1" [ELiteral $ LiteralExp (LInteger 1) def]

mkBuyGasTerm
  :: MinerId   -- ^ Id of the miner to fund
  -> MinerKeys -- ^ Miner keyset
  -> Text      -- ^ Address of the sender from the command
  -> GasSupply -- ^ The gas limit total * price
  -> (Term Name,ExecMsg ParsedCode)
mkBuyGasTerm (MinerId mid) (MinerKeys ks) sender total = (populatedTerm,execMsg)
  where (term,senderS,minerS) = buyGasTemplate
        populatedTerm = set senderS sender $ set minerS mid term
        execMsg = ExecMsg dummyParsedCode buyGasData
        buyGasData = object
          [ "miner-keyset" A..= ks
          , "total" A..= total
          ]
{-# INLINABLE mkBuyGasTerm #-}


coinbaseTemplate :: (Term Name,ASetter' (Term Name) Text)
coinbaseTemplate =
  (app (qn "coin" "coinbase")
   [strLit "mid"
   ,app (bn "read-keyset") [strLit "miner-keyset"]
   ,app (bn "read-decimal") [strLit "reward"]]
  ,strArgSetter 0)

mkCoinbaseTerm :: MinerId -> MinerKeys -> ParsedDecimal -> (Term Name,ExecMsg ParsedCode)
mkCoinbaseTerm (MinerId mid) (MinerKeys ks) reward = (populatedTerm,execMsg)
  where
    (term,minerS) = coinbaseTemplate
    populatedTerm = set minerS mid term
    execMsg = ExecMsg dummyParsedCode coinbaseData
    coinbaseData = object
      [ "miner-keyset" A..= ks
      , "reward" A..= reward
      ]
{-# INLINABLE mkCoinbaseTerm #-}
