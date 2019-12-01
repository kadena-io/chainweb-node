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
{-# LANGUAGE TemplateHaskell #-}

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
  , mkDirectBuyGasTerm
  , mkDirectRedeemGasTerm
  , mkDummyExec
  ) where


import Control.Lens
import Data.Aeson hiding ((.=))
import qualified Data.Aeson as A
import Data.Default (def)
import Data.Decimal
import Data.Text (Text)

import Pact.Parse
import Pact.Types.Command
import Pact.Types.RPC
import Pact.Types.Runtime

import Chainweb.Miner.Pact
import Chainweb.Pact.Types

app :: Name -> [Term Name] -> Term Name
app f as = TApp (App (TVar f def) as def) def
{-# INLINE app #-}

qn :: ModuleName -> Text -> Name
qn mn d = QName $ QualifiedName mn d def
{-# INLINE qn #-}

bn :: Text -> Name
bn n = Name $ BareName n def
{-# INLINE bn #-}

strLit :: Text -> Term Name
strLit s = TLiteral (LString s) def
{-# INLINE strLit #-}

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
{-# NOINLINE buyGasTemplate #-}


dummyParsedCode :: ParsedCode
dummyParsedCode = ParsedCode "1" [ELiteral $ LiteralExp (LInteger 1) def]
{-# NOINLINE dummyParsedCode #-}

mkDummyExec :: Value -> ExecMsg ParsedCode
mkDummyExec v = ExecMsg dummyParsedCode v


mkBuyGasTerm
  :: MinerId   -- ^ Id of the miner to fund
  -> MinerKeys -- ^ Miner keyset
  -> Text      -- ^ Address of the sender from the command
  -> GasSupply -- ^ The gas limit total * price
  -> (Term Name,ExecMsg ParsedCode)
mkBuyGasTerm (MinerId mid) (MinerKeys ks) sender total = (populatedTerm,execMsg)
  where (term,senderS,minerS) = buyGasTemplate
        populatedTerm = set senderS sender $ set minerS mid term
        execMsg = mkDummyExec buyGasData
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
{-# NOINLINE coinbaseTemplate #-}


mkCoinbaseTerm :: MinerId -> MinerKeys -> ParsedDecimal -> (Term Name,ExecMsg ParsedCode)
mkCoinbaseTerm (MinerId mid) (MinerKeys ks) reward = (populatedTerm,execMsg)
  where
    (term,minerS) = coinbaseTemplate
    populatedTerm = set minerS mid term
    execMsg = mkDummyExec coinbaseData
    coinbaseData = object
      [ "miner-keyset" A..= ks
      , "reward" A..= reward
      ]
{-# INLINABLE mkCoinbaseTerm #-}


directBuyGasTemplate :: (Term Name,ASetter' (Term Name) Text,ASetter' (Term Name) Decimal)
directBuyGasTemplate =
  (app (qn "coin" "buy-gas")
   [strLit "sender"
   ,TLiteral (LDecimal 0) def
   ]
  ,strArgSetter 0
  ,tApp . appArgs . ix 1 . tLiteral . _LDecimal
  )
{-# NOINLINE directBuyGasTemplate #-}



-- TODO backport to Pact
makePrisms ''Guard


directRedeemGasTemplate
  :: (Term Name
     ,ASetter' (Term Name) Text
     ,ASetter' (Term Name) KeySet
     ,ASetter' (Term Name) Text
     ,ASetter' (Term Name) Decimal
     )
directRedeemGasTemplate =
  (app (qn "coin" "redeem-gas")
   [strLit "mid"
   ,TGuard (GKeySet (KeySet mempty (bn ""))) def
   ,strLit "sender"
   ,TLiteral (LDecimal 0) def
   ]
  ,strArgSetter 0
  ,tApp . appArgs . ix 1 . tGuard . _GKeySet
  ,strArgSetter 2
  ,tApp . appArgs . ix 3 . tLiteral . _LDecimal
  )
{-# NOINLINE directRedeemGasTemplate #-}


mkDirectBuyGasTerm
  :: Text      -- ^ Address of the sender from the command
  -> GasSupply -- ^ The gas limit total * price
  -> Term Name
mkDirectBuyGasTerm sender (GasSupply (ParsedDecimal s)) = populatedTerm
  where (term,senderS,totalS) = directBuyGasTemplate
        populatedTerm = set senderS sender $ set totalS s term
{-# INLINABLE mkDirectBuyGasTerm #-}

mkDirectRedeemGasTerm
  :: MinerId   -- ^ Id of the miner to fund
  -> MinerKeys -- ^ Miner keyset
  -> Text      -- ^ Address of the sender from the command
  -> GasSupply -- ^ The gas limit total * price
  -> GasSupply -- ^ the fee
  -> (Term Name,Value)
mkDirectRedeemGasTerm (MinerId mid) (MinerKeys ks) sender
  (GasSupply (ParsedDecimal s)) fee = (populatedTerm,redeemData)
  where (term,minerS,minerKeySetS,senderS,totalS) = directRedeemGasTemplate
        populatedTerm =
          set minerS mid $
          set minerKeySetS ks $
          set senderS sender $
          set totalS s term
        redeemData = object [ "fee" A..= fee ]
{-# INLINABLE mkDirectRedeemGasTerm #-}
