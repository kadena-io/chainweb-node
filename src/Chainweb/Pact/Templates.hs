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
( mkBuyGasTerm
, mkCoinbaseTerm
, mkCoinbaseCmd
) where


import Control.Lens
import Data.Aeson hiding ((.=))
import qualified Data.Aeson as A
import Data.Default (def)
import Data.Text (Text, pack)

import Text.Trifecta.Delta (Delta(..))

-- internal modules

import Pact.Parse
import Pact.Types.Command
import Pact.Types.RPC
import Pact.Types.Runtime

import Chainweb.Miner.Pact
import Chainweb.Pact.Types
import Chainweb.Pact.Service.Types


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

buyGasTemplate :: (Term Name, ASetter' (Term Name) Text, ASetter' (Term Name) Text)
buyGasTemplate =
  ( app (qn "coin" "fund-tx")
      [ strLit "sender"
      , strLit "mid"
      , app (bn "read-keyset") [strLit "miner-keyset"]
      , app (bn "read-decimal") [strLit "total"]
      ]
  , strArgSetter 0
  , strArgSetter 1
  )
{-# NOINLINE buyGasTemplate #-}


dummyParsedCode :: ParsedCode
dummyParsedCode = ParsedCode "1" [ELiteral $ LiteralExp (LInteger 1) def]
{-# NOINLINE dummyParsedCode #-}


mkBuyGasTerm
  :: MinerId   -- ^ Id of the miner to fund
  -> MinerKeys -- ^ Miner keyset
  -> Text      -- ^ Address of the sender from the command
  -> GasSupply -- ^ The gas limit total * price
  -> (Term Name,ExecMsg ParsedCode)
mkBuyGasTerm (MinerId mid) (MinerKeys ks) sender total = (populatedTerm, execMsg)
  where (term, senderS, minerS) = buyGasTemplate
        populatedTerm = set senderS sender $ set minerS mid term
        execMsg = ExecMsg dummyParsedCode buyGasData
        buyGasData = object
          [ "miner-keyset" A..= ks
          , "total" A..= total
          ]
{-# INLINABLE mkBuyGasTerm #-}


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


mkCoinbaseTerm :: MinerId -> MinerKeys -> ParsedDecimal -> (Term Name,ExecMsg ParsedCode)
mkCoinbaseTerm (MinerId mid) (MinerKeys ks) reward = (populatedTerm, execMsg)
  where
    (term, minerS) = coinbaseTemplate
    populatedTerm = set minerS mid term
    execMsg = ExecMsg dummyParsedCode coinbaseData
    coinbaseData = object
      [ "miner-keyset" A..= ks
      , "reward" A..= reward
      ]
{-# INLINABLE mkCoinbaseTerm #-}

-- | Build the 'ExecMsg' for some pact code fed to the function. The 'value'
-- parameter is for any possible environmental data that needs to go into
-- the 'ExecMsg'.
--
buildExecParsedCode :: Maybe Value -> Text -> IO (ExecMsg ParsedCode)
buildExecParsedCode value code = maybe (go Null) go value
  where
    go v = case parsePact code of
      Right !t -> pure $! ExecMsg t v
      -- if we can't construct coin contract calls, this should
      -- fail fast
      Left err -> internalError $ "buildExecParsedCode: parse failed: " <> pack err

-- | "Old method" to build a coinbase 'ExecMsg' for back-compat.
--
mkCoinbaseCmd :: MinerId -> MinerKeys -> ParsedDecimal -> IO (ExecMsg ParsedCode)
mkCoinbaseCmd (MinerId mid) (MinerKeys ks) reward =
    buildExecParsedCode coinbaseData $ mconcat
      [ "(coin.coinbase"
      , " \"" <> mid <> "\""
      , " (read-keyset \"miner-keyset\")"
      , " (read-decimal \"reward\"))"
      ]
  where
    coinbaseData = Just $ object
      [ "miner-keyset" A..= ks
      , "reward" A..= reward
      ]
{-# INLINABLE mkCoinbaseCmd #-}
