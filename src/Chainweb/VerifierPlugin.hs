{-# language DerivingStrategies #-}
{-# language EmptyCase #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language TypeApplications #-}

module Chainweb.VerifierPlugin(VerifierPlugin(..), VerifierError(..), runVerifierPlugins) where

import Control.DeepSeq
import Control.Exception.Safe(Exception, throwIO)
import Control.Lens
import Control.Monad

import Data.Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Lens
import Data.Foldable
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Merge
import Data.Maybe
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Text(Text)

import Chainweb.Transaction
import Chainweb.Utils

import Pact.JSON.Legacy.Value
import Pact.Types.Capability
import Pact.Types.Command
import Pact.Types.PactValue
import Pact.Types.RPC
import Pact.Types.Verifier

data VerifierError = VerifierError Text
    deriving stock Show
instance Exception VerifierError

newtype VerifierPlugin
    = VerifierPlugin
    { runVerifierPlugin :: ChainwebTransaction -> [PactValue] -> Set MsgCapability -> IO ()
    }
    deriving newtype NFData

runVerifierPlugins :: Map Text VerifierPlugin -> ChainwebTransaction -> IO ()
runVerifierPlugins allVerifiers tx = do
    let
        txData = _getLegacyValue $ case _pPayload $ payloadObj $ _cmdPayload tx of
            Exec m -> _pmData m
            Continuation m -> _cmData m
    txDataValues <- fmap Map.fromList $ forM usedTxDataKeys $ \k -> do
        v <- case fmap fromJSON (txData ^? key k) of
            Just (Success v) -> return v
            _ -> throwIO $ VerifierError $ "verifier argument not valid: " <> sshow k
        return (k, v)
    _ <- Merge.mergeA
        (Merge.traverseMissing $ \k _ -> throwIO $ VerifierError ("verifier does not exist: " <> k))
        Merge.dropMissing
        (Merge.zipWithAMatched $ \_vn argsAndCaps verifierPlugin ->
            for_ argsAndCaps $ \(args, caps) ->
                let argValues = [txDataValues ^?! at k . _Just | k <- args]
                in runVerifierPlugin verifierPlugin tx argValues caps
            )
        usedVerifiers
        allVerifiers
    return ()
    where
    usedTxDataKeys :: [Key]
    usedTxDataKeys =
        [ arg | (_, verifier) <- Map.toList usedVerifiers, (args, _) <- verifier, arg <- args ]
    usedVerifiers :: Map Text [([Key], Set MsgCapability)]
    usedVerifiers =
        Map.fromListWith (++) $
        fmap
            (\Verifier {_veArgs = VerifierArgs name args, _veCapList = caps} ->
                (name, [(Key.fromText <$> args, Set.fromList caps)])) $
        fromMaybe [] $ _pVerifiers (payloadObj $ _cmdPayload tx)
