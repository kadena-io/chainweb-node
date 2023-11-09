{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeApplications #-}

module Chainweb.VerifierPlugin(VerifierPlugin(..), VerifierError(..), runVerifierPlugins) where

import Chainweb.Transaction

import Control.DeepSeq
import Control.Exception.Safe(Exception, throwIO)
import Control.Monad.Except

import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Merge
import Data.Maybe
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Text(Text)

import Pact.Types.Capability
import Pact.Types.Command
import Pact.Types.Term

data VerifierError = VerifierError
    deriving stock Show
instance Exception VerifierError

newtype VerifierPlugin
    = VerifierPlugin
    { runVerifierPlugin :: ChainwebTransaction -> Set MsgCapability -> IO ()
    }
    deriving newtype NFData

runVerifierPlugins :: Map Text VerifierPlugin -> ChainwebTransaction -> IO Bool
runVerifierPlugins allVerifiers tx = do
    isLeft $ try $
    Merge.mergeA
        (Merge.traverseMissing $ \_ _ -> throwIO VerifierError)
        Merge.dropMissing
        (Merge.zipWithAMatched $ \vn caps verifierPlugin -> undefined $ runVerifierPlugin verifierPlugin tx caps)
        mentionedVerifiers
        allVerifiers
    undefined
    where
    mentionedVerifiers =
        Map.fromListWith (Set.union @MsgCapability) $
        fmap (\Verifier {_veName = VerifierName vn, _veCapList = caps} -> (vn, Set.fromList caps)) $
        fromMaybe [] $ _pVerifiers (_payloadObj $ _cmdPayload tx)
