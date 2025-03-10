{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- |

module Chainweb.VerifierPlugin.Plonk
  (plugin) where

import Debug.Trace
import Control.Monad.Except
import Control.Monad.Trans.Class
import Pact.Core.Errors (VerifierError(..))
import Data.Bifunctor
import Data.ByteString.Base16 qualified as B16
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

import GHC.IO

import Pact.Types.Capability
import Pact.Types.Exp
import Pact.Types.PactValue

import PlonkBn254.Verify qualified as P
import PlonkBn254.VMKeys qualified as P

-- internal modules

import Chainweb.Utils
import Chainweb.VerifierPlugin

-- The plugin verifies Plonk zero-knowledge proofs within the context of Pact transactions.
--
-- [@proof@] A Base-64 URL encoded message without padding.
--
-- [@caps@] A set containing precisely one 'SigCapability' with three arguments:
--    1. A UTF-8 encoded string identifying the verifying key.
--    2. A Base-16 URL encoded message representing the program ID.
--    3. A Base-16 encoded message representing the public parameters.
plugin :: VerifierPlugin
plugin = VerifierPlugin $ \_ proof caps gasRef -> do
  chargeGas gasRef 100
  case proof of
   PLiteral (LString proofMsg) -> do
     plonkProof <- P.Proof  <$> decodeB64UrlNoPaddingText proofMsg

     SigCapability _ capArgs <- case Set.toList caps of
       [cap] -> pure cap
       _ -> throwError $ VerifierError "Expected single capability."

     (vk, progId, pub) <- case capArgs of
       [PLiteral (LString vk), PLiteral (LString progId), PLiteral (LString pub)] -> do
         vk' <- maybe (throwError $ VerifierError "Unknown verifier key.") pure $ M.lookup vk P.verifyingKeys

         let progId' = P.SP1VKeyHash $ T.encodeUtf8 progId

         let spub = fromMaybe pub $ T.stripPrefix "0x" pub
         pub' <- liftEither . first (const (VerifierError "Base16 decoding failure.")) $
           P.PublicInputs  <$> B16.decode (T.encodeUtf8 spub)

         pure (vk', progId', pub')
       _ -> throwError $ VerifierError "Incorrect number of capability arguments. Expected vk, progId, pub."

     withness <- lift $ unsafeIOToST $ P.verifyProof plonkProof pub progId vk
     case withness of
        P.VerificationSuccessful -> pure ()
        _ -> throwError $ VerifierError "Verification failed."

   _ -> throwError $ VerifierError "Expected proof data as a string."
