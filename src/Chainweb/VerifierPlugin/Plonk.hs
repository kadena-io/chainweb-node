{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- |

module Chainweb.VerifierPlugin.Plonk
  (plugin) where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Class

import Data.Bifunctor
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Short qualified as BS
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

import GHC.IO

import Pact.Types.Capability
import Pact.Types.Exp
import Pact.Types.PactValue

import PlonkBn254.Utils.EmbedVMKeys qualified as P
import PlonkBn254.Verify qualified as P

-- internal modules

import Chainweb.Utils
import Chainweb.VerifierPlugin

-- | Verifying Keys
--
-- Providing a (compile-time) mapping of the  @VMKey@.
verifyingKeys :: M.Map T.Text P.VMKey
verifyingKeys = M.fromList $ map (over _1 T.pack) $$(P.embedVMKeys "bin" "verifier-assets")


-- The plugin verifies Plonk zero-knowledge proofs within the context of Pact transactions.
--
-- [@proof@] A Base-64 URL encoded message without padding.
--
-- [@caps@] A set containing precisely one 'SigCapability' with three arguments:
--    1. A UTF-8 encoded string identifying the verifying key.
--    2. A Base-64 URL encoded message (without padding) representing the program ID.
--    3. A Base-16 encoded message representing the public parameters.
plugin :: VerifierPlugin
plugin = VerifierPlugin $ \_ proof caps gasRef -> do
  chargeGas gasRef 100
  case proof of
   PLiteral (LString proofMsg) -> do
     plonkProof <- P.Proof . BS.toShort <$> decodeB64UrlNoPaddingText proofMsg

     SigCapability _ capArgs <- case Set.toList caps of
       [cap] -> pure cap
       _ -> throwError $ VerifierError "Expected single capability."

     (vk, progId, pub) <- case capArgs of
       [PLiteral (LString vk), PLiteral (LString progId), PLiteral (LString pub)] -> do
         vk' <- maybe (throwError $ VerifierError "Unknown verifier key.") pure $ M.lookup vk verifyingKeys

         progId' <- P.ProgramId . BS.toShort <$> decodeB64UrlNoPaddingText progId

         let spub = fromMaybe pub $ T.stripPrefix "0x" pub
         pub' <- liftEither . first (const (VerifierError "Base16 decoding failure.")) $
           P.PublicParameter . BS.toShort <$> B16.decode (T.encodeUtf8 spub)

         pure (vk', progId', pub')
       _ -> throwError $ VerifierError "Incorrect number of capability arguments. Expected vk, progId, pub."

     withness <- lift $ unsafeIOToST $ P.verify vk plonkProof progId pub
     unless withness $ throwError $ VerifierError "Verification failed."

   _ -> throwError $ VerifierError "Expected proof data as a string."
