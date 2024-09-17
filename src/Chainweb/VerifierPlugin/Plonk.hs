{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

-- |

module Chainweb.VerifierPlugin.Plonk
  (plugin) where

import Chainweb.Utils
import Chainweb.VerifierPlugin
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Class
import Data.Bifunctor
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Short as BS
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified EmbedVKeys
import GHC.IO
import Pact.Types.Capability
import Pact.Types.Exp
import Pact.Types.PactValue
import qualified PlonkVerify

-- | Verifying Keys
--
-- Providing a (compile-time) mapping of the  @VKey@.
verifyingKeys :: M.Map T.Text PlonkVerify.VKey
verifyingKeys = M.fromList $ map (over _1 T.pack) $$(EmbedVKeys.embedVKeys "bin" "verifier-assets")


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
     plonkProof <- PlonkVerify.Proof . BS.toShort <$> decodeB64UrlNoPaddingText proofMsg

     SigCapability _ capArgs <- case Set.toList caps of
       [cap] -> pure cap
       _ -> throwError $ VerifierError "Expected single capability."

     (vk, progId, pub) <- case capArgs of
       [PLiteral (LString vk), PLiteral (LString progId), PLiteral (LString pub)] -> do
         vk' <- maybe (throwError $ VerifierError "Unknown verifier key.") pure $ M.lookup vk verifyingKeys

         progId' <- PlonkVerify.ProgramId . BS.toShort <$> decodeB64UrlNoPaddingText progId

         let spub = fromMaybe pub $ T.stripPrefix "0x" pub
         pub' <- liftEither . first (const (VerifierError "Base16 decoding failure.")) $
           PlonkVerify.PublicParameter . BS.toShort <$> B16.decode (T.encodeUtf8 spub)

         pure (vk', progId', pub')
       _ -> throwError $ VerifierError "Incorrect number of capability arguments. Expected vk, progId, pub."

     withness <- lift $ unsafeIOToST $ PlonkVerify.verifyPlonkBn254 vk plonkProof progId pub
     unless withness $ throwError $ VerifierError "Verification failed."

   _ -> throwError $ VerifierError "Expected proof data as a string."
