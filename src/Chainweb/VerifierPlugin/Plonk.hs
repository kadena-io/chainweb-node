{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

-- |

module Chainweb.VerifierPlugin.Plonk (plugin) where

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
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import GHC.IO
import GHC.ST
import Pact.Types.Exp
import Pact.Types.PactValue
import Pact.Types.Term.Internal

import qualified PlonkVerify
import qualified EmbedVKeys

verifyingKeys :: M.Map T.Text PlonkVerify.VKey
verifyingKeys = M.fromList $ map (over _1 T.pack) $$(EmbedVKeys.embedVKeys "bin" "verifier-assets")

plugin :: VerifierPlugin
plugin = VerifierPlugin $ \_ proofObj _caps gasRef -> do
  chargeGas gasRef 100
  case proofObj of
    PObject (ObjectMap om) -> do
      let getField k = case om ^? at (FieldKey k) . _Just . _PLiteral of
            Just (LString str) -> pure str
            _ -> throwError $ VerifierError $ k <> " is missing."

      vk    <- getField "vkey" >>= (\case
                 Just vk -> pure vk
                 Nothing -> throwError $ VerifierError "Unknown verifier key.") . (`M.lookup` verifyingKeys)

      proof  <- PlonkVerify.Proof  <$> (getField "proof" >>= toB16Short)
      progId <- PlonkVerify.ProgramId <$> (getField "progId" >>= toB16Short)

      res <- case om ^. at (FieldKey "pub") of
        Just (PLiteral (LString hsh)) -> do
          bytes <- toB16Short hsh
          pub <- liftEither . first (VerifierError . T.pack) $ PlonkVerify.mkPublicParameterHash bytes
          lift $ unsafeIOToST $ PlonkVerify.verifyPlonkBn254' vk proof progId pub

        Just (PList l) -> do
          pub <- traverse mkPublicParameter (V.toList l)
          lift $ unsafeIOToST $ PlonkVerify.verifyPlonkBn254 vk proof progId pub

        Just _ ->
          throwError $ VerifierError "Expected public parameters as string literal or list."
        _ ->
          throwError $ VerifierError "Expected field pub."


      unless res $ throwError $ VerifierError "Verification failed."

    _ -> throwError $ VerifierError "Expected object with keys: vkey, progId, pub, proof."
  where
    toB16Short str = do
      let sstr = fromMaybe str $ T.stripPrefix "0x" str
      case B16.decode (T.encodeUtf8 sstr) of
        Left _e -> throwError $ VerifierError "decoding hex string failed."
        Right bytes -> pure $ BS.toShort bytes

    mkPublicParameter :: PactValue -> ExceptT VerifierError (ST s) PlonkVerify.PublicParameter
    mkPublicParameter = \case
      PLiteral (LString str) -> PlonkVerify.PublicParameter <$> toB16Short str
      _ -> throwError $ VerifierError "Expected string literals as public parameters."
