{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- |

module Chainweb.VerifierPlugin.Plonk (plugin) where

import Chainweb.VerifierPlugin
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Class
import qualified Data.Text.Foreign as T
import Foreign.C.String
import GHC.IO
import Pact.Types.Exp
import Pact.Types.PactValue
import Pact.Types.Term.Internal

import qualified PlonkVerify

plonkVerifierPath :: CString
plonkVerifierPath = unsafePerformIO $ newCString "assets/plonk/"
{-# NOINLINE plonkVerifierPath #-}

plugin :: VerifierPlugin
plugin = VerifierPlugin $ \_ proofObj _caps gasRef -> do
  chargeGas gasRef 100
  (vk, pub, proof) <- case proofObj of
    PObject (ObjectMap om) -> do
      let getField k = case om ^? at (FieldKey k) . _Just . _PLiteral of
            Just (LString str) -> pure str
            _ -> throwError $ VerifierError $ k <> " is missing."

      vk    <- getField "vkey"
      pub   <- getField "pub"
      proof <- getField "proof"

      pure (vk, pub, proof)

    _ -> throwError $ VerifierError "Expected object with keys: vkey, pub, proof."

  pres <- lift $ unsafeIOToST $
            T.withCString vk $ \cvk ->
              T.withCString pub $ \cpub ->
                T.withCString proof $ \cproof ->
                  PlonkVerify.verify_plonk_bn254 plonkVerifierPath cproof cvk cpub

  -- https://github.com/argumentcomputer/lurk-hs/blob/main/src/lib.rs#L58-L63
  unless (pres == 1) $ throwError $ VerifierError "Verification failed."
