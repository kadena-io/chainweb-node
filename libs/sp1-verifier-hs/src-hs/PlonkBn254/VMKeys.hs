{-# LANGUAGE TemplateHaskell #-}

module PlonkBn254.VMKeys (verifyingKeys) where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import PlonkBn254.Utils.EmbedVMKeys
import PlonkBn254.Verify

verifyingKeys :: M.Map T.Text PlonkVK
verifyingKeys = M.fromList $ map (over _1 T.pack) $$(embedVMKeys "bin" "verifier-assets")
