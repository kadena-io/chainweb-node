{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: PlonkBn254.Utils.EmbedVMKeys
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module PlonkBn254.Utils.EmbedVMKeys
( embedVMKeys
) where

import Control.Monad

import Data.ByteString qualified as B
import Data.Functor
import Data.List qualified as L
import Data.Vector qualified as V

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import System.Directory
import System.FilePath

-- internal modules

import PlonkBn254.Verify

-- -------------------------------------------------------------------------- --
-- Embed VM Keys

-- | Running this slice produces @[(FilePath, VMKey)]@.
--
-- It does not recurse into subdirectories and ignores any files that do
-- not have the suffix @.bin@.
--
-- The file path is the (relative) file name within the given directory.
--
embedVMKeys :: String -> FilePath -> Code Q [(FilePath, PlonkVK)]
embedVMKeys suffix fp = embedIO $ readVMKeyDir suffix fp

readVMKeyDir :: String -> FilePath -> IO [(FilePath, PlonkVK)]
readVMKeyDir suffix fp = do
    paths <- listFiles suffix fp
    forM paths $ \p -> do
        vk <- PlonkVK <$> B.readFile (fp </> p)
        return (stripSuffix p, vk)
  where
    stripSuffix x = take (length x - length suffix - 1) x

-- | The returned paths are relative to the given directory
--
listFiles :: String -> FilePath -> IO [FilePath]
listFiles suffix r = listDirectory r
    >>= filterM (doesFileExist . (r </>))
    >>= filterM (fmap readable . getPermissions . (r </>))
    <&> filter (L.isSuffixOf ("." <> suffix))

-- -------------------------------------------------------------------------- --
-- File embedding

embedIO :: Lift a => IO a -> Code Q a
embedIO action = runIO action `bindCode` liftTyped

-- -------------------------------------------------------------------------- --
-- Orphan Lift instances
--
-- Requires template-haskell >=2.16

instance (Lift a) => Lift (V.Vector a) where
    lift v = [| V.fromListN n' v' |]
      where
        n' = V.length v
        v' = V.toList v
    liftTyped = Code . unsafeTExpCoerce . lift
