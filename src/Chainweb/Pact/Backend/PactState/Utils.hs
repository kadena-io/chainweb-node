{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Chainweb.Pact.Backend.PactState.Utils
  ( inQuotes
  , fromTextSilly
  , doesPactDbExist
  , bsLength
  , buildW64
  , buildI64
  , dontIncludeZero
  , dontIncludeZeroBytes
  , buildPrettyBytes
  , showBytes
  , oneKB, oneMB, oneGB, oneTB
  )
  where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Numeric (showFFloat)
import Pact.JSON.Encode qualified as J
import Data.Word (Word64)
import Data.Int (Int64)
import Unsafe.Coerce (unsafeCoerce)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import GHC.Stack (HasCallStack)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Chainweb.Utils (HasTextRepresentation, fromText)
import Chainweb.Version (ChainId, chainIdToText)

inQuotes :: (IsString s, Monoid s) => s -> s
inQuotes x = "\"" <> x <> "\""

fromTextSilly :: (HasTextRepresentation a, HasCallStack) => Text -> a
fromTextSilly t = case fromText t of
  Just a -> a
  Nothing -> error "fromText failed"

doesPactDbExist :: ChainId -> FilePath -> IO Bool
doesPactDbExist cid dbDir = do
  let chainDbFileName = mconcat
        [ "pact-v1-chain-"
        , Text.unpack (chainIdToText cid)
        , ".sqlite"
        ]
  let file = dbDir </> chainDbFileName
  doesFileExist file

bsLength :: ByteString -> Word64
bsLength b = fromIntegral @_ @Word64 (BS.length b)

buildW64 :: Word64 -> J.Builder
buildW64 = unsafeCoerce BB.word64Dec -- pact-json should provide this, or export its constructors

buildI64 :: Int64 -> J.Builder
buildI64 = unsafeCoerce BB.int64Dec -- pact-json should provide this, or export its constructors

dontIncludeZero :: Word64 -> Maybe J.Builder
dontIncludeZero x = buildW64 <$> J.ifMaybe (> 0) x

dontIncludeZeroBytes :: Word64 -> Maybe J.Builder
dontIncludeZeroBytes x = buildPrettyBytes <$> J.ifMaybe (> 0) x

buildPrettyBytes :: Word64 -> J.Builder
buildPrettyBytes bytes = J.text (Text.pack (showBytes bytes))

showBytes :: Word64 -> String
showBytes bytes
  | bytes > oneTB = showFloat (w2d bytes / w2d oneTB) ++ " TB"
  | bytes > oneGB = showFloat (w2d bytes / w2d oneGB) ++ " GB"
  | bytes > oneMB = showFloat (w2d bytes / w2d oneMB) ++ " MB"
  | bytes > oneKB = showFloat (w2d bytes / w2d oneKB) ++ " KB"
  | otherwise     = show bytes ++ " B"
  where
    w2d :: Word64 -> Double
    w2d = fromIntegral

    showFloat f = showFFloat (Just 2) f ""

oneKB :: Word64
oneKB = 1024

oneMB :: Word64
oneMB = oneKB * oneKB

oneGB :: Word64
oneGB = oneKB * oneKB * oneKB

oneTB :: Word64
oneTB = oneKB * oneKB * oneKB * oneKB
