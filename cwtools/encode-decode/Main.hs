{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Chainweb.Utils
import Control.Monad
import Data.ByteString (ByteString)
import Data.Text (Text)
import Options.Applicative
import Data.ByteString.Char8 qualified as B
import Data.Text qualified as T
import Data.Text.IO qualified as T

main :: IO ()
main = execParser opts >>= \case
    Encode variant -> B.getContents >>= encoderWork variant
    Decode variant -> T.getContents >>= decoderWork variant
  where
    opts :: ParserInfo Command
    opts = info (pCommand <**>  helper)
        (fullDesc <> progDesc "Encode/decode input via Base64 (and its variants)")

encoderWork :: Base64Variant -> ByteString -> IO ()
encoderWork variant bytes =
    mapM_ (T.putStrLn . encodingFunction variant) (B.lines bytes)
  where
    encodingFunction v = case v of
      Base64 -> encodeB64Text
      Base64Url -> encodeB64UrlText
      Base64UrlNoPadding -> encodeB64UrlNoPaddingText

decoderWork ::Base64Variant -> Text -> IO ()
decoderWork variant text =
    mapM_ (decodingFunction variant >=> B.putStrLn) (T.lines text)
  where
    decodingFunction v = case v of
      Base64 -> decodeB64Text
      Base64Url -> decodeB64UrlText
      Base64UrlNoPadding -> decodeB64UrlNoPaddingText

data Command
  = Encode Base64Variant
  | Decode Base64Variant

data Base64Variant
  = Base64
  | Base64Url
  | Base64UrlNoPadding

pCommand :: Parser Command
pCommand =
  hsubparser
    (toCommand "encodeB64" (pure $ Encode Base64) <>
     toCommand "encodeB64Url" (pure $ Encode Base64Url) <>
     toCommand "encodeB64UrlNoPadding" (pure $ Encode Base64UrlNoPadding) <>
     toCommand "decodeB64" (pure $ Decode Base64) <>
     toCommand "decodeB64Url" (pure $ Decode Base64Url) <>
     toCommand "decodeB64UrlNoPadding" (pure $ Decode Base64UrlNoPadding))
  where
    toCommand string pcmd = command string (info pcmd (progDesc string))
