{-# language
    BangPatterns
  , DataKinds
  , ImportQualifiedPost
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeApplications
#-}

module Script (main) where

import Chainweb.BlockHash (BlockHash)
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.IO qualified as TL
import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.BlockHeight (BlockHeight)
import Chainweb.Utils (sshow)
import Colonnade
import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson qualified as A
import Data.Aeson.Lens
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Req
import Siphon

baseUrl :: Url 'Https
baseUrl = https "api.chainweb.com" /: "chainweb" /: "0.0" /: "mainnet01"

-- returns (height, hash) of chain 0
getLatestChain0 :: Req (Word, Text)
getLatestChain0 = do
  cut <- responseBody <$> req @_ @_ @_ @(JsonResponse Value) GET (baseUrl /: "cut") NoReqBody jsonResponse mempty
  let chain0Obj = cut ^?! key "hashes" . _Value ^?! key "0" . _Value
  let height = chain0Obj ^?! key "height" . _Integral
  let hash = chain0Obj ^?! key "hash" . _String
  pure (height, hash)

-- returns (headers, nextPageIndex)
blockHeaderBranches :: Text -> Req ([BlockHeader], Text)
blockHeaderBranches hash = do
  let payload = object
        [ "lower" A..= ([] :: [Text])
        , "upper" A..= [hash]
        ]
  let itemLimit :: Word
      itemLimit = 100
  resp <- responseBody <$> req @_ @_ @_ @(JsonResponse Value) POST (baseUrl /: "chain" /: "0" /: "header" /: "branch") (ReqBodyJson payload) jsonResponse ("limit" =: itemLimit)
  let items :: [BlockHeader]
      items = resp ^?! key "items" . _Array ^.. traverse . _JSON
  let next :: Text
      next = resp ^?! key "next" . _String
  pure (items, maybe (error "") id (T.stripPrefix "inclusive:" next))

getLastNBlockHeaders :: Int -> Text -> Req [BlockHeader]
getLastNBlockHeaders n upperBoundHash = do
  let go :: Text -> Int -> [BlockHeader] -> Req [BlockHeader]
      go nextIx !len acc
        | len >= n = pure acc
        | otherwise = do
            (headers, nextIndex) <- blockHeaderBranches nextIx
            if len + length headers > n
            then pure (acc ++ take (n - len) headers)
            else go nextIndex (len + length headers) (acc ++ headers)

  (headersInitial, nextIndex) <- blockHeaderBranches upperBoundHash
  go nextIndex (length headersInitial) headersInitial

col :: Colonnade Headed (BlockHeight, BlockHash) Text
col = mconcat
  [ Colonnade.headed "blockheight" (sshow . fst)
  , Colonnade.headed "blockhash" (showBlockHash . snd)
  ]
  where
    showBlockHash :: BlockHash -> Text
    -- shows up as """foo"""
    showBlockHash = T.drop 3 . T.dropEnd 3 . sshow

main :: IO ()
main = runReq defaultHttpConfig $ do
  _latestChain0@(_height, hash) <- getLatestChain0

  lastNHeaders <- getLastNBlockHeaders 2000 hash

  let dat = map (\h -> (_blockHeight h, _blockHash h)) lastNHeaders
  liftIO $ TL.writeFile "last_2000_blocks" $ TB.toLazyText $ Siphon.encodeCsv col dat
