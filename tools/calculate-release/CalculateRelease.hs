{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module CalculateRelease(main) where

import Control.Lens
import Control.Monad
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Time

import Network.Wreq
import System.Exit
import Text.Regex.TDFA

import Chainweb.BlockHeight

main :: IO ()
main = do
    -- fetch current service date
    nodeMainModule <- get "https://raw.githubusercontent.com/kadena-io/chainweb-node/master/node/ChainwebNode.hs" <&> (^. responseBody . to LBSC.unpack)
    let serviceDateRegex :: String = "^\\s*serviceDate = Just \"([0-9]+-[0-9]+-[0-9]+)(T[0-9:]+Z)?\""
    let serviceDateMatch :: (String, String, String, [String]) =
            (nodeMainModule =~ serviceDateRegex)
    serviceDateDay <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d" $
        (serviceDateMatch ^?! _4 . _head)
    putStrLn $ "Current service date detected: " <> show serviceDateDay
    let serviceDateTime = UTCTime { utctDay = serviceDateDay, utctDayTime = 0 }

    -- check current time difference from service date
    now <- getCurrentTime
    when (serviceDateTime `diffUTCTime` now > 30 * 24 * 60 * 60) $
        putStrLn $ unlines
            [ "WARNING: estimating the fork height from >30 days in advance."
            , "This may result in an inaccurate fork height. Remember that an"
            , "early fork height can fork the network, and a late fork height can"
            , "result in team members not being around during the fork." ]

    !mainnetHeightNow <- heightOfChain0 "https://us-e1.chainweb.com/chainweb/0.0/mainnet01/cut"
    mainnetTimeNow <- getCurrentTime
    !testnetHeightNow <- heightOfChain0 "https://us1.testnet.chainweb.com/chainweb/0.0/testnet04/cut"
    testnetTimeNow <- getCurrentTime
    let mainnetForkTime = addUTCTime (24 * 60 * 60) serviceDateTime
    let testnetForkTime = addUTCTime (12 * 60 * 60) serviceDateTime
    mainnetForkHeight <- estimateHeight mainnetForkTime (mainnetTimeNow, mainnetHeightNow)
    testnetForkHeight <- estimateHeight testnetForkTime (testnetTimeNow, testnetHeightNow)

    putStrLn $ "Mainnet fork height (at " <> show mainnetForkTime <> "): " <> show mainnetForkHeight
    putStrLn $ "Testnet fork height (at " <> show testnetForkTime <> "): " <> show testnetForkHeight
    let nextServiceDateDay = addDays (12 * 7) serviceDateDay
    putStrLn $ "Next service date (+12 weeks): " <> show nextServiceDateDay
    where
    heightOfChain0 :: String -> IO BlockHeight
    heightOfChain0 cutUrl =
        get cutUrl <&> (^?! (responseBody . key "hashes" . key "0" . key "height" . _Number . to truncate))

estimateHeight :: UTCTime -> (UTCTime, BlockHeight) -> IO BlockHeight
estimateHeight forkTime (nowTime, nowHeight) =
    let
        estimatedForkHeight = nowHeight + round (realToFrac (forkTime `diffUTCTime` nowTime) / 30 :: Double)
        checkForkTime = addUTCTime (fromIntegral $ (* 30) $ estimatedForkHeight - nowHeight) nowTime
    in
        if abs (checkForkTime `diffUTCTime` forkTime) > 60
        then die "internal error: estimation is incorrect (roundtrip error)"
        else return estimatedForkHeight
