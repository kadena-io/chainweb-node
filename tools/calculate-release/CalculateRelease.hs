{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module CalculateRelease(main) where

import Chainweb.BlockHeight
import Chainweb.Version (ChainwebVersion(..))
import Chainweb.Version.Mainnet (mainnet)
import Control.Lens
import Control.Monad
import Data.Aeson.Lens
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Time.Format.ISO8601 (formatParseM, iso8601Format)
import Network.Wreq
import System.Exit

main :: IO ()
main = do
    let v = fromMaybe (error "mainnet serviceDate not found") $ _versionServiceDate mainnet
    -- fetch current service date
    serviceDateDay <- utctDay <$> formatParseM (iso8601Format @UTCTime) v
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
