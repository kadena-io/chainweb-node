{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Chainweb.Test.Pact.SPV.Hyperlane
( -- * test suite
  tests
) where

import Control.Monad.Trans.Except
import Data.Text (Text)
import Data.Default (def)
import Data.DoubleWord (Word256)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.QuickCheck.Instances ()

import Chainweb.Test.Utils (prop_iso)

-- internal pact modules

import Pact.Types.Exp
import Pact.Native.Internal
import Pact.Types.Term
import Pact.Types.Util (AsString(..))

-- internal chainweb modules

import Chainweb.Pact.SPV.Hyperlane
import Chainweb.Pact.SPV.Hyperlane.Binary
import Chainweb.Utils.Serialization (runGetS)

instance Arbitrary Word256 where
  arbitrary = fromInteger . getNonNegative <$> arbitrary

tests :: TestTree
tests = testGroup "hyperlane"
  [ testCase "empty object" hyperlaneEmptyObject

  , testCase "encodeTokenMessageERC20" hyperlaneEncodeTokenMessageERC20
  , testCase "decodeTokenMessageERC20" hyperlaneDecodeTokenMessageERC20

  , testCase "decimalToWord" hyperlaneDecimalToWord
  , testCase "decimalToWord2" hyperlaneDecimalToWord2
  , testCase "wordToDecimal" hyperlaneWordToDecimal
  , testCase "wordToDecimal2" hyperlaneWordToDecimal2
  , testProperty "wordDecimalRoundTrip" $ prop_iso @Word256 @_ decimalToWord wordToDecimal

  , testCase "encodeHyperlaneMessage" hyperlaneEncodeHyperlaneMessage

  , testCase "recoverValidatorAnnouncementSuccess" hyperlaneRecoverValidatorAnnouncementSuccess
  , testCase "recoverValidatorAnnouncementFailure" hyperlaneRecoverValidatorAnnouncementFailure

  , testCase "verifySuccess" hyperlaneVerifySuccess
  , testCase "verifySuccessMoreValidators" hyperlaneVerifySuccessMoreValidators

  , testCase "verifyFailure" hyperlaneVerifyFailure
  , testCase "verifyFailureWrongTypeValidator" hyperlaneVerifyFailureWrongTypeValidator
  , testCase "verifyFailureIncorrectValidator" hyperlaneVerifyFailureIncorrectValidator
  , testCase "verifyFailureNotEnoughRecoveredSignatures" hyperlaneVerifyFailureNotEnoughRecoveredSignatures
  , testCase "verifyFailureNotEnoughSignaturesToPassThreshold" hyperlaneVerifyFailureNotEnoughSignaturesToPassThreshold
  ]

hyperlaneEmptyObject :: Assertion
hyperlaneEmptyObject = do
  res <- runExceptT $ evalHyperlaneCommand $ mkObject []
  assertEqual "Should fail with missing command name" (Left "Unknown hyperlane command") res

hyperlaneEncodeTokenMessageERC20 :: Assertion
hyperlaneEncodeTokenMessageERC20 = do
  let
    res = encodeTokenMessageERC20 $ mkObject
        [ ("recipient", tStr $ asString ("recipient" :: Text))
          , ("amount", tLit $ LDecimal 3333333333333333333.333333333333333333) ]
  case res of
    Nothing -> assertFailure "Should get the result"
    Just t ->
      let
        expectedMessage :: Text = "0x0000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000281fa059c9e179daafc1235555555550000000000000000000000000000000000000000000000000000000000000009726563697069656e740000000000000000000000000000000000000000000000"
      in assertEqual "Should get encoded message" expectedMessage t

hyperlaneDecodeTokenMessageERC20 :: Assertion
hyperlaneDecodeTokenMessageERC20 = do
  let
    encodedMessage :: Text = "0x00000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000002e426101834d55550000000000000000000000000000000000000000000000000000000000000009726563697069656e740000000000000000000000000000000000000000000000"
  encodedBinary <- case decodeHex encodedMessage of
      Right r -> pure r
      Left _ -> assertFailure "hyperlaneDecodeTokenMessageERC20: failed to decode"
  tm <- runGetS getTokenMessageERC20 encodedBinary
  let
    expectedObject = TokenMessageERC20 "recipient" 3333333333333333333
  assertEqual "Should properly decode the object" expectedObject tm

hyperlaneDecimalToWord :: Assertion
hyperlaneDecimalToWord = assertEqual "" 10000000000000000000000000000000000000 (decimalToWord 10000000000000000000)

hyperlaneDecimalToWord2 :: Assertion
hyperlaneDecimalToWord2 = assertEqual "" 10333333333333333000 (decimalToWord 10.333333333333333)

hyperlaneWordToDecimal :: Assertion
hyperlaneWordToDecimal = assertEqual "" 3333333333333333333.333333333333333333 (wordToDecimal 3333333333333333333333333333333333333)

hyperlaneWordToDecimal2 :: Assertion
hyperlaneWordToDecimal2 = assertEqual "" 10.333333333333333 (wordToDecimal 10333333333333333000)

hyperlaneEncodeHyperlaneMessage :: Assertion
hyperlaneEncodeHyperlaneMessage = do
  let
    obj' = mkObject
        [ ("message", obj
          [ ("version", tLit $ LInteger 1)
          , ("nonce", tLit $ LInteger 325)
          , ("originDomain", tLit $ LInteger 626)
          , ("sender", tStr $ asString ("0x6b622d746f6b656e2d726f75746572" :: Text))
          , ("destinationDomain", tLit $ LInteger 1)
          , ("recipient", tStr $ asString ("0x71C7656EC7ab88b098defB751B7401B5f6d8976F" :: Text))
          , ("tokenMessage", obj
              [ ("recipient", tStr $ asString ("0x71C7656EC7ab88b098defB751B7401B5f6d8976F" :: Text))
              , ("amount", tLit $ LDecimal 10000000000000000000) ]
            )
          ])
        ]
  res <- runExceptT $ evalHyperlaneCommand obj'
  case res of
    Left err -> assertFailure $ "Should get the result" ++ show err
    Right o ->
      let
        expectedMessage :: Text = "0x01000001450000027230783662363232643734366636623635366532643732366637353734363537320000000100000000000000000000000071c7656ec7ab88b098defb751b7401b5f6d8976f0000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000785ee10d5da46d900f436a000000000000000000000000000000000000000000000000000000000000000000000002a30783731433736353645433761623838623039386465664237353142373430314235663664383937364600000000000000000000000000000000000000000000"
        expectedObject = mkObject
          [ ("encodedMessage", tStr $ asString expectedMessage)
          , ("messageId", tStr $ asString ("0x97d98aa7fdb548f43c9be37aaea33fca79680247eb8396148f1df10e6e0adfb7" :: Text))
          ]
      in assertEqual "Should get encoded message" expectedObject o

hyperlaneRecoverValidatorAnnouncementSuccess :: Assertion
hyperlaneRecoverValidatorAnnouncementSuccess = do
  let
    obj' = mkObject
        [ ("storageLocation", tStr $ asString ("storagelocation" :: Text))
        , ("signature", tStr $ asString ("0x53ba1fb621a19fbae9589c9d3fab7414a4ad75c45ddb6ddaf2e493a8a8ecf0af27256ed4f38b7304e80f653b462a79dcc22bbc975d7ce6f077f1cefe3afedabc1c" :: Text))
        ]
  res <- runExceptT $ evalHyperlaneCommand obj'
  case res of
    Left err -> assertFailure $ "Should get the result" ++ show err
    Right o ->
      let
        expectedObject = mkObject
          [ ("address", tStr $ asString ("0x6c414e7a15088023e28af44ad0e1d593671e4b15" :: Text))
          ]
      in assertEqual "Should get encoded message" expectedObject o

hyperlaneRecoverValidatorAnnouncementFailure :: Assertion
hyperlaneRecoverValidatorAnnouncementFailure = do
  let
    obj' = mkObject
        [ ("storageLocation", tStr $ asString ("storagelocation" :: Text))
        , ("signature", tStr $ asString ("0x43ba1fb621a19fbae9589c9d3fab7414a4ad75c45ddb6ddaf2e493a8a8ecf0af27256ed4f38b7304e80f653b462a79dcc22bbc975d7ce6f077f1cefe3afedabc1c" :: Text))
        ]
  res <- runExceptT $ evalHyperlaneCommand obj'
  case res of
    Left err -> assertEqual "Address recover failure" "Failed to recover address" err
    Right _ -> assertFailure "Should fail"

hyperlaneVerifySuccess :: Assertion
hyperlaneVerifySuccess = do
  let
    obj' = mkObject
        [ ("message", tStr $ asString ("0x030000000000007a69000000000000000000000000740b133dedb75bdb58d000054e873cae6fc565fb0000027236594b7a7170444e41546d5068554a7a63354131376d4a624658482d64426b560000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000002629f66e0c530000000000000000000000000000000000000000000000000000000000000000000426b3a39346333356162316264373032343365633637303439353037376637383436333733623464633565393737396437613637333262356365623666646530353963000000000000000000000000000000000000000000000000000000000000" :: Text))
        , ("validators", toTList tTyString def $ map (tStr . asString) [("0x71239e00ae942b394b3a91ab229e5264ad836f6f" :: Text)])
        , ("metadata", tStr $ asString ("0x0000000000000000000000005af5561c3017722a1fe42338cf5bfc615eac78ff271a508c6fe0999d87bef8e8f95ea00974e1e9dfa709f51630c71c348e201e9f00000000cc35e3e92c1a1979108506c67c7768047a99a8d6f57829ffb822bffaa81c1bb2597e0ddae84f945b046064306d45c0e8385485cfb777dcb16a8489073244dfeb1b" :: Text))
        , ("threshold", tLit $ LInteger 1)
        ]
  res <- runExceptT $ evalHyperlaneCommand obj'
  case res of
    Left err -> assertFailure $ "Should get the result" ++ show err
    Right o ->
      let
        expectedObject = mkObject
          [ ("message", obj
            [ ("version", tLit $ LInteger 3)
            , ("nonce", tLit $ LInteger 0)
            , ("originDomain", tLit $ LInteger 31337)
            , ("sender", tStr $ asString ("0x740b133dedb75bdb58d000054e873cae6fc565fb" :: Text))
            , ("destinationDomain", tLit $ LInteger 626)
            , ("recipient", tStr $ asString ("6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV" :: Text))
            , ("tokenMessage", obj
                [ ("recipient", tStr $ asString ("k:94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c" :: Text))
                , ("amount", tLit $ LDecimal 44) ]
              )
            ])
          , ("messageId", tStr $ asString ("0x2fcb90e3f4b580c2de0c68c41fea6a4055c0c084ed697a1605eae414bb54152e" :: Text))
          ]
      in assertEqual "Should get encoded message" expectedObject o

hyperlaneVerifySuccessMoreValidators :: Assertion
hyperlaneVerifySuccessMoreValidators = do
  let
    obj' = mkObject
        [ ("message", tStr $ asString ("0x030000000000007a69000000000000000000000000740b133dedb75bdb58d000054e873cae6fc565fb0000027236594b7a7170444e41546d5068554a7a63354131376d4a624658482d64426b560000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000002629f66e0c530000000000000000000000000000000000000000000000000000000000000000000426b3a39346333356162316264373032343365633637303439353037376637383436333733623464633565393737396437613637333262356365623666646530353963000000000000000000000000000000000000000000000000000000000000" :: Text))
        , ("validators", toTList tTyString def $
            map (tStr . asString) ["0x5DD34992E0994E9d3c53c1CCfe5C2e38d907338e", "0x71239e00ae942b394b3a91ab229e5264ad836f6f" :: Text])
        , ("metadata", tStr $ asString ("0x0000000000000000000000005af5561c3017722a1fe42338cf5bfc615eac78ff271a508c6fe0999d87bef8e8f95ea00974e1e9dfa709f51630c71c348e201e9f00000000cc35e3e92c1a1979108506c67c7768047a99a8d6f57829ffb822bffaa81c1bb2597e0ddae84f945b046064306d45c0e8385485cfb777dcb16a8489073244dfeb1b" :: Text))
        , ("threshold", tLit $ LInteger 1)
        ]
  res <- runExceptT $ evalHyperlaneCommand obj'
  case res of
    Left err -> assertFailure $ "Should get the result" ++ show err
    Right o ->
      let
        expectedObject = mkObject
          [ ("message", obj
            [ ("version", tLit $ LInteger 3)
            , ("nonce", tLit $ LInteger 0)
            , ("originDomain", tLit $ LInteger 31337)
            , ("sender", tStr $ asString ("0x740b133dedb75bdb58d000054e873cae6fc565fb" :: Text))
            , ("destinationDomain", tLit $ LInteger 626)
            , ("recipient", tStr $ asString ("6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV" :: Text))
            , ("tokenMessage", obj
                [ ("recipient", tStr $ asString ("k:94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c" :: Text))
                , ("amount", tLit $ LDecimal 44) ]
              )
            ])
          , ("messageId", tStr $ asString ("0x2fcb90e3f4b580c2de0c68c41fea6a4055c0c084ed697a1605eae414bb54152e" :: Text))
          ]
      in assertEqual "Should get encoded message" expectedObject o

hyperlaneVerifyFailure :: Assertion
hyperlaneVerifyFailure = do
  let
    obj' = mkObject
        [ ("message", tStr $ asString ("0x030000000000007a69000000000000000000000000740b133dedb75bdb58d000054e873cae6fc565fb0000027236594b7a7170444e41546d5068554a7a63354131376d4a624658482d64426b560000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000002629f66e0c530000000000000000000000000000000000000000000000000000000000000000000426b3a39346333356162316264373032343365633637303439353037376637383436333733623464633565393737396437613637333262356365623666646530353963000000000000000000000000000000000000000000000000000000000000" :: Text))
        -- validators: incorrect
        , ("validators", toTList tTyString def $ map (tStr . asString) [("0x5DD34992E0994E9d3c53c1CCfe5C2e38d907338e" :: Text)])
        , ("metadata", tStr $ asString ("0x0000000000000000000000005af5561c3017722a1fe42338cf5bfc615eac78ff271a508c6fe0999d87bef8e8f95ea00974e1e9dfa709f51630c71c348e201e9f00000000cc35e3e92c1a1979108506c67c7768047a99a8d6f57829ffb822bffaa81c1bb2597e0ddae84f945b046064306d45c0e8385485cfb777dcb16a8489073244dfeb1b" :: Text))
        , ("threshold", tLit $ LInteger 1)
        ]
  res <- runExceptT $ evalHyperlaneCommand obj'

  case res of
    Left err -> assertEqual "Verification should fail" "Verification failed" err
    Right _ -> assertFailure "Should fail"

hyperlaneVerifyFailureWrongTypeValidator :: Assertion
hyperlaneVerifyFailureWrongTypeValidator = do
  let
    obj' = mkObject
        [ ("message", tStr $ asString ("0x030000000000007a69000000000000000000000000740b133dedb75bdb58d000054e873cae6fc565fb0000027236594b7a7170444e41546d5068554a7a63354131376d4a624658482d64426b560000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000002629f66e0c530000000000000000000000000000000000000000000000000000000000000000000426b3a39346333356162316264373032343365633637303439353037376637383436333733623464633565393737396437613637333262356365623666646530353963000000000000000000000000000000000000000000000000000000000000" :: Text))
        -- validators: badly formatted
        , ("validators", toTList tTyInteger def $ map (tLit . LInteger) [2 :: Integer])
        , ("metadata", tStr $ asString ("0x0000000000000000000000005af5561c3017722a1fe42338cf5bfc615eac78ff271a508c6fe0999d87bef8e8f95ea00974e1e9dfa709f51630c71c348e201e9f00000000cc35e3e92c1a1979108506c67c7768047a99a8d6f57829ffb822bffaa81c1bb2597e0ddae84f945b046064306d45c0e8385485cfb777dcb16a8489073244dfeb1b" :: Text))
        , ("threshold", tLit $ LInteger 1)
        ]
  res <- runExceptT $ evalHyperlaneCommand obj'

  case res of
    Left err -> assertEqual "Verification should fail" "Only string validators are supported" err
    Right _ -> assertFailure "Should fail"

hyperlaneVerifyFailureIncorrectValidator :: Assertion
hyperlaneVerifyFailureIncorrectValidator = do
  let
    obj' = mkObject
        [ ("message", tStr $ asString ("0x030000000000007a69000000000000000000000000740b133dedb75bdb58d000054e873cae6fc565fb0000027236594b7a7170444e41546d5068554a7a63354131376d4a624658482d64426b560000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000002629f66e0c530000000000000000000000000000000000000000000000000000000000000000000426b3a39346333356162316264373032343365633637303439353037376637383436333733623464633565393737396437613637333262356365623666646530353963000000000000000000000000000000000000000000000000000000000000" :: Text))
        -- validators: badly formatted
        , ("validators", toTList tTyString def $ map (tStr . asString) [("badValidator" :: Text)])
        , ("metadata", tStr $ asString ("0x0000000000000000000000005af5561c3017722a1fe42338cf5bfc615eac78ff271a508c6fe0999d87bef8e8f95ea00974e1e9dfa709f51630c71c348e201e9f00000000cc35e3e92c1a1979108506c67c7768047a99a8d6f57829ffb822bffaa81c1bb2597e0ddae84f945b046064306d45c0e8385485cfb777dcb16a8489073244dfeb1b" :: Text))
        , ("threshold", tLit $ LInteger 1)
        ]
  res <- runExceptT $ evalHyperlaneCommand obj'

  case res of
    Left err -> assertEqual "Verification should fail" "Failed to decode a validator (badValidator):decodeHex: does not start with 0x" err
    Right _ -> assertFailure "Should fail"

hyperlaneVerifyFailureNotEnoughRecoveredSignatures :: Assertion
hyperlaneVerifyFailureNotEnoughRecoveredSignatures = do
  let
    obj' = mkObject
        [ ("message", tStr $ asString ("0x030000000000007a69000000000000000000000000740b133dedb75bdb58d000054e873cae6fc565fb0000027236594b7a7170444e41546d5068554a7a63354131376d4a624658482d64426b560000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000002629f66e0c530000000000000000000000000000000000000000000000000000000000000000000426b3a39346333356162316264373032343365633637303439353037376637383436333733623464633565393737396437613637333262356365623666646530353963000000000000000000000000000000000000000000000000000000000000" :: Text))
        -- validators: incorrect
        , ("validators", toTList tTyString def $ map (tStr . asString) [("0x5DD34992E0994E9d3c53c1CCfe5C2e38d907338e" :: Text)])
        -- metadata without signatures
        , ("metadata", tStr $ asString ("0x0000000000000000000000005af5561c3017722a1fe42338cf5bfc615eac78ff271a508c6fe0999d87bef8e8f95ea00974e1e9dfa709f51630c71c348e201e9f00000000" :: Text))
        , ("threshold", tLit $ LInteger 1)
        ]
  res <- runExceptT $ evalHyperlaneCommand obj'

  case res of
    Left err -> assertEqual "Verification should fail" "The number of recovered addresses from the signatures is less than threshold: 1" err
    Right _ -> assertFailure "Should fail"

-- | We pass 2 signatures, 1st one matches to the correct validator,
-- but there is no second valid validator for the 2nd signature, and the verification fails.
hyperlaneVerifyFailureNotEnoughSignaturesToPassThreshold :: Assertion
hyperlaneVerifyFailureNotEnoughSignaturesToPassThreshold = do
  let
    obj' = mkObject
        [ ("message", tStr $ asString ("0x030000000000007a69000000000000000000000000740b133dedb75bdb58d000054e873cae6fc565fb0000027236594b7a7170444e41546d5068554a7a63354131376d4a624658482d64426b560000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000002629f66e0c530000000000000000000000000000000000000000000000000000000000000000000426b3a39346333356162316264373032343365633637303439353037376637383436333733623464633565393737396437613637333262356365623666646530353963000000000000000000000000000000000000000000000000000000000000" :: Text))
        , ("validators", toTList tTyString def $ map (tStr . asString)
            -- validators: incorrect, correct, incorrect
            ["0x5DD34992E0994E9d3c53c1CCfe5C2e38d907338e" :: Text, "0x71239e00ae942b394b3a91ab229e5264ad836f6f", "0x55D34992E0994E9d3c53c1CCfe5C2e38d907338e"])
        -- metadata with the 2 same signatures
        , ("metadata", tStr $ asString ("0x0000000000000000000000005af5561c3017722a1fe42338cf5bfc615eac78ff271a508c6fe0999d87bef8e8f95ea00974e1e9dfa709f51630c71c348e201e9f00000000cc35e3e92c1a1979108506c67c7768047a99a8d6f57829ffb822bffaa81c1bb2597e0ddae84f945b046064306d45c0e8385485cfb777dcb16a8489073244dfeb1bcc35e3e92c1a1979108506c67c7768047a99a8d6f57829ffb822bffaa81c1bb2597e0ddae84f945b046064306d45c0e8385485cfb777dcb16a8489073244dfeb1b" :: Text))
        , ("threshold", tLit $ LInteger 2)
        ]
  res <- runExceptT $ evalHyperlaneCommand obj'

  case res of
    Left err -> assertEqual "Verification should fail" "Verification failed" err
    Right _ -> assertFailure "Should fail"
