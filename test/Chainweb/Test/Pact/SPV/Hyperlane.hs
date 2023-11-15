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
        expectedMessage :: Text = "0x01000001450000027200000000000000000000000000000000006b622d746f6b656e2d726f757465720000000100000000000000000000000071c7656ec7ab88b098defb751b7401b5f6d8976f0000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000785ee10d5da46d900f436a000000000000000000000000000000000000000000000000000000000000000000000002a30783731433736353645433761623838623039386465664237353142373430314235663664383937364600000000000000000000000000000000000000000000"
        expectedObject = mkObject
          [ ("encodedMessage", tStr $ asString expectedMessage)
          , ("messageId", tStr $ asString ("0x42f79cdedbfc03af3296d1b337255cc53f870c8b2bb02107bded2a082bd02323" :: Text))
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
        [ ("message", tStr $ asString ("0x01000001450000027200000000000000000000000000000000006b622d746f6b656e2d726f757465720000000100000000000000000000000071c7656ec7ab88b098defb751b7401b5f6d8976f00000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000008ac7230489e80000000000000000000000000000000000000000000000000000000000000000002a30783731433736353645433761623838623039386465664237353142373430314235663664383937364600000000000000000000000000000000000000000000" :: Text))
        , ("validators", toTList tTyString def $ map (tStr . asString) [("0x4BD34992E0994E9d3c53c1CCfe5C2e38d907338e" :: Text)])
        , ("metadata", tStr $ asString ("0x0000000000000000000000002e234dae75c793f67a35089c9d99245e1c58470b00000000000000000000000000000000000000000000000000000000000000ad0000000f0e1c8be19e9e2bd14665599b8e8ed1f3dbca562788e5844975770eb31380b3ae5de03487e89a1d3c42fad8aac486a06e1af6b3478ec0d148c0c8566c404537291b" :: Text))
        , ("threshold", tLit $ LInteger 1)
        ]
  res <- runExceptT $ evalHyperlaneCommand obj'
  case res of
    Left err -> assertFailure $ "Should get the result" ++ show err
    Right o ->
      let
        expectedObject = mkObject
          [ ("message", obj
            [ ("version", tLit $ LInteger 1)
            , ("nonce", tLit $ LInteger 325)
            , ("originDomain", tLit $ LInteger 626)
            , ("sender", tStr $ asString ("0x00000000006b622d746f6b656e2d726f75746572" :: Text))
            , ("destinationDomain", tLit $ LInteger 1)
            , ("recipient", tStr $ asString ("0x71c7656ec7ab88b098defb751b7401b5f6d8976f" :: Text))
            , ("tokenMessage", obj
                [ ("recipient", tStr $ asString ("0x71C7656EC7ab88b098defB751B7401B5f6d8976F" :: Text))
                , ("amount", tLit $ LDecimal 10) ]
              )
            ])
          , ("messageId", tStr $ asString ("0x3b2cc397eb65735078b967c5497a028ee35a0ebe89478a2417b2e9c7e891e0f3" :: Text))
          ]
      in assertEqual "Should get encoded message" expectedObject o

hyperlaneVerifySuccessMoreValidators :: Assertion
hyperlaneVerifySuccessMoreValidators = do
  let
    obj' = mkObject
        [ ("message", tStr $ asString ("0x01000001450000027200000000000000000000000000000000006b622d746f6b656e2d726f757465720000000100000000000000000000000071c7656ec7ab88b098defb751b7401b5f6d8976f00000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000008ac7230489e80000000000000000000000000000000000000000000000000000000000000000002a30783731433736353645433761623838623039386465664237353142373430314235663664383937364600000000000000000000000000000000000000000000" :: Text))
        , ("validators", toTList tTyString def $
            map (tStr . asString) ["0x5DD34992E0994E9d3c53c1CCfe5C2e38d907338e", "0x4BD34992E0994E9d3c53c1CCfe5C2e38d907338e" :: Text])
        , ("metadata", tStr $ asString ("0x0000000000000000000000002e234dae75c793f67a35089c9d99245e1c58470b00000000000000000000000000000000000000000000000000000000000000ad0000000f0e1c8be19e9e2bd14665599b8e8ed1f3dbca562788e5844975770eb31380b3ae5de03487e89a1d3c42fad8aac486a06e1af6b3478ec0d148c0c8566c404537291b" :: Text))
        , ("threshold", tLit $ LInteger 1)
        ]
  res <- runExceptT $ evalHyperlaneCommand obj'
  case res of
    Left err -> assertFailure $ "Should get the result" ++ show err
    Right o ->
      let
        expectedObject = mkObject
          [ ("message", obj
            [ ("version", tLit $ LInteger 1)
            , ("nonce", tLit $ LInteger 325)
            , ("originDomain", tLit $ LInteger 626)
            , ("sender", tStr $ asString ("0x00000000006b622d746f6b656e2d726f75746572" :: Text))
            , ("destinationDomain", tLit $ LInteger 1)
            , ("recipient", tStr $ asString ("0x71c7656ec7ab88b098defb751b7401b5f6d8976f" :: Text))
            , ("tokenMessage", obj
                [ ("recipient", tStr $ asString ("0x71C7656EC7ab88b098defB751B7401B5f6d8976F" :: Text))
                , ("amount", tLit $ LDecimal 10) ]
              )
            ])
          , ("messageId", tStr $ asString ("0x3b2cc397eb65735078b967c5497a028ee35a0ebe89478a2417b2e9c7e891e0f3" :: Text))
          ]
      in assertEqual "Should get encoded message" expectedObject o

hyperlaneVerifyFailure :: Assertion
hyperlaneVerifyFailure = do
  let
    obj' = mkObject
        [ ("message", tStr $ asString ("0x01000001450000027200000000000000000000000000000000006b622d746f6b656e2d726f757465720000000100000000000000000000000071c7656ec7ab88b098defb751b7401b5f6d8976f00000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000008ac7230489e80000000000000000000000000000000000000000000000000000000000000000002a30783731433736353645433761623838623039386465664237353142373430314235663664383937364600000000000000000000000000000000000000000000" :: Text))
        -- validators: incorrect
        , ("validators", toTList tTyString def $ map (tStr . asString) [("0x5DD34992E0994E9d3c53c1CCfe5C2e38d907338e" :: Text)])
        , ("metadata", tStr $ asString ("0x0000000000000000000000002e234dae75c793f67a35089c9d99245e1c58470b00000000000000000000000000000000000000000000000000000000000000ad0000000f0e1c8be19e9e2bd14665599b8e8ed1f3dbca562788e5844975770eb31380b3ae5de03487e89a1d3c42fad8aac486a06e1af6b3478ec0d148c0c8566c404537291b" :: Text))
        , ("threshold", tLit $ LInteger 1)
        ]
  res <- runExceptT $ evalHyperlaneCommand obj'

  case res of
    Left err -> assertEqual "Verification should fail" "Verification failed" err
    Right _ -> assertFailure "Should fail"

hyperlaneVerifyFailureIncorrectValidator :: Assertion
hyperlaneVerifyFailureIncorrectValidator = do
  let
    obj' = mkObject
        [ ("message", tStr $ asString ("0x01000001450000027200000000000000000000000000000000006b622d746f6b656e2d726f757465720000000100000000000000000000000071c7656ec7ab88b098defb751b7401b5f6d8976f00000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000008ac7230489e80000000000000000000000000000000000000000000000000000000000000000002a30783731433736353645433761623838623039386465664237353142373430314235663664383937364600000000000000000000000000000000000000000000" :: Text))
        -- validators: badly formatted
        , ("validators", toTList tTyString def $ map (tStr . asString) [("badValidator" :: Text)])
        , ("metadata", tStr $ asString ("0x0000000000000000000000002e234dae75c793f67a35089c9d99245e1c58470b00000000000000000000000000000000000000000000000000000000000000ad0000000f0e1c8be19e9e2bd14665599b8e8ed1f3dbca562788e5844975770eb31380b3ae5de03487e89a1d3c42fad8aac486a06e1af6b3478ec0d148c0c8566c404537291b" :: Text))
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
        [ ("message", tStr $ asString ("0x01000001450000027200000000000000000000000000000000006b622d746f6b656e2d726f757465720000000100000000000000000000000071c7656ec7ab88b098defb751b7401b5f6d8976f00000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000008ac7230489e80000000000000000000000000000000000000000000000000000000000000000002a30783731433736353645433761623838623039386465664237353142373430314235663664383937364600000000000000000000000000000000000000000000" :: Text))
        -- validators: incorrect
        , ("validators", toTList tTyString def $ map (tStr . asString) [("0x5DD34992E0994E9d3c53c1CCfe5C2e38d907338e" :: Text)])
        -- metadata without signatures
        , ("metadata", tStr $ asString ("0x0000000000000000000000002e234dae75c793f67a35089c9d99245e1c58470b00000000000000000000000000000000000000000000000000000000000000ad0000000f" :: Text))
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
        [ ("message", tStr $ asString ("0x01000001450000027200000000000000000000000000000000006b622d746f6b656e2d726f757465720000000100000000000000000000000071c7656ec7ab88b098defb751b7401b5f6d8976f00000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000008ac7230489e80000000000000000000000000000000000000000000000000000000000000000002a30783731433736353645433761623838623039386465664237353142373430314235663664383937364600000000000000000000000000000000000000000000" :: Text))
        , ("validators", toTList tTyString def $ map (tStr . asString)
            -- validators: incorrect, correct, incorrect
            ["0x5DD34992E0994E9d3c53c1CCfe5C2e38d907338e" :: Text, "0x4BD34992E0994E9d3c53c1CCfe5C2e38d907338e", "0x55D34992E0994E9d3c53c1CCfe5C2e38d907338e"])
        -- metadata with the 2 same signatures
        , ("metadata", tStr $ asString ("0x0000000000000000000000002e234dae75c793f67a35089c9d99245e1c58470b00000000000000000000000000000000000000000000000000000000000000ad0000000f0e1c8be19e9e2bd14665599b8e8ed1f3dbca562788e5844975770eb31380b3ae5de03487e89a1d3c42fad8aac486a06e1af6b3478ec0d148c0c8566c404537291b0e1c8be19e9e2bd14665599b8e8ed1f3dbca562788e5844975770eb31380b3ae5de03487e89a1d3c42fad8aac486a06e1af6b3478ec0d148c0c8566c404537291b" :: Text))
        , ("threshold", tLit $ LInteger 2)
        ]
  res <- runExceptT $ evalHyperlaneCommand obj'

  case res of
    Left err -> assertEqual "Verification should fail" "Verification failed" err
    Right _ -> assertFailure "Should fail"
