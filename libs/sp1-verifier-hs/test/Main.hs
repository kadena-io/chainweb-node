{-# LANGUAGE  OverloadedStrings #-}
module Main (main) where

import PlonkBn254.Verify
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8  as B8
import qualified Data.ByteString.Base16 as B16
import Data.Maybe
import Data.List (stripPrefix)
import qualified System.Exit as System


fromHex :: String -> B.ByteString
fromHex s = case B16.decode $ B8.pack $ fromMaybe s $ stripPrefix "0x" s of
    Left e -> error $ "failed to decode hex string: " <> e
    Right r -> r

main :: IO ()
main = do
    progId <- PlonkVK <$> B.readFile "verifier-assets/v4.0.0-rc.3/vk.bin"
    let
        proof = Proof $ fromHex "0x1b34fe112678c3fac76791e6c3917e79c156349bd2e931846eea1f70c0d0eece8d2cdb13268714d65710184b2176072f6f21f48d599db6076270cf0c5d9f4bd7f10e2847071c4b547b7bcdd576d3d24ff508c3753f9ce5c65f0f8d30db655af15e8d2bc902a2d6b5e905138d3a4175a616b56b0dc48d6fa7de771dedcd97c05262b99ee6303787c022bd67c934b4edfea9088d65704b9b9c699af27d937107e5d70ff8c114a2b8645d8357161ae1a4bd4c8f0eb51f5518141a6f15b52413dad14f1c963e0bc13863494d86820fd58ddf51d02248e79592da16d1ef79252e367b6273247706c08a509d36020f91679a73e1fb6091b2e02b11663ce90f1f9a21ab7abfbb4f15c6de816df61dc35da318e4f41339379199810fc1f9d6d1e67812f67b4203850a768e0718995d05ec9dd70fe42b09fe7ad1271b119888bd7f99eed4ff62368a095f147f1f9d34c024b005578322b387304b518c267b1419245ec3770ad84eb92d239d1437efa234efee60044df444260ebb9331458cdba01039c75a6c9c5dd50162bf36ea9e4c684feeb7d0e68de15cce2f7587ce1edf8e6c384a29b76fc5901acfa1216cc518a61054649241e0c603a2e2f682180d46c745db79635ee40f6502279f66b396f1ac8057d10e887480eeccc94f2524875e267d58b338dedb8f31164bb18d579c2624d34b9e518e88edcdf3e1de007723ea7798548ddeb8f2aa0a042166ae7607896c914f9d7f6b2dac9190dd76a0a0d2d6659fb23e9dbc497943077ac90125e6479dc9f520a9eae9a32baca5b4bf5f8dda216bdae6b8b8d3f49d12f2ca25ffbfd01d16188df663e71e8bc4e4fc394ce64fc0bbf7af4f950bde282b66ce33a41f6e1174b57d4ded9d4cd5da65f0ac337123f81308bd9593828eb302a8ec70428c575c8e3c418a772a032b0d8f63a8dfe832b7b6c6a535b3761ac104f99cbc9e9c5d13914d3ba13feb89e433f4f47d2d9e32684f8c98ceb00e88b32bfabe1cc5d8766d3a302c38caaf85f49d826bb89a41dcca7d90bdded6f2c1b40b648f872b75d7ee6367b2ad76f712363c44f7e57f59eccb72271a7207a2824e27cad9dddd426baefc0228e90111c887b4b2c885b9a877bd63e5e388513a15e516fc90bc900533aef8744bf972f84c91e2022185cefa5261be1795ae77f53b6b04bcb6f8d3d8200992e406c9c98250f762289c31b7e21e6e3dde75e6707eb3e9"
        vk = SP1VKeyHash "0x00778ea1aadaac68666a873ad5c49e85506ca8c58deea6ad05313da66f14cfe8"
        pubParam = PublicInputs $ fromHex "0xf4010000f404000086070000"
    r <- verifyProof proof pubParam vk progId
    case r of
        VerificationSuccessful -> do
            putStrLn "Verification successful!"
            System.exitSuccess
        _ -> error "Verification failure"
