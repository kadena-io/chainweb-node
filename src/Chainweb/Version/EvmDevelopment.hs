{-# language LambdaCase #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}

module Chainweb.Version.EvmDevelopment
( evmDevnet
, pattern EvmDevelopment
) where

import qualified Data.Set as Set

import Chainweb.BlockCreationTime
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.Rule
import Chainweb.Version

import Pact.Core.Names

pattern EvmDevelopment :: ChainwebVersion
pattern EvmDevelopment <- ((== evmDevnet) -> True) where
    EvmDevelopment = evmDevnet

-- How to compute the hashes:
--
-- Mininal Payload Provider:
--
-- @
-- -- create dummy payload hashes
-- import Chainweb.PayloadProvider.Minimal.Payload
-- import Chainweb.Version.EvmDevelopment
--
-- mapM_ (\i -> T.putStrLn (sshow i <> " " <> encodeToText (view payloadHash $ genesisPayload EvmDevelopment $ unsafeChainId i))) [25..97]
-- @
--
-- EVM Payload Provider:
--
-- @
-- cabal run evm-genesis -- evm-development
-- @
--
-- Pact Provider:
--
-- TODO (use ea?)

evmDevnet :: ChainwebVersion
evmDevnet = withVersion evmDevnet $ ChainwebVersion
    { _versionCode = ChainwebVersionCode 0x0000_000a
    , _versionName = ChainwebVersionName "evm-development"
    , _versionForks = tabulateHashMap $ const $ onAllChains ForkAtGenesis
    , _versionUpgrades = onAllChains mempty
    , _versionGraphs = Bottom (minBound, d4k4ChainGraph)
    , _versionBlockDelay = BlockDelay 30_000_000
    , _versionWindow = WindowWidth 120
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionBootstraps = []
    , _versionGenesis = VersionGenesis
        { _genesisBlockTarget = onAllChains $ HashTarget (maxBound `div` 100_000)
        , _genesisTime = onChains
            -- FIXME: is the creation time for the pact headers correct?
            $ [ (unsafeChainId i, BlockCreationTime [timeMicrosQQ| 2025-01-01T00:00:00.000000 |]) | i <- [0..19] ]
            <> [ (unsafeChainId i, BlockCreationTime (Time (secondsToTimeSpan 1687223762))) | i <- [20..24] ]
            <> [ (unsafeChainId i, BlockCreationTime [timeMicrosQQ| 2025-01-01T00:00:00.000000 |]) | i <- [25..97] ]
        , _genesisBlockPayload = onChains $
            -- Pact Payload Provider
            [ (unsafeChainId 0, unsafeFromText "QzxVHFZ5go4PYd3QeAZhxP61hsVnICPw4BB9h-T3PDM")
            , (unsafeChainId 1, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 2, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 3, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 4, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 5, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 6, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 7, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 8, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 9, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 10, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 11, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 12, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 13, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 14, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 15, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 16, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 17, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 18, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 19, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            -- EVM Payload Provider
            , (unsafeChainId 20, unsafeFromText "JsA7DnXagbgNR_UQvlPv8juluB4bIJ1UwvnctkwxFqs")
            , (unsafeChainId 21, unsafeFromText "ua1zzX1zYiGsdhgUwnJCFrpbZi3F1e0x8ZXSryRq0NE")
            , (unsafeChainId 22, unsafeFromText "xgXdm3SBJ0pwcCR-3KzNGDy7VgS6YytBUZrNS2vYNOU")
            , (unsafeChainId 23, unsafeFromText "7Aq2PRVKFYXyNG0a15-ZaLjPz9MTO9YWg2_c8Rhdf3c")
            , (unsafeChainId 24, unsafeFromText "fpST-z8pu0KX7u-aA9ultehTlF8SdEOa4l4CyKwkG60")
            -- Minimal Payload Provider
            , (unsafeChainId 25, unsafeFromText "Gt116uJVwjUEM0f07u_x8-SUFHgGpoH1xf3sfPoe0ZY")
            , (unsafeChainId 26, unsafeFromText "NLRP0OiqRldiZclvoKBGhv9m5wO0TrhNKaZZslZuZvw")
            , (unsafeChainId 27, unsafeFromText "xAOBFMKZ_lSHNVhHW-GhbhiWh6sX48S2KPyyPMjAnZM")
            , (unsafeChainId 28, unsafeFromText "eav79QetdNuKo1HDW27Aeqbxr8oAt6Fh2U6ZtSfVyYU")
            , (unsafeChainId 29, unsafeFromText "jlcxQ4wXUrApJDUQRS8KxuSyoG7ZFEwLV4-92wmqLOQ")
            , (unsafeChainId 30, unsafeFromText "odUxYpZ8ZeW0WuQcibJH3isuI045MuEEeQqLrkivWEk")
            , (unsafeChainId 31, unsafeFromText "poJ65aDiZYYthbhrgUI2jJS_8vK1CTHRE2C6dLbjTXA")
            , (unsafeChainId 32, unsafeFromText "SC3ol1uFOHAewfQrMQczKrvhE8Dw1Bp9fBzNi9l_zTw")
            , (unsafeChainId 33, unsafeFromText "p38GsNdY-T8ULYN1OTpYyO1E7WOGwzE2g92aIPereUw")
            , (unsafeChainId 34, unsafeFromText "1V0SzqA9fHDOMnfvXvSd57H0r-iycjjLu3CKkRLcjRk")
            , (unsafeChainId 35, unsafeFromText "SeiRMENBv5XqR7wXUYSR2orjGHSUrtawx5gLrDCfZYA")
            , (unsafeChainId 36, unsafeFromText "JpsiIjm8aZEbyrcsMqLDneT0BNoJAJunk4BYqDO_1Y0")
            , (unsafeChainId 37, unsafeFromText "0yx4OFT3sbLS_wrqpULgPpgzzxmMRwM6VTmkasitTlA")
            , (unsafeChainId 38, unsafeFromText "Z3LE9JHGpSYuR3SvoYWPae7zB9-X05vFLjwD4GnXX_A")
            , (unsafeChainId 39, unsafeFromText "9ZHBlxeYPWjhvMySgVx3ThEyooYx724zjORClgmq8Lk")
            , (unsafeChainId 40, unsafeFromText "H3VBsNGh-SQE-0d_qlYSHnS2obzUeo6Zi1XDDvhndYo")
            , (unsafeChainId 41, unsafeFromText "N6hVHz6vo0frpS3eyqvtMeZg1eFbAMJ1CS315M-JpWw")
            , (unsafeChainId 42, unsafeFromText "9mo8CRwvTLLJ4cSQtErBfOIxzwpale-AwnbXPWQd184")
            , (unsafeChainId 43, unsafeFromText "SotTkMw2Eq5UbOObv5kyaUlMqHp6-PUSKDeLHWYCr2s")
            , (unsafeChainId 44, unsafeFromText "1IB27CAQ6MpXFV-OiyDVIxbXh91BK2Bl6PYiAwvyg-E")
            , (unsafeChainId 45, unsafeFromText "ou9ns1_Q72IsaXoVjCErGGGmzsI07IIx6Vo14gU0Fl8")
            , (unsafeChainId 46, unsafeFromText "dZsEBdKKdeLkTS35IV54npY01mB9HOWO3TvoXE0xoWg")
            , (unsafeChainId 47, unsafeFromText "oLbpBWhnhCdKHy_q009-06PYug12KMtA5u9mv82_I8s")
            , (unsafeChainId 48, unsafeFromText "coBTWu6iFvyDX_3W2dSnuwK9WheRa9_40kh564myiXw")
            , (unsafeChainId 49, unsafeFromText "fiOvn4JEAf7NXGAP3YkXhygalCnKCwzhe2dC1VO4YiU")
            , (unsafeChainId 50, unsafeFromText "UL6_gjNoxuryRd3xBj3OU00A6IjvHtdnggucOISDgc4")
            , (unsafeChainId 51, unsafeFromText "Wtu06D5r5DcNSZ8ZxxPv3Jvq4dtYW1PnHJtrxmdVSzw")
            , (unsafeChainId 52, unsafeFromText "yrSDpQnAtnw-WYVrXMd-zAt2ZCOBNz7mACG-UjX5Tl0")
            , (unsafeChainId 53, unsafeFromText "c743P-dTKKA6PpKCJ6ZC9im7bo1BpJWViZ6xjZJokkw")
            , (unsafeChainId 54, unsafeFromText "qlj-G-PO_TtM5mp2C6UI6GgVWR2H1um2v6VOMrXjX4M")
            , (unsafeChainId 55, unsafeFromText "XO6ZWLmRlyiGygt2pdDZpxZfwHrmkLsBM99rJSxa31M")
            , (unsafeChainId 56, unsafeFromText "jiXkFn_Nv73-X8d3xUtsY25lNN2g35sjSsu43X1pOEM")
            , (unsafeChainId 57, unsafeFromText "Dr15tRuU6JSXOARB46_r1DGb9e2WX4a61BoiJ9Uq5p0")
            , (unsafeChainId 58, unsafeFromText "KYlhzAW01sBwnO2dXl9_0BuRNV64nJJCSSo6JdDNMZA")
            , (unsafeChainId 59, unsafeFromText "ywo1yl72s89SQibkkKuRm4tmBnp8guONArOLa03lETU")
            , (unsafeChainId 60, unsafeFromText "t4u3_IuTXANdi2NrM2prmWCOFSc3AkrwHYziL1LSsEU")
            , (unsafeChainId 61, unsafeFromText "Ucp32OmDetbZPozGHES6F7HKqbAnIfynOsfCzUo3lDI")
            , (unsafeChainId 62, unsafeFromText "VEsZDVjM1lJkfJUWTXEyC7wH27vgDoviFD2Rt22vJ1I")
            , (unsafeChainId 63, unsafeFromText "NDsftZSHa8N1yDdkgQJQ1rk1J3vRFFxFzCSrd8SzEUo")
            , (unsafeChainId 64, unsafeFromText "80yIgBalnINZyprtYhZVCgOgMbB2DoW5Xq42FJf7nKk")
            , (unsafeChainId 65, unsafeFromText "BMi5YZ0GpYTemGe6x_FtXEI5JTO9rinekaNnhEV87Jc")
            , (unsafeChainId 66, unsafeFromText "uAIl8FGVbOPzitFHovTPJtPHaCQzYA8ZOaE6PxBx3Ng")
            , (unsafeChainId 67, unsafeFromText "tmzmkzngbTHNfywBySBDE-OLXwhjgn2gNhqK86uFRXk")
            , (unsafeChainId 68, unsafeFromText "MNkeZut1raJk7-Vh6Yf2HR-Lhf7x97ZYqtZvM-czHmA")
            , (unsafeChainId 69, unsafeFromText "BMa_Ucv2c0Q9TU9wE8HZaX5hFv919yqh57s_ijfb0aA")
            , (unsafeChainId 70, unsafeFromText "YZlG4QzDr285OQMCs5k9ZS6SNYSNjq2gr9RnJ1DmyBw")
            , (unsafeChainId 71, unsafeFromText "ZDwNBLBZSTRertPlENXC6CUj6StykJLucUnN_DydnLc")
            , (unsafeChainId 72, unsafeFromText "BtxzFZrXiRYOMouRmU7eA9pohvahn_GdKPREhcuGCY4")
            , (unsafeChainId 73, unsafeFromText "cGIf74TXx8V_XrUUr2B9MU8adtQeQc1hpk6XOey1GOU")
            , (unsafeChainId 74, unsafeFromText "j3hvD0Yjztjc_trqX4OMHPOqEWTd04GKvPfmt4r92D8")
            , (unsafeChainId 75, unsafeFromText "eSIZD24zvKNw-2OJtWJujayy3AKU2h11RhRWZUC6MtM")
            , (unsafeChainId 76, unsafeFromText "P3H3_4I6vJTQOszcrYreI6LOhSRVgVv7Hb0nFawzbsM")
            , (unsafeChainId 77, unsafeFromText "CKomlT6tiEs6DCa2VNy3519TjAiwFNx8EkkI54JKY0I")
            , (unsafeChainId 78, unsafeFromText "2NldG3su7R_RXpes25X09t8evnjNMuZoL8j3PcNMfXE")
            , (unsafeChainId 79, unsafeFromText "fMlZOHs-mzu5u2DXAiCKzhZNOlCMROY2YXxHMCdiHyo")
            , (unsafeChainId 80, unsafeFromText "mDocxA63bstQQ-vqzM02_avZmSjrPFFxcvcVa6ZCYMU")
            , (unsafeChainId 81, unsafeFromText "hqkNDhLpy-9usTAyu77mvwoCD8YDlW9O66EFQ98ARsg")
            , (unsafeChainId 82, unsafeFromText "jSGKQqn_KP4RbFsbTT_6VWFTj9WOqAqv-INoMyJntAw")
            , (unsafeChainId 83, unsafeFromText "fvOd-k-4j_OFmAQo1M2Vy1T2O18UkpcjRYlSMVcjxXo")
            , (unsafeChainId 84, unsafeFromText "s26jheDzrGXuWZ8mddMkIDU9UICfsRg9z-TQwDCRWos")
            , (unsafeChainId 85, unsafeFromText "ZTR2dcyy-ZgOX5OVsKMV0t4Bkp4mf2ocvUs8KGZVw74")
            , (unsafeChainId 86, unsafeFromText "AnmY1tCYiwIf35bZZXXdx3ZVOfmwsT0jvOlLA0s0NHY")
            , (unsafeChainId 87, unsafeFromText "aBL54Dj7EkUqrrWykyQfFGa9vCf3nS34QogArm33188")
            , (unsafeChainId 88, unsafeFromText "YGs9zbwtzR0FqoVwAITceumfKfCMilFYfSxkJU6pf70")
            , (unsafeChainId 89, unsafeFromText "J1CdN2TlCux7xAoX8m7fdGyJHV_IeEgboyQMvI2ToO8")
            , (unsafeChainId 90, unsafeFromText "_mphcuK5KOq2_-DQB9bqlM6C4f6eAnO7cCLSK--gLsE")
            , (unsafeChainId 91, unsafeFromText "TvoF0OcZ86zj_C9nExaubCzXRgGTGMQ9wViM1Zm0f-A")
            , (unsafeChainId 92, unsafeFromText "dAAvn6c3IkClNgEpjSTt-ZrPCG8YcqBsma_vvLnRji4")
            , (unsafeChainId 93, unsafeFromText "RBJolQY0GKyqcGBUef18tAr51aRS52IQ8HJoJX6EPR8")
            , (unsafeChainId 94, unsafeFromText "lPLkYdoQHw_auHSFnlcNL3fI_oI2b5jCCpaSXbw5rZ4")
            , (unsafeChainId 95, unsafeFromText "M4Webe0zta7bJ_53pHTIjU5d25TLG6FISfiLw1eFFgg")
            , (unsafeChainId 96, unsafeFromText "jiQb8cx7bl48fvqA6QeLXh_YXP2Bzg8gSroKGfceqUk")
            , (unsafeChainId 97, unsafeFromText "ZE5xfgDK6KW4q8o98qCWZ4NJL74NiMG1hu3DUZrHatI")
            ]
        }

    -- still the *default* block gas limit is set, see
    -- defaultChainwebConfiguration._configBlockGasLimit
    , _versionMaxBlockGasLimit = Bottom (minBound, Nothing)
    , _versionCheats = VersionCheats
        { _disablePow = True
        , _fakeFirstEpochStart = True
        , _disablePact = False
        }
    , _versionDefaults = VersionDefaults
        { _disablePeerValidation = True
        , _disableMempoolSync = False
        }
    , _versionVerifierPluginNames = onAllChains $ Bottom
        (minBound, Set.fromList $ map VerifierName ["hyperlane_v3_message", "allow"])
    , _versionQuirks = noQuirks
    , _versionServiceDate = Nothing

    -- FIXME make this safe for graph changes
    , _versionPayloadProviderTypes = onChains
        $ [ (unsafeChainId i, PactProvider) | i <- [0..19] ]
        <> [ (unsafeChainId i, EvmProvider (1789 - 20 + int i)) | i <- [20..24] ]
        <> [ (unsafeChainId i, MinimalProvider) | i <- [25..97] ]
    }
