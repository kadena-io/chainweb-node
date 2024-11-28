{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: KnownGraphs
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Main (main) where

import Chainweb.Graph
import Chainweb.Utils
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.DiGraph qualified as G
import Data.Text qualified as T
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    graphs <- traverse (fromText . T.pack) args
    BL8.putStrLn $ encode $ knownChainGraph <$> graphs

instance ToJSON KnownGraph where
    toJSON = toJSON . toText
    {-# INLINE toJSON #-}

instance ToJSON ChainGraph where
    toJSON g = object
        [ "name" .= view chainGraphKnown g
        , "diameter" .= diameter g
        , "degree" .= degree g
        , "order" .= order g
        , "size" .= size g
        , "adjacents" .= G.adjacencySets (view chainGraphGraph g)
        ]
    {-# INLINE toJSON #-}

