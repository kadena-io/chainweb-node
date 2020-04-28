module Chainweb.Utils.Bench
  (
    NoopNFData(..)
  ) where

import Control.DeepSeq



-- | Newtype to provide a noop NFData instance.
-- Intended for use in criterion's 'envWithCleanup'
-- which wants environment values to be NFData.
newtype NoopNFData a = NoopNFData a
  deriving (Show)
instance NFData (NoopNFData a) where
  rnf _ = ()
