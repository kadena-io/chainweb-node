{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}


-- |
-- Module: Chainweb.ChainDB.Sync
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Sync a local `ChainDb` with that of some remote peer.
--

module Chainweb.ChainDB.Sync
  ( -- * Syncronizing a Chain
    sync
  ) where

import           Chainweb.BlockHeader (BlockHeader(..), BlockHeight(..))
import           Chainweb.ChainDB
import           Chainweb.ChainDB.RestAPI.Client (headersClient)
import           Chainweb.RestAPI.Utils (Page(..))
import           Control.Arrow ((&&&))
import           Control.Monad.Trans.State.Strict
import           Data.Foldable (toList)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Refined hiding (NonEmpty)
import           Servant.Client hiding (client)
import           Streaming
import qualified Streaming.Prelude as SP

-- | Some abstract notion of a peer to connect to.
-- Assumed to never yield phoney `BlockHeader`s.
data Peer

-- TODO The real version of this will be present elsewhere.
-- | The diameter of the current chain graph.
newtype Diameter = Diameter { diameter :: Refined (Positive && LessThan 10) Int }

-- | `BlockHeader`s in a `ChainDb` that have no children.
newtype Leaves = Leaves { unleaves :: NonEmpty BlockHeader }

-- TODO How to model failure?
-- | Given a `Peer` to connect to, fetch all `BlockHeader`s that exist
-- in the peer's chain but not our local given `ChainDb`, and sync them.
sync :: Diameter -> Peer -> ChainDb -> IO ()
sync d p db = do
  s <- snapshot db
  let mleaves :: Maybe Leaves
      mleaves = traverse (`getEntry` s) (toList $ branches s)
        >>= NEL.nonEmpty
        >>= Just . Leaves . fmap dbEntry
  case mleaves of
    Nothing -> undefined
    Just leaves -> putThemIn s . headers p $ lowLeaf d leaves

-- | \(\mathcal{O}(n \log n)\).
leafMap :: Leaves -> M.Map BlockHeight BlockHeader
leafMap = M.fromList . map (_blockHeight &&& id) . toList . unleaves

-- TODO Add unit tests for this.
-- | Given some leaves, find the lowest one such that it's at most only
-- (diameter * 2) in height away from the highest.
lowLeaf :: Diameter -> Leaves -> BlockHeader
-- The `lookupGE` should never fail. In the degenerate case, the difference
-- between the high and low points would be negative, defaulting to a
-- `BlockHeight` lookup of 0 (the genesis block).
--
-- To restore this function's totality, we offer the `fromMaybe`, the default of
-- which should never be needed. The default given is the only thing known
-- to exist: the head of the `NonEmpty` list.
lowLeaf d l = fromMaybe (NEL.head $ unleaves l) . fmap snd $ M.lookupGE lowH lmap
  where
    lmap = leafMap l
    lowH = BlockHeight . fromIntegral $ max (high - low) 0
    high = fromIntegral . _blockHeight . NEL.head $ unleaves l
    low  = 2 * unrefine (diameter d)

-- TODO How to model failure of the invariants? Exceptions?
-- | Fetch all `BlockHeader`s from a peer from a given `BlockHeight` and higher.
--
-- INVARIANTS:
--   * The `BlockHeader`s are streamed in order of `BlockHeight`, lowest to highest.
--     We assume the server will do this correctly.
headers :: Peer -> BlockHeader -> Stream (Of BlockHeader) IO ()
headers p h = do
    env <- flip mkClientEnv (url p) <$> lift (newManager tlsManagerSettings)
    headers' env h

-- | Manage a connection to a `HeadersApi` and automatically handle its paging.
headers' :: ClientEnv -> BlockHeader -> Stream (Of BlockHeader) IO ()
headers' env h = g $ client Nothing
  where
    height = 1 + fromIntegral (_blockHeight h)

    -- TODO What's the best limit value? 100? 1000?
    client :: Maybe (Key 'Unchecked) -> ClientM (Page (Key 'Unchecked) (Entry 'Unchecked))
    client next = headersClient (_blockChainwebVersion h) (_blockChainId h) (Just 100) next (Just height) Nothing Nothing

    g :: ClientM (Page (Key 'Unchecked) (Entry 'Unchecked)) -> Stream (Of BlockHeader) IO ()
    g c = do
        lift (runClientM c env) >>= \case
            Left err   -> undefined  -- TODO
            Right page -> f page

    f :: Page (Key 'Unchecked) (Entry 'Unchecked) -> Stream (Of BlockHeader) IO ()
    f page = do
        SP.map dbEntry . SP.each $ _pageItems page
        maybe (pure ()) (g . client . Just) $ _pageNext page

url :: Peer -> BaseUrl
url p = BaseUrl Https (ip p) 443 ""

-- TODO In the real version, this is just a field accessor.
ip :: Peer -> String
ip = undefined

-- TODO How often to call `syncSnapshot`?
-- Currently it calls it after all new `BlockHeader`s have been inserted.
-- | Add the new remote `BlockHeader`s to our local chain.
putThemIn :: Snapshot -> Stream (Of BlockHeader) IO () -> IO ()
putThemIn s bs = execStateT (SP.mapM_ goIn $ hoist lift bs) s >>= void . syncSnapshot

-- TODO This is copied from `Persist` and should be factored out into something common.
goIn :: BlockHeader -> StateT Snapshot IO ()
goIn bh = get >>= insert (entry bh) >>= put
{-# INLINE goIn #-}
