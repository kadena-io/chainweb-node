{-# language
    DerivingStrategies
    , ImportQualifiedPost
    , LambdaCase
    , OverloadedRecordDot
    , RecordWildCards
    , ScopedTypeVariables
    , TypeApplications
#-}

module Chainweb.BlockHeaderDB.HeaderOracle
    (
        -- * Oracle type
        HeaderOracle
        -- ** Accessors
        , lowerBound
        , upperBound

        -- * Oracle creation
        , create
        , createSpv
        -- ** Oracle creation Exceptions
        , InvalidHeaderOracleBounds(..)

        -- * Oracle querying
        , query
        , HeaderOracleResponse(..)
    )
    where

import Chainweb.BlockHash (BlockHash)
import Chainweb.BlockHeader (BlockHeader, blockHash, blockHeight, genesisBlockHeader)
import Chainweb.BlockHeaderDB (BlockHeaderDb)
import Chainweb.TreeDB (seekAncestor)
import Chainweb.TreeDB qualified as TreeDB
import Chainweb.Version (_chainwebVersion)
import Chainweb.Version.Guards (minimumBlockHeaderHistory)
import Control.Exception (Exception(..), throwIO)
import Control.Lens (view)
import Numeric.Natural (Natural)

-- | A 'HeaderOracle' is a 'BlockHeaderDb' with a lower and upper bound, and the only
--   query on it is whether a given block is within those bounds.
data HeaderOracle = HeaderOracle
    { lowerBound :: !BlockHeader
    , upperBound :: !BlockHeader
    , db :: !BlockHeaderDb
    }

-- | Create a new 'HeaderOracle' specifically for SPV verification, with the given upper bound.
--
--   The lower bound of the oracle is determined by the 'spvProofExpirationWindow'.
createSpv :: BlockHeaderDb -> BlockHeader -> IO HeaderOracle
createSpv db upperBound = do
    let mWindow = minimumBlockHeaderHistory (_chainwebVersion upperBound) (view blockHeight upperBound)
    let gh = genesisBlockHeader (_chainwebVersion upperBound) upperBound
    let defaultOracle = create db gh upperBound

    case mWindow of
        Nothing -> do
            defaultOracle
        Just window -> do
            mWindowthAncestor <- do
                let w = fromIntegral @_ @Natural window
                let ur = TreeDB.rank upperBound
                let gr = TreeDB.rank gh
                -- Saturated subtraction with the genesis rank as origin
                if w + gr <= ur
                then do
                    seekAncestor db upperBound (ur - w)
                else do
                    return Nothing
            case mWindowthAncestor of
                Nothing -> do
                    defaultOracle
                Just windowthAncestor -> do
                    create db windowthAncestor upperBound

-- | Exception thrown when creating a new 'HeaderOracle' with invalid bounds.
data InvalidHeaderOracleBounds = InvalidHeaderOracleBounds !BlockHash !BlockHash
    deriving stock (Eq, Show)

instance Exception InvalidHeaderOracleBounds where
    displayException (InvalidHeaderOracleBounds l u) = "Header Oracle `create` called with a (lower, upper) pair, where `lower` is not an ancestor of `upper`: " ++ show (l, u)

-- | Create a new 'HeaderOracle' with the given lower and upper bounds.
--
--   Throws an 'InvalidHeaderOracleBounds' exception if the lower bound is not an ancestor of the upper bound.
create :: BlockHeaderDb -> BlockHeader -> BlockHeader -> IO HeaderOracle
create db lowerBound upperBound = do
    valid <- TreeDB.ancestorOf db (view blockHash lowerBound) (view blockHash upperBound)
    if valid
    then do
        return HeaderOracle
            { ..
            }
    else do
        throwIO $ InvalidHeaderOracleBounds (view blockHash lowerBound) (view blockHash upperBound)

-- | The response from the oracle when querying a block.
--
--   We would ideally like something like
--   @
--   data Response = TooLate | OutOfBounds | InBounds
--   @
--   but shallow nodes can't determine the difference between something
--   that is too late and something that is out of bounds.
data HeaderOracleResponse
    = OutOfBounds
        -- ^ The block is not within the oracle's bounds.
    | InBounds
        -- ^ The block is within the oracle's bounds.
    deriving stock (Eq)

-- | Query the oracle, asking if the block is within its bounds.
query :: ()
    => HeaderOracle
    -> BlockHash
    -> IO HeaderOracleResponse
query oracle subject = do
    -- Check if the lower bound is an ancestor of the header
    -- Check if the header is an ancestor of the upper bound
    r <- (&&)
        <$> TreeDB.ancestorOf oracle.db (view blockHash oracle.lowerBound) subject
        <*> TreeDB.ancestorOf oracle.db subject (view blockHash oracle.upperBound)
    return $ if r
        then InBounds
        else OutOfBounds
