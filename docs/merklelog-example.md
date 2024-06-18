In this example we are going create a Merkle tree log for a records of bug
reports.

This example can be compiled with GHC using
[markdown-lit](http://hackage.haskell.org/package/markdown-unlit).

We first need to enable a few GHC language extensions and define the module
header.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module MerkleLog
(
-- * Data Model
  BugId(..)
, BugTime(..)
, BugDescription(..)
, BugComment(..)
, BugReport(..)
, BugReportStore(..)
, BugReportIdx(..)
, bugReportIdxFingerPrint
, newBugReport

-- * Merkle Log Universe
, BugReportTag(..)

-- * Creating Witnesses
, proofCommentInReport
, proofDate
, verifyProof

-- * Testing
, testCommentProof
, testDateProof
) where
```

We also require a few external modules that we import next:

```haskell
import Control.Monad.Catch

import Crypto.Hash.Algorithms

import Data.Bifunctor
import qualified Data.ByteString.Char8 as B8
import Data.Foldable
import Data.List (sort)
import qualified Data.List.NonEmpty as N
import qualified Data.Map.Strict as M
import Data.MerkleLog
import qualified Data.Sequence as S
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time

import Text.Read (readEither)

-- internal modules

import Chainweb.Crypto.MerkleLog
```

# Data Model

For each field in a bug report we create a type for additional type safety and
to avoid orphan instances.


```haskell
newtype BugId = BugId (MerkleRoot SHA512t_256) deriving (Show, Eq, Ord)
newtype BugTime = BugTime UTCTime deriving (Show, Read, Eq, Ord)
newtype BugDescription = BugDescription T.Text deriving (Show, Read, Eq, Ord)
newtype BugComment = BugComment T.Text deriving (Show, Read, Eq, Ord)

-- | A bug report
--
data BugReport = BugReport
    { _bugReportId :: !BugId
    , _bugReportDate :: !BugTime
    , _bugReportDescription :: !BugDescription
    , _bugReportComments :: !(S.Seq BugComment)
    }
    deriving (Show, Eq)

-- | A Key Value Store for 'BugReport's
--
newtype BugReportStore = BugReportStore
    { _bugReportStore :: M.Map BugId BugReport }
    deriving (Show, Semigroup, Monoid)

-- | An index that defines an order on 'BugReport's for inclusion in a
-- Merkle tee.
--
data BugReportIdx = BugReportIdx
    { _bugReportIdx :: !(Set.Set BugId)
    , _bugReportTree :: !(MerkleTree SHA512t_256)
    }
    deriving (Show)

bugReportIdxFingerPrint :: BugReportIdx -> (MerkleRoot SHA512t_256)
bugReportIdxFingerPrint = merkleRoot . _bugReportTree
```

The index is needed, because the entries in a Merkle log must be ordered. Any
(total) order is fine, as long as as each item has a well defined position. Note
that this is different from Merkle Tries are Merkle Patricia trees where the
position in the tree is implicitly derived from the order of the hash value of
the entries.

We also store the Merkle tree for table, because the table is not of fixed size
and it could be costly to recompute the Merkle tree each time it is needed.

#  Merkle Log Universe

We have two levels of logs that we want to track in Merkle trees:

1.  The fields and the comments within a bug report.
2.  The log of bug report records.

In the end we want to support inclusion proof for queries across both levels.
For instance, we want to provide witnesses for questions like

*   Has there been a bug report for the first week of this month?
*   Has there been a bug report that include "#233" in the description?
*   For bug with Id "...", has there been a comment that mentions "@john"?

For that we have to be able to construct proofs for individual fields within a
given bug report record as well as proofs for the inclusion of a particular bug
report in the log of all bug reports.

First we have to define the universe that fixes the hash algorithm and defines
the domain of types for leaf entries in a Merkle Log.

The Universe is defined at kind level. Each type constructor of the universe
kind is used as tag for a type in the domain. The purpose of the universe is
that we can't accidentally mix values from different contexts within a single
Merkle tree and mix different hash algorithms within a tree. It also makes sure
that each leaf entry in the tree is tagged with its type. These are important
security properties that ensure that the inclusion proofs are sound. Any
violation could potentially allow an attacker to issue phony proofs.

```haskell
data BugReportTag = BugIdTag | BugTimeTag | BugDescriptionTag | BugCommentTag

instance MerkleUniverse BugReportTag where
    type HashAlg BugReportTag = SHA512t_256
    type MerkleTagVal BugReportTag 'BugIdTag = 0x0001
    type MerkleTagVal BugReportTag 'BugTimeTag = 0x0002
    type MerkleTagVal BugReportTag 'BugDescriptionTag = 0x0003
    type MerkleTagVal BugReportTag 'BugCommentTag = 0x0004
```

# Merkle tree entries

After we fix our universe, we have to define how the individual entries are
mapped to Merkle tree nodes. A Merkle tree node is either an input node or a
tree node. Tree nodes are roots of nested Merkle trees. Input nodes are leafs of
the tree and hold a binary representation of the respective value. It is up to
the user to define the binary representation for each Input node type.

In addition the user has to assign a tag from the universe to each entry type.
The tag is used to create a unique representation for each type in the tree so
that it is guaranteed that there can't be collisions of values of different types
in the tree. It is possible to assign the same tag to different types in the
universe, which means that an inclusion proofs can't differentiate between both
types. For instance, if the user would assign the same tag, say `TextTag`, for
`BugDescription` and `BugComment` one could now proof "There is a comment or
description that contains the word `@john`", but one couldn't prove this fact
just for comments. Actually, one would prove that "there is an entry, that is
tagged with `TextTag` that contains the word `@john`".

So, here are the definitions of entries:

```haskell
instance IsMerkleLogEntry BugReportTag BugId where
    type Tag BugId = 'BugIdTag
    toMerkleNode (BugId r) = TreeNode r
    fromMerkleNode (TreeNode r) = return $ BugId r
    fromMerkleNode (InputNode _) = throwM expectedTreeNodeException

instance IsMerkleLogEntry BugReportTag BugTime where
    type Tag BugTime = 'BugTimeTag
    toMerkleNode (BugTime t) = InputNode (B8.pack $ show t)
    fromMerkleNode (InputNode b) = BugTime <$> case readEither (B8.unpack b) of
        Left e -> throwM $ MerkleLogDecodeException $ T.pack e <> ": " <> T.decodeUtf8 b
        Right x -> return x
    fromMerkleNode (TreeNode _) = throwM expectedInputNodeException

instance IsMerkleLogEntry BugReportTag BugDescription where
    type Tag BugDescription = 'BugDescriptionTag
    toMerkleNode (BugDescription t) = InputNode (T.encodeUtf8 t)
    fromMerkleNode (InputNode b) = return $ BugDescription $ T.decodeUtf8 b
    fromMerkleNode (TreeNode _) = throwM expectedInputNodeException

instance IsMerkleLogEntry BugReportTag BugComment where
    type Tag BugComment = 'BugCommentTag
    toMerkleNode (BugComment t) = InputNode (T.encodeUtf8 t)
    fromMerkleNode (InputNode b) = return $ BugComment $ T.decodeUtf8 b
    fromMerkleNode (TreeNode _) = throwM expectedInputNodeException
```

In many use cases, one has already defined serialization instances for the
involved types, which simplifies the definition of `IsMerkleLogEntry` instances.

#  Merkle Log instances

Note, that the 'BugId' type is used as a Merkle tree root, while the other types
are `InputNodes`.

With the entries defined, we can now create the Merkle logs. For that we have to
provide instances of `HasMerkleLog`, that provide a representation of a type in
terms of a fixed polymorphic list of headers fields and a monomorphic sequence
of body entries:

```haskell
instance HasMerkleLog BugReportTag BugReport where
    type MerkleLogHeader BugReport = '[BugTime, BugDescription]
    type MerkleLogBody BugReport = BugComment
    toLog r = merkleLog root entries
      where
         BugId root = _bugReportId r
         entries = _bugReportDate r
             :+: _bugReportDescription r
             :+: MerkleLogBody (_bugReportComments r)

    fromLog l = BugReport bid bdate bdesc bcomments
      where
        bid = BugId $ _merkleLogRoot l
        (bdate :+: bdesc :+: MerkleLogBody bcomments) = _merkleLogEntries l

instance HasMerkleLog BugReportTag BugReportIdx where
    type MerkleLogHeader BugReportIdx = '[]
    type MerkleLogBody BugReportIdx = BugId
    toLog idx = merkleLog root entries
      where
        root = bugReportIdxFingerPrint idx
        entries = MerkleLogBody $ S.fromList $ Set.toAscList (_bugReportIdx idx)

    fromLog l = BugReportIdx
        (Set.fromList $ toList $ body l)
        (_merkleLogTree l)
```

For types with empty body, one can use the type `Void`.

# Creating Witnesses

We now have everything what we need to compute hashes, Merkle trees, and create
and verify inclusion proofs:

```haskell
-- | @proofCommentInReport bug idx@ creates a witness that the 'BugComment'
-- at position @idx@ is in the 'BugReport' @bug@.
--
proofCommentInReport :: BugReport -> Int -> Either String (MerkleProof SHA512t_256)
proofCommentInReport bug idx = first displayException $ bodyProof @BugReportTag bug idx
```

In order that there is a bug report for a particular date, the proof goes across
the Merkle tree of the `BugReport` of `bid` and the Merkle tree of the
`BugReportIdx` `t`. The final proof doesn't associate the date with `bid`. It
only proofs that there is /some/ `'BugReport` with that date in the
`BugReportIdx`.

To construct the proof the function `merkleProof_` takes as arguments that a
proof subject (in binary representation) and a non-empty list of Merkle trees
along with the position of the leaf which takes part in the proof. The
respective ingredients are obtained from Merkle logs by using the functions
`headerTree` and `bodyTree`. These functions return the Merkle tree and the
positions that are given to the `merkleProof_` function. For both functions that
is a version that is suffixed with an underscore, which also returns the proof
subject.

```haskell
-- | @proofDate t bid@ creates a witness that the 'BugDate' of the 'BugReport'
-- @bid@ is occurs in the bug report index.
--
proofDate
    :: BugReportStore
    -> BugReportIdx
    -> BugId
    -> Either String (MerkleProof SHA512t_256)
proofDate tbl idx bid = do

    -- get tree from bug report
    bug <- maybe (Left "failed to lookup bug") Right
        $ M.lookup bid $ _bugReportStore tbl
    let (subj, pos, t) = headerTree @BugTime @BugReportTag bug
    let bugTree = (pos, t)

    -- get tree from index
    bugIdx <- maybe (Left "failed to lookup bug index") Right
        $ Set.lookupIndex bid $ _bugReportIdx idx
    let idxTree = bodyTree_ @BugReportTag idx bugIdx

    -- create proof
    first displayException
        $ merkleProof_ subj $ bugTree N.:| [idxTree]
```

The proof is verified by checking that the Merkle root that is computed by the
`runMerkleProof` function matches the expectation. The encoded proof subject can
be obtained from the proof with the function `proofSubject`.

```haskell
verifyProof
    :: forall b
    . IsMerkleLogEntry BugReportTag b
    => MerkleProof SHA512t_256
    -> MerkleRoot SHA512t_256
    -> Either String b
verifyProof p expectedRoot
    | expectedRoot /= actualRoot = Left
        $ "Proof failed, roots differ"
        <> ". Expected: " <> show expectedRoot
        <> ", Actual: " <> show actualRoot
    | otherwise = first displayException $ proofSubject @BugReportTag p
  where
    actualRoot = runMerkleProof p
```

# Example

We can use the newMerkleLog function to create a new 'BugReport' from its
entries.

```haskell
newBugReport :: UTCTime -> T.Text -> S.Seq T.Text -> BugReport
newBugReport time desc comments = fromLog mlog
  where
    mlog = newMerkleLog @BugReportTag
        $ BugTime time
        :+: BugDescription desc
        :+: MerkleLogBody (BugComment <$> comments)

newBugReportIdx :: S.Seq BugId -> BugReportIdx
newBugReportIdx bugs = fromLog mlog
  where
    mlog = newMerkleLog @BugReportTag $ MerkleLogBody bugs
```

With that we can create a bug database:

```haskell
bugDb :: (BugReportStore, BugReportIdx)
bugDb = (store, idx)
  where
    t = read @UTCTime "2019-02-05 00:00:00"
    day = 3600 * 24
    bugs = flip map [0..9] $ \(i :: Int) -> newBugReport
        ((fromIntegral i * day) `addUTCTime` t)
        (T.pack $ show i)
        (T.pack . show <$> S.fromList [0..9])
    store = BugReportStore $ foldr (\a -> M.insert (_bugReportId a) a) mempty bugs

    -- the index uses the default sort order for bug reports
    idx = newBugReportIdx (S.fromList $ sort $ _bugReportId <$> bugs)
```

```haskell
testCommentProof :: Either String BugComment
testCommentProof = do
    p <- proofCommentInReport bug 3
    verifyProof @BugComment p bugRoot
  where
    bug = head $ toList $ _bugReportStore store
    BugId bugRoot = _bugReportId bug
    (store, _) = bugDb

testDateProof :: Either String BugTime
testDateProof = do
    p <- proofDate store idx bid
    verifyProof @BugTime p (bugReportIdxFingerPrint idx)
  where
    (store, idx) = bugDb
    bid = _bugReportId $ head $ toList $ _bugReportStore store
```
