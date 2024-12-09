{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DeriveTraversable #-}
{-# language DerivingStrategies #-}
{-# language InstanceSigs #-}
{-# language LambdaCase #-}
{-# language TupleSections #-}

module Chainweb.Utils.Rule
  ( Rule(..)
  , ruleHead
  , ruleDropWhile
  , ruleTakeWhile
  , ruleValid
  , ruleElems
  , RuleZipper(..)
  , ruleZipperHere
  , unzipRule
  , ruleZipperFind
  , ruleSeek
  , ruleZipperDown

  ) where

import Control.DeepSeq

import Data.Aeson
import Data.Bifunctor
import Data.Hashable
import qualified Data.List.NonEmpty as NE
import Data.Functor.Apply
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import qualified Data.Vector as V

import GHC.Generics

-- | `a` values graded by `h`, starting with the highest `h` value and lowering
-- as you go deeper, bottoming out with no `h` value at all. Used to efficiently
-- represent behaviors that change as the block height increases.
--
-- Is is optimized for lookups of items at the top of stack. On the blockchain
-- we often lookup chain properties (e.g. forks) where we are interested in the
-- latest occurrence.
--
data Rule h a = Above (h, a) (Rule h a) | Bottom (h, a)
    deriving stock (Eq, Ord, Show, Foldable, Functor, Generic, Generic1, Traversable)
    deriving anyclass (Hashable, NFData)

instance Bifunctor Rule where
  bimap :: (h -> h') -> (a -> a') -> Rule h a -> Rule h' a'
  bimap fh fa = go
    where
      go = \case
        Above (h, a) r -> Above (fh h, fa a) (go r)
        Bottom (h, a) -> Bottom (fh h, fa a)

instance Foldable1 (Rule h) where foldMap1 = foldMap1Default
instance Traversable1 (Rule h) where
    traverse1 f (Above (h, a) t) = Above . (h,) <$> f a <.> traverse1 f t
    traverse1 f (Bottom (h, a)) = Bottom . (h,) <$> f a

instance (ToJSON h, ToJSON a) => ToJSON (Rule h a) where
    toJSON = toJSON . ruleElems

instance (FromJSON h, FromJSON a) => FromJSON (Rule h a) where
    parseJSON = withArray "Rule" $ go . V.toList
      where
        go [] = fail "empty list"
        go [a] = Bottom <$> parseJSON a
        go (x:xs) = Above <$> parseJSON x <*> go xs

ruleHead :: Rule h a -> (h, a)
ruleHead (Above (h, a) _) = (h, a)
ruleHead (Bottom (h, a)) = (h, a)

ruleTakeWhile :: (h -> Bool) -> Rule h a -> Rule h a
ruleTakeWhile p (Above (h, a) t)
    | p h = Above (h, a) (ruleTakeWhile p t)
    | otherwise = ruleTakeWhile p t
ruleTakeWhile _ t = t

ruleDropWhile :: (h -> Bool) -> Rule h a -> Rule h a
ruleDropWhile p (Above (h, a) t)
    | p h = ruleDropWhile p t
    | otherwise = Above (h, a) t
ruleDropWhile _ t = t

-- | A zipper on a rule represents a measurement on the rule, either at some
-- point on the rule (including the top) or at the bottom of the rule.
-- Leftmost fields are "below", rightmost fields are "above".
data RuleZipper h a
  = BetweenZipper (Rule h a) [(h, a)]
  deriving Show

ruleZipperHere :: RuleZipper h a -> (h, a)
ruleZipperHere (BetweenZipper r _) = ruleHead r

-- | Construct a zipper at the top of the Rule, O(1).
unzipRule :: Rule h a -> RuleZipper h a
unzipRule r = BetweenZipper r []
{-# inline unzipRule #-}

ruleZipperDown :: RuleZipper h a -> RuleZipper h a
ruleZipperDown = \case
  BetweenZipper (Bottom t) above -> BetweenZipper (Bottom t) above
  BetweenZipper (justBelow `Above` below) above ->
    BetweenZipper below (justBelow : above)

-- | Find the place in the rule zipper that satisfies the condition.
-- Note that if the condition is never reached, the rule is returned reversed.
-- O(length(untrue prefix)).
ruleZipperFind :: (h -> a -> Bool) -> RuleZipper h a -> (Bool, RuleZipper h a)
ruleZipperFind p = go
  where
  go pl@(BetweenZipper below above)
    | (h, a) <- ruleHead below, p h a =
      (True, pl)
    | Bottom {} <- below =
      (False, pl)
    | justBelow `Above` wayBelow <- below =
      go (BetweenZipper wayBelow (justBelow : above))
{-# inline ruleZipperFind #-}

-- | Find the place in the rule that satisfies the condition, and
-- return it as a zipper.
-- Note that if it reaches the bottom, the bottom is returned.
-- O(length(untrue prefix)).
ruleSeek :: (h -> a -> Bool) -> Rule h a -> (Bool, RuleZipper h a)
ruleSeek p = ruleZipperFind p . unzipRule
{-# inline ruleSeek #-}

-- | Returns the elements of the Rule. O(n) and lazily produces elements.
--
ruleElems :: Rule h a -> NE.NonEmpty (h, a)
ruleElems (Bottom (h, a)) = (h, a) NE.:| []
ruleElems (Above (h, a) t) = (h, a) `NE.cons` ruleElems t

-- | Checks that a Rule is decreasing, and thus valid.
-- O(n).
ruleValid :: Ord h => Rule h a -> Bool
ruleValid (Above (h, _) t@(Above (h', _) _)) = h > h' && ruleValid t
ruleValid _ = True
