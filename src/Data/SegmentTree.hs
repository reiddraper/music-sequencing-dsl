{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Data.SegmentTree
-- Copyright   :  (c) Dmitry Astapov 2010
-- License     :  BSD-style
-- Maintainer  :  dastapov@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs, etc - see above)
--
-- Segment Tree implemented following section 10.3 and 10.4 of
--
--    * Mark de Berg, Otfried Cheong, Marc van Kreveld, Mark Overmars
--      "Computational Geometry, Algorithms and Applications", Third Edition
--      (2008) pp 231-237
--      \"Finger trees: a simple general-purpose data structure\",
--      /Journal of Functional Programming/ 16:2 (2006) pp 197-217.
--      <http://www.soi.city.ac.uk/~ross/papers/FingerTree.html>
--
-- Accumulation of results with monoids following "Monoids and Finger Trees",
-- http://apfelmus.nfshost.com/articles/monoid-fingertree.html
--
-- An amortized running time is given for each operation, with /n/
-- referring to the number of intervals.
module Data.SegmentTree (STree (..), fromList, queryTree) where

import Data.ExtendedReal (Extended (NegInf, PosInf))
import Data.Interval (Interval, hull, isConnected, isSubsetOf, lowerBound, member, upperBound, (<..<), (<=..<=))
import Data.List (foldl', sort, unfoldr)
import Text.Printf

-- | Segment Tree is a binary tree that stores Interval in each leaf or branch.
-- By construction (see `leaf' and `branch') intervals in branches should be union
-- of the intervals from left and right subtrees.
--
-- Additionally, each node carries a "tag" of type "t" (which should be monoid).
-- By supplying different monoids, segment tree could be made to support different types
-- of stabbing queries: Sum or Integer monoid will give tree that counts hits, and list or
-- Set monoids will give a tree that returns actual intervals containing point.
data STree t a
  = Leaf !(Maybe t) !(Interval a)
  | Branch !(Maybe t) !(Interval a) !(STree t a) !(STree t a)

instance (Show t, Show a, Ord a) => Show (STree t a) where
  show (Leaf t i) = printf "Leaf %s %s" (show t) (show i)
  show (Branch t i left right) = printf "Branch %s %s (\n  %s\n  %s)" (show t) (show i) (show left) (show right)

-- leaves :: (Ord a) => STree t a -> [STree t a]
-- leaves leaf@(Leaf _ _) = [leaf]
-- leaves (Branch _ _ left right) = leaves left ++ leaves right

-- leafIntervals :: (Ord a) => STree t a -> [Interval a]
-- leafIntervals (Leaf _ i) = [i]
-- leafIntervals (Branch _ _ left right) = leafIntervals left ++ leafIntervals right

-- Selectors for STree
tag :: STree t a -> Maybe t
tag (Leaf val _) = val
tag (Branch val _ _ _) = val

interval (Leaf _ i) = i
interval (Branch _ i _ _) = i

-- Constructors for STree nodes
branch :: (Ord a, Monoid t) => STree t a -> STree t a -> STree t a
branch x y = Branch (tag x `mappend` tag y) (hull (interval x) (interval y)) x y

fromList :: (Ord a, Monoid t) => [(Interval a, t)] -> STree t a
fromList intervalAndData = foldl' (\tree (i, t) -> insert tree i t) skeleton intervalAndData
  where
    -- "skeleton" tree is a binary tree where each leaf holds some atomic interval (and empty tag)
    -- and each branch holds union of intervals from its leaves (and empty tag).
    -- Tree is built from bottom up, by making "leaves" first and then connecting them with branches
    -- pairwise, until a single root is obtained.
    ([skeleton] : _) = dropWhile (not . converged) $ iterate (unfoldr connect) leaves
    leaves = map (Leaf Nothing) atomics
    connect [] = Nothing
    connect [x, y, z] = Just ((x `branch` y) `branch` z, [])
    connect (x : y : rest) = Just (x `branch` y, rest)
    converged [_x] = True
    converged _ = False

    -- Open "atomic" intervals are formed between the (sorted) endpoints of original intervals.
    -- Leftmost atomic interval starts from minu infinity, rightmost ends with infinity.
    -- All endpoints are also converted to closed single-point atomic intervals.
    -- For details, see book referenced above or wikipedia.
    atomics = concat (zipWith atomicInterval endpoints (drop 1 endpoints))
    atomicInterval a PosInf = [a <=..<= PosInf]
    atomicInterval a b = [a <..< b, b <=..<= b]
    endpoints = sort $ foldl' (\acc i -> lowerBound i : upperBound i : acc) [NegInf, PosInf] (map fst intervalAndData)

-- | Insert interval `i' into segment tree, updating tag values as necessary.
-- Semantics of tags depends on the monoid used (see `fromList')
insert :: (Ord a, Semigroup t) => STree t a -> Interval a -> t -> STree t a
insert leaf@(Leaf leafT iu) i t
  | iu `isSubsetOf` i = Leaf (leafT <> Just t) iu
  | otherwise = leaf
insert (Branch branchT iu left right) i t
  | iu `isSubsetOf` i = Branch (branchT <> Just t) iu left right
  | otherwise =
    let left' = if i `isConnected` interval left then insert left i t else left
        right' = if i `isConnected` interval right then insert right i t else right
     in Branch branchT iu left' right'

-- | Query the segment tree for the specified point. Time: O(log n)
queryTree :: (Ord a, Semigroup t) => STree t a -> a -> Maybe t
queryTree tree point = go tree point
  where
    go (Leaf t ivl) point
      | point `member` ivl = t
      | otherwise = Nothing
    go (Branch t _ivl left right) point = t <> qleft <> qright
      where
        qleft = if point `member` interval left then go left point else Nothing
        qright = if point `member` interval right then go right point else Nothing
