{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Data.Carousel
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Jon Sterling <jsterling@alephcloud.com>
-- Stability: experimental
--
module Data.Carousel
( Carousel
, empty
, cursor
, moveLeft
, moveRight
, dropCursor
, nub
, append
, clSequence
, clList
) where

import Control.Lens
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Sequence as S

newtype Carousel α
  = Carousel
  { _clSequence ∷ S.Seq α
  } deriving (Show, Eq)

instance Functor Carousel where
  fmap f (Carousel sq) = Carousel $ fmap f sq

instance F.Foldable Carousel where
  foldMap f (Carousel sq) = F.foldMap f sq

instance Traversable Carousel where
  traverse f (Carousel sq) = fmap Carousel (traverse f sq)

clSequence ∷ Iso (Carousel α) (Carousel β) (S.Seq α) (S.Seq β)
clSequence = iso _clSequence Carousel

clList ∷ Iso (Carousel α) (Carousel β) [α] [β]
clList = clSequence . iso F.toList S.fromList


-- | There is guaranteed to be a cursor so long as the carousel is non-empty.
cursor ∷ Traversal' (Carousel α) α
cursor = clSequence . _head

-- | You can delete the cursor from the carousel; the items to its right will
-- move one over to the left.
dropCursor
  ∷ Carousel α
  → Carousel α
dropCursor = clSequence %~ (^. _tail)

-- | The carousel may be rotated to the right; this operation is written so as
-- to always guarantee the presence of a cursor in a non-empty carousel.
moveRight
  ∷ Carousel α
  → Carousel α
moveRight = clSequence %~ rotate . S.viewl
  where
    rotate
      ∷ S.ViewL α
      → S.Seq α
    rotate S.EmptyL = S.empty
    rotate (x S.:< xs) = xs S.|> x

-- | The carousel may be rotated to the left; this operation is written so as
-- to always guarantee the presence of a cursor in a non-empty carousel.
moveLeft
  ∷ Carousel α
  → Carousel α
moveLeft = clSequence %~ rotate . S.viewr
  where
    rotate
      ∷ S.ViewR α
      → S.Seq α
    rotate S.EmptyR = S.empty
    rotate (xs S.:> x) = x S.<| xs

-- | The empty carousel.
empty
  ∷ Carousel α
empty = Carousel S.empty

-- | Append a list of elements to the right side of the carousel.
append
  ∷ [α]
  → Carousel α
  → Carousel α
append xs = clSequence %~ (S.>< S.fromList xs)

-- | Remove all duplicate elements from the carousel. (TODO: this is inefficient)
nub
  ∷ Eq α
  ⇒ Carousel α
  → Carousel α
nub = clList %~ L.nub
