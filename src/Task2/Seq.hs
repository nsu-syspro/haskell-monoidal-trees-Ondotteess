{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2.Seq where

import Common.MonoidalTree
import Common.Sequence

import Data.Foldable (toList)

import Task1 (Measured(..), Size(..))
import Task2.Tree

-- * Sequence definition

-- | Random-access sequence based on binary tree
newtype Seq a = Seq { getTree :: Tree (Size a) (Elem a) }
  deriving (Show, Eq)

-- | Sequence element wrapper
newtype Elem a = Elem { getElem :: a }
  deriving (Show, Eq)

-- | Measures given element as 'Size 1'
instance Measured (Size a) (Elem a) where
  measure _ = Size 1

instance Foldable Seq where
  foldMap f (Seq t) = foldMap (f . getElem) t

  -- An O(1) implementation of length is possible
  -- due to size of the tree being cached at each node
  length :: forall a. Seq a -> Int
  length (Seq t) = treeSize t

-- * Sequence instance

instance Sequence Seq where
  empty = Seq Empty

  toSequence = Seq . toTree . map Elem . toList

  x +| Seq t = Seq (Elem x <| t)

  Seq t |+ x = Seq (t |> Elem x)

  insertAt i x (Seq t) = Seq (insertTree i (Elem x) t)

  removeAt i (Seq t)
    | i < 0 || i >= treeSize t = Seq t
    | otherwise = Seq (removeTree i t)

  elemAt i (Seq t)
    | i < 0 || i >= treeSize t = Nothing
    | otherwise = getElem <$> elemAtTree i t

treeSize :: forall a. Tree (Size a) (Elem a) -> Int
treeSize t = getSize (measure t :: Size a)

insertTree :: Int -> Elem a -> Tree (Size a) (Elem a) -> Tree (Size a) (Elem a)
insertTree _ x Empty = leaf x
insertTree i x (Leaf y)
  | i <= 0 = branch (leaf x) (leaf y)
  | otherwise = branch (leaf y) (leaf x)
insertTree i x (Branch _ l r)
  | i <= leftSize = branch (insertTree i x l) r
  | otherwise = branch l (insertTree (i - leftSize) x r)
  where
    leftSize = treeSize l

removeTree :: Int -> Tree (Size a) (Elem a) -> Tree (Size a) (Elem a)
removeTree _ Empty = Empty
removeTree _ (Leaf _) = Empty
removeTree i (Branch _ l r)
  | i < leftSize = branch (removeTree i l) r
  | otherwise = branch l (removeTree (i - leftSize) r)
  where
    leftSize = treeSize l

elemAtTree :: Int -> Tree (Size a) (Elem a) -> Maybe (Elem a)
elemAtTree _ Empty = Nothing
elemAtTree i (Leaf x)
  | i == 0 = Just x
  | otherwise = Nothing
elemAtTree i (Branch _ l r)
  | i < leftSize = elemAtTree i l
  | otherwise = elemAtTree (i - leftSize) r
  where
    leftSize = treeSize l