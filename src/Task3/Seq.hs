{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3.Seq where

import Common.MonoidalTree
import Common.Sequence

import Data.Foldable (toList)

import Task1 (Measured(..), Size(..))
import Task3.Tree

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

  insertAt i x (Seq t) = Seq (normalizeInsert (insertAtTree j (Elem x) t))
    where
      j = max 0 (min i (treeSize t))

  removeAt i (Seq t)
    | i < 0 || i >= treeSize t = Seq t
    | otherwise                = Seq (normalizeRemove (removeAtTree i t))

  elemAt i (Seq t)
    | i < 0 || i >= treeSize t = Nothing
    | otherwise                = getElem <$> elemAtTree i t

treeSize :: forall a. Tree (Size a) (Elem a) -> Int
treeSize t = getSize (measure t :: Size a)

insertAtTree :: Int -> Elem a -> Tree (Size a) (Elem a) -> InsertResult (Size a) (Elem a)
insertAtTree _ x Empty = Inserted (leaf x)
insertAtTree i x (Leaf y)
  | i <= 0    = Overflow (leaf x) (leaf y)
  | otherwise = Overflow (leaf y) (leaf x)
insertAtTree i x (Node2 _ l r)
  | i <= sl =
      case insertAtTree i x l of
        Inserted l'    -> Inserted (node2 l' r)
        Overflow a b   -> Inserted (node3 a b r)
  | otherwise =
      case insertAtTree (i - sl) x r of
        Inserted r'    -> Inserted (node2 l r')
        Overflow a b   -> Inserted (node3 l a b)
  where
    sl = treeSize l
insertAtTree i x (Node3 _ l m r)
  | i <= sl =
      case insertAtTree i x l of
        Inserted l'    -> Inserted (node3 l' m r)
        Overflow a b   -> Overflow (node2 a b) (node2 m r)
  | i <= sl + sm =
      case insertAtTree (i - sl) x m of
        Inserted m'    -> Inserted (node3 l m' r)
        Overflow a b   -> Overflow (node2 l a) (node2 b r)
  | otherwise =
      case insertAtTree (i - sl - sm) x r of
        Inserted r'    -> Inserted (node3 l m r')
        Overflow a b   -> Overflow (node2 l m) (node2 a b)
  where
    sl = treeSize l
    sm = treeSize m

removeAtTree :: Int -> Tree (Size a) (Elem a) -> RemoveResult (Size a) (Elem a)
removeAtTree _ Empty = Removed Empty
removeAtTree _ (Leaf _) = Short Empty
removeAtTree i (Node2 _ l r)
  | i < sl =
      case removeAtTree i l of
        Removed l' -> Removed (node2 l' r)
        Short l'   -> fixNode2Left l' r
  | otherwise =
      case removeAtTree (i - sl) r of
        Removed r' -> Removed (node2 l r')
        Short r'   -> fixNode2Right l r'
  where
    sl = treeSize l
removeAtTree i (Node3 _ l m r)
  | i < sl =
      case removeAtTree i l of
        Removed l' -> Removed (node3 l' m r)
        Short l'   -> fixNode3Left l' m r
  | i < sl + sm =
      case removeAtTree (i - sl) m of
        Removed m' -> Removed (node3 l m' r)
        Short m'   -> fixNode3Middle l m' r
  | otherwise =
      case removeAtTree (i - sl - sm) r of
        Removed r' -> Removed (node3 l m r')
        Short r'   -> fixNode3Right l m r'
  where
    sl = treeSize l
    sm = treeSize m

elemAtTree :: Int -> Tree (Size a) (Elem a) -> Maybe (Elem a)
elemAtTree _ Empty = Nothing
elemAtTree i (Leaf x)
  | i == 0    = Just x
  | otherwise = Nothing
elemAtTree i (Node2 _ l r)
  | i < sl    = elemAtTree i l
  | otherwise = elemAtTree (i - sl) r
  where
    sl = treeSize l
elemAtTree i (Node3 _ l m r)
  | i < sl      = elemAtTree i l
  | i < sl + sm = elemAtTree (i - sl) m
  | otherwise   = elemAtTree (i - sl - sm) r
  where
    sl = treeSize l
    sm = treeSize m