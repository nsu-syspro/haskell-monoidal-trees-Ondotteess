{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2.PQueue where

import Common.MonoidalTree
import Common.PriorityQueue

import Data.Foldable (toList)

import Task1 (Measured(..), Min(..), Max(..), MinMax(..))
import Task2.Tree

-- * Priority queue definition

-- | Priority queue based on binary tree
newtype PQueue k v = PQueue { getTree :: Tree (MinMax k) (Entry k v) }
  deriving (Show, Eq)

-- | Priority queue entry wrapper
newtype Entry k v = Entry { getEntry :: (k, v) }
  deriving (Show, Eq)

instance Ord k => Measured (MinMax k) (Entry k v) where
  measure (Entry (k, _)) = measure k

-- * Priority queue instance

instance PriorityQueue PQueue where
  empty = PQueue Empty

  toPriorityQueue = PQueue . toTree . map Entry . toList

  entries (PQueue t) = foldMap (\(Entry kv) -> [kv]) t

  insert k v (PQueue t) = PQueue (t |> Entry (k, v))

  extractMin (PQueue t) = do
    (v, t') <- extractMinTree t
    pure (v, PQueue t')

  extractMax (PQueue t) = do
    (v, t') <- extractMaxTree t
    pure (v, PQueue t')

minKey :: Ord k => Tree (MinMax k) (Entry k v) -> Maybe k
minKey t =
  case measure t of
    MinMax (PosInf, _) -> Nothing
    MinMax (Min k, _)  -> Just k

maxKey :: Ord k => Tree (MinMax k) (Entry k v) -> Maybe k
maxKey t =
  case measure t of
    MinMax (_, NegInf) -> Nothing
    MinMax (_, Max k)  -> Just k

extractMinTree :: Ord k => Tree (MinMax k) (Entry k v) -> Maybe (v, Tree (MinMax k) (Entry k v))
extractMinTree Empty = Nothing
extractMinTree (Leaf (Entry (_, v))) = Just (v, Empty)
extractMinTree (Branch _ l r) =
  case (minKey l, minKey r) of
    (Nothing, Nothing) -> Nothing
    (Just _, Nothing)  -> do
      (v, l') <- extractMinTree l
      pure (v, branch l' r)
    (Nothing, Just _)  -> do
      (v, r') <- extractMinTree r
      pure (v, branch l r')
    (Just kl, Just kr)
      | kl <= kr  -> do
          (v, l') <- extractMinTree l
          pure (v, branch l' r)
      | otherwise -> do
          (v, r') <- extractMinTree r
          pure (v, branch l r')

extractMaxTree :: Ord k => Tree (MinMax k) (Entry k v) -> Maybe (v, Tree (MinMax k) (Entry k v))
extractMaxTree Empty = Nothing
extractMaxTree (Leaf (Entry (_, v))) = Just (v, Empty)
extractMaxTree (Branch _ l r) =
  case (maxKey l, maxKey r) of
    (Nothing, Nothing) -> Nothing
    (Just _, Nothing)  -> do
      (v, l') <- extractMaxTree l
      pure (v, branch l' r)
    (Nothing, Just _)  -> do
      (v, r') <- extractMaxTree r
      pure (v, branch l r')
    (Just kl, Just kr)
      | kl >= kr  -> do
          (v, l') <- extractMaxTree l
          pure (v, branch l' r)
      | otherwise -> do
          (v, r') <- extractMaxTree r
          pure (v, branch l r')