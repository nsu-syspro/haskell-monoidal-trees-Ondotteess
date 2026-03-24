{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3.PQueue where

import Common.MonoidalTree
import Common.PriorityQueue

import Data.Foldable (toList)

import Task1 (Measured(..), Min(..), Max(..), MinMax(..))
import Task3.Tree

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
    (v, res) <- extractMinTree t
    pure (v, PQueue (normalizeRemove res))

  extractMax (PQueue t) = do
    (v, res) <- extractMaxTree t
    pure (v, PQueue (normalizeRemove res))

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

extractMinTree
  :: Ord k
  => Tree (MinMax k) (Entry k v)
  -> Maybe (v, RemoveResult (MinMax k) (Entry k v))
extractMinTree Empty = Nothing
extractMinTree (Leaf (Entry (_, v))) = Just (v, Short Empty)
extractMinTree (Node2 _ l r)
  | chooseLeftMin l r =
      do
        (v, res) <- extractMinTree l
        pure (v, case res of
          Removed l' -> Removed (node2 l' r)
          Short l'   -> fixNode2Left l' r)
  | otherwise =
      do
        (v, res) <- extractMinTree r
        pure (v, case res of
          Removed r' -> Removed (node2 l r')
          Short r'   -> fixNode2Right l r')
extractMinTree (Node3 _ l m r)
  | chooseLeftMin3 l m r =
      do
        (v, res) <- extractMinTree l
        pure (v, case res of
          Removed l' -> Removed (node3 l' m r)
          Short l'   -> fixNode3Left l' m r)
  | chooseMiddleMin3 l m r =
      do
        (v, res) <- extractMinTree m
        pure (v, case res of
          Removed m' -> Removed (node3 l m' r)
          Short m'   -> fixNode3Middle l m' r)
  | otherwise =
      do
        (v, res) <- extractMinTree r
        pure (v, case res of
          Removed r' -> Removed (node3 l m r')
          Short r'   -> fixNode3Right l m r')

extractMaxTree
  :: Ord k
  => Tree (MinMax k) (Entry k v)
  -> Maybe (v, RemoveResult (MinMax k) (Entry k v))
extractMaxTree Empty = Nothing
extractMaxTree (Leaf (Entry (_, v))) = Just (v, Short Empty)
extractMaxTree (Node2 _ l r)
  | chooseLeftMax l r =
      do
        (v, res) <- extractMaxTree l
        pure (v, case res of
          Removed l' -> Removed (node2 l' r)
          Short l'   -> fixNode2Left l' r)
  | otherwise =
      do
        (v, res) <- extractMaxTree r
        pure (v, case res of
          Removed r' -> Removed (node2 l r')
          Short r'   -> fixNode2Right l r')
extractMaxTree (Node3 _ l m r)
  | chooseLeftMax3 l m r =
      do
        (v, res) <- extractMaxTree l
        pure (v, case res of
          Removed l' -> Removed (node3 l' m r)
          Short l'   -> fixNode3Left l' m r)
  | chooseMiddleMax3 l m r =
      do
        (v, res) <- extractMaxTree m
        pure (v, case res of
          Removed m' -> Removed (node3 l m' r)
          Short m'   -> fixNode3Middle l m' r)
  | otherwise =
      do
        (v, res) <- extractMaxTree r
        pure (v, case res of
          Removed r' -> Removed (node3 l m r')
          Short r'   -> fixNode3Right l m r')

chooseLeftMin :: Ord k => Tree (MinMax k) (Entry k v) -> Tree (MinMax k) (Entry k v) -> Bool
chooseLeftMin l r =
  case (minKey l, minKey r) of
    (Just kl, Just kr) -> kl <= kr
    (Just _, Nothing)  -> True
    _                  -> False

chooseLeftMax :: Ord k => Tree (MinMax k) (Entry k v) -> Tree (MinMax k) (Entry k v) -> Bool
chooseLeftMax l r =
  case (maxKey l, maxKey r) of
    (Just kl, Just kr) -> kl >= kr
    (Just _, Nothing)  -> True
    _                  -> False

chooseLeftMin3
  :: Ord k
  => Tree (MinMax k) (Entry k v)
  -> Tree (MinMax k) (Entry k v)
  -> Tree (MinMax k) (Entry k v)
  -> Bool
chooseLeftMin3 l m r =
  case (minKey l, minKey m, minKey r) of
    (Just kl, Just km, Just kr) -> kl <= km && kl <= kr
    (Just _, Nothing, Nothing)  -> True
    (Just kl, Just km, Nothing) -> kl <= km
    (Just kl, Nothing, Just kr) -> kl <= kr
    _                           -> False

chooseMiddleMin3
  :: Ord k
  => Tree (MinMax k) (Entry k v)
  -> Tree (MinMax k) (Entry k v)
  -> Tree (MinMax k) (Entry k v)
  -> Bool
chooseMiddleMin3 l m r =
  case (minKey l, minKey m, minKey r) of
    (_, Just km, Just kr)       -> maybe True (> km) (minKey l) && km <= kr
    (Nothing, Just _, Nothing)  -> True
    _                           -> False

chooseLeftMax3
  :: Ord k
  => Tree (MinMax k) (Entry k v)
  -> Tree (MinMax k) (Entry k v)
  -> Tree (MinMax k) (Entry k v)
  -> Bool
chooseLeftMax3 l m r =
  case (maxKey l, maxKey m, maxKey r) of
    (Just kl, Just km, Just kr) -> kl >= km && kl >= kr
    (Just _, Nothing, Nothing)  -> True
    (Just kl, Just km, Nothing) -> kl >= km
    (Just kl, Nothing, Just kr) -> kl >= kr
    _                           -> False

chooseMiddleMax3
  :: Ord k
  => Tree (MinMax k) (Entry k v)
  -> Tree (MinMax k) (Entry k v)
  -> Tree (MinMax k) (Entry k v)
  -> Bool
chooseMiddleMax3 l m r =
  case (maxKey l, maxKey m, maxKey r) of
    (_, Just km, Just kr)       -> maybe True (< km) (maxKey l) && km >= kr
    (Nothing, Just _, Nothing)  -> True
    _                           -> False