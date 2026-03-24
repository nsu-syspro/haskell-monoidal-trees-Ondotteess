{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3.Tree where

import Common.MonoidalTree

import Data.Foldable (toList)

import Task1 (Measured(..))

-- * 2-3 tree definition

-- | 2-3 tree with values 'a' in leaves
-- Intermediate nodes contain only accumulated measure 'm'
data Tree m a
  = Empty
  | Leaf a
  | Node2 m (Tree m a) (Tree m a)
  | Node3 m (Tree m a) (Tree m a) (Tree m a)
  deriving (Show, Eq)

-- | Measures given tree using provided measure of 'a'
instance Measured m a => Measured m (Tree m a) where
  measure Empty            = mempty
  measure (Leaf x)         = measure x
  measure (Node2 m _ _)    = m
  measure (Node3 m _ _ _)  = m

instance Foldable (Tree m) where
  foldMap _ Empty          = mempty
  foldMap f (Leaf x)       = f x
  foldMap f (Node2 _ l r)  = foldMap f l <> foldMap f r
  foldMap f (Node3 _ l m r)= foldMap f l <> foldMap f m <> foldMap f r

-- * Smart constructors

leaf :: a -> Tree m a
leaf = Leaf

node2 :: Measured m a => Tree m a -> Tree m a -> Tree m a
node2 l r = Node2 (measure l <> measure r) l r

node3 :: Measured m a => Tree m a -> Tree m a -> Tree m a -> Tree m a
node3 l m r = Node3 (measure l <> measure m <> measure r) l m r
-- * Helper result types

data InsertResult m a
  = Inserted (Tree m a)
  | Overflow (Tree m a) (Tree m a)

data RemoveResult m a
  = Removed (Tree m a)
  | Short (Tree m a)

-- * Left/right insertion used by MonoidalTree instance

insertLeftmost :: Measured m a => Tree m a -> Tree m a -> InsertResult m a
insertLeftmost x Empty = Inserted x
insertLeftmost x (Leaf y) = Overflow x (Leaf y)
insertLeftmost x (Node2 _ l r) =
  case insertLeftmost x l of
    Inserted l'      -> Inserted (node2 l' r)
    Overflow a b     -> Inserted (node3 a b r)
insertLeftmost x (Node3 _ l m r) =
  case insertLeftmost x l of
    Inserted l'      -> Inserted (node3 l' m r)
    Overflow a b     -> Overflow (node2 a b) (node2 m r)

insertRightmost :: Measured m a => Tree m a -> Tree m a -> InsertResult m a
insertRightmost Empty x = Inserted x
insertRightmost (Leaf y) x = Overflow (Leaf y) x
insertRightmost (Node2 _ l r) x =
  case insertRightmost r x of
    Inserted r'      -> Inserted (node2 l r')
    Overflow a b     -> Inserted (node3 l a b)
insertRightmost (Node3 _ l m r) x =
  case insertRightmost r x of
    Inserted r'      -> Inserted (node3 l m r')
    Overflow a b     -> Overflow (node2 l m) (node2 a b)

normalizeInsert :: Measured m a => InsertResult m a -> Tree m a
normalizeInsert (Inserted t)  = t
normalizeInsert (Overflow l r)= node2 l r

-- * Generic deletion repair helpers

fixNode2Left :: Measured m a => Tree m a -> Tree m a -> RemoveResult m a
fixNode2Left l' r =
  case r of
    Empty          -> Short l'
    Leaf _         -> Short r
    Node2 _ a b    -> Short (node3 l' a b)
    Node3 _ a b c  -> Removed (node2 (node2 l' a) (node2 b c))

fixNode2Right :: Measured m a => Tree m a -> Tree m a -> RemoveResult m a
fixNode2Right l r' =
  case l of
    Empty          -> Short r'
    Leaf _         -> Short l
    Node2 _ a b    -> Short (node3 a b r')
    Node3 _ a b c  -> Removed (node2 (node2 a b) (node2 c r'))

fixNode3Left :: Measured m a => Tree m a -> Tree m a -> Tree m a -> RemoveResult m a
fixNode3Left l' m r =
  case m of
    Empty          -> Removed (node2 l' r)
    Leaf _         -> Removed (node2 m r)
    Node2 _ a b    -> Removed (node2 (node3 l' a b) r)
    Node3 _ a b c  -> Removed (node3 (node2 l' a) (node2 b c) r)

fixNode3Middle :: Measured m a => Tree m a -> Tree m a -> Tree m a -> RemoveResult m a
fixNode3Middle l m' r =
  case l of
    Empty          -> Removed (node2 m' r)
    Leaf _         -> Removed (node2 l r)
    Node2 _ a b    -> Removed (node2 (node3 a b m') r)
    Node3 _ a b c  -> Removed (node3 (node2 a b) (node2 c m') r)

fixNode3Right :: Measured m a => Tree m a -> Tree m a -> Tree m a -> RemoveResult m a
fixNode3Right l m r' =
  case m of
    Empty          -> Removed (node2 l r')
    Leaf _         -> Removed (node2 l m)
    Node2 _ a b    -> Removed (node2 l (node3 a b r'))
    Node3 _ a b c  -> Removed (node3 l (node2 a b) (node2 c r'))

normalizeRemove :: RemoveResult m a -> Tree m a
normalizeRemove (Removed t) = t
normalizeRemove (Short t)   = t

-- * Monoidal tree instance

instance MonoidalTree Tree where
  toTree = buildBalanced . map leaf . toList

  x <| t = normalizeInsert (insertLeftmost (leaf x) t)

  t |> x = normalizeInsert (insertRightmost t (leaf x))

-- * Balanced construction

buildBalanced :: Measured m a => [Tree m a] -> Tree m a
buildBalanced []   = Empty
buildBalanced [t]  = t
buildBalanced ts   = buildBalanced (groupLevel ts)

groupLevel :: Measured m a => [Tree m a] -> [Tree m a]
groupLevel ts = consume (groupSizes (length ts)) ts
  where
    consume :: Measured m a => [Int] -> [Tree m a] -> [Tree m a]
    consume [] [] = []
    consume (2:gs) (a:b:rest)   = node2 a b : consume gs rest
    consume (3:gs) (a:b:c:rest) = node3 a b c : consume gs rest
    consume _ _ = error "groupLevel: impossible grouping"

groupSizes :: Int -> [Int]
groupSizes 0 = []
groupSizes 1 = error "groupSizes: impossible size 1"
groupSizes n =
  case n `mod` 3 of
    0 -> replicate (n `div` 3) 3
    1 -> replicate (n `div` 3 - 1) 3 ++ [2, 2]
    _ -> replicate (n `div` 3) 3 ++ [2]