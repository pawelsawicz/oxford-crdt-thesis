module Core.Semilattice

import Data.List
import Data.Vect
import Data.SortedSet

import Core.Log.LogEvent

public export
interface JoinSemilattice ty where
  lub : ty -> ty -> ty

|||         12
|||        / \
|||       4   6
|||      / \ / \
|||     2   3
|||      \ /
|||       1
public export
JoinSemilattice Nat where
  lub Z Z = Z
  lub Z (S k) = S k
  lub (S k) Z = S k
  lub (S j) (S k) = S (lub j k)

|||   True
|||   /  \
||| False
JoinSemilattice Bool where
  lub True _ = True
  lub _ True = True
  lub _ _ = False

public export
JoinSemilattice a => JoinSemilattice (Vect m a) where
  lub [] [] = []
  lub (x::xs) (y::ys) = (lub x y) :: lub xs ys

public export
JoinSemilattice (SortedSet a) where
  lub xs ys = union xs ys

public export
JoinSemilattice a => JoinSemilattice b => JoinSemilattice (a, b) where
  lub (x1, y1) (x2, y2) = ((lub x1 x2), (lub y1 y2))