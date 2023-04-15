module Semilattice

import Data.List
import Data.Vect

interface Ord ty => JoinSemilattice ty where
  lub : ty -> ty -> ty

JoinSemilattice Nat where
  lub Z Z = Z
  lub Z (S k) = S k
  lub (S k) Z = S k
  lub (S j) (S k) = S (lub j k)

JoinSemilattice Bool where
  lub True _ = True
  lub _ True = True
  lub _ _ = False

JoinSemilattice a => JoinSemilattice (Vect m a) where
  lub [] [] = []
  lub (x::xs) (y::ys) = (lub x y) :: lub xs ys

JoinSemilattice a => JoinSemilattice (List a) where
  lub [] [] = []
  lub xs ys = union xs ys

JoinSemilattice a => JoinSemilattice b => JoinSemilattice (a, b) where
  lub (x1, y1) (x2, y2) = ((lub x1 x2), (lub y1 y2))