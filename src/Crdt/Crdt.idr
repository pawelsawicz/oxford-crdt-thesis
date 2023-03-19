module Crdt

import Data.List
import Data.Vect
import Data.Nat

public export
interface Ord ty => Crdt ty where
  mergeCrdt : ty -> ty -> ty

public export
interface Crdt ty => VectorCrdt ty where
  increment : ty -> ty -> ty

public export
interface Crdt ty => SetCrdt ty where
  add : ty -> (a : ty) -> ty

public export
Crdt Nat where
  mergeCrdt Z Z = Z
  mergeCrdt Z (S k) = S k
  mergeCrdt (S k) Z = S k
  mergeCrdt (S j) (S k) = S (mergeCrdt j k)

public export
Crdt Bool where
  mergeCrdt True _ = True
  mergeCrdt _ True = True
  mergeCrdt _ _ = False

public export
Crdt a => Crdt (Vect m a) where
  mergeCrdt [] [] = []
  mergeCrdt (x::xs) (y::ys) = (mergeCrdt x y) :: mergeCrdt xs ys

public export
Crdt a => Crdt (List a) where
  mergeCrdt [] [] = []
  mergeCrdt xs ys = union xs ys

public export
Crdt a => Crdt b => Crdt (a, b) where
  mergeCrdt (x1, y1) (x2, y2) = ((mergeCrdt x1 x2), (mergeCrdt y1 y2))

public export
merge : (Crdt crdt) => (c1, c2 : (crdt)) -> crdt
merge c1 c2 = mergeCrdt c1 c2