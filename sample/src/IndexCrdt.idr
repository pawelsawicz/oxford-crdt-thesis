module IndexCrdt

import Data.List
import Data.Vect
import Data.Nat

import Crdt
import TestExtensions

import GrowOnlyCounter
import PNCounter

%default total

public export
createVectorClock : {n : Nat} -> Crdt (Vect n Nat)
createVectorClock =
  MkCrdt
  (Vect n Nat)
  (Fin n -> Vect n Nat -> Vect n Nat)
  query
  update
  mergeCrdt

public export
createGrowOnlyCounter : {n : Nat} -> Crdt (Vect n Nat)
createGrowOnlyCounter =
  MkCrdt
  Nat
  (Fin n -> Vect n Nat -> Vect n Nat)
  queryCounter
  update
  mergeCrdt

public export
pnCounter : {n : Nat} -> Crdt (Vect n Nat, Vect n Nat)
pnCounter =
  MkCrdt
  Integer
  (CrdtOperation -> Fin n -> (Vect n Nat, Vect n Nat) -> Vect n Nat)
  query
  update
  mergeCrdt

vc1 : Crdt (Vect 3 Nat)
vc1 = createVectorClock

c1 : Vect 3 Nat
c1 = [1,0,0]

c2 : Vect 3 Nat
c2 = [0,1,0]

c3 : Vect 3 Nat
c3 = [0,0,1]

c0 : Vect 3 Nat
c0 = [0,0,0]

public export
merge_comms_test : (Crdt (Vect 3 Nat)) -> (xs, ys : (Vect 3 Nat)) -> Bool
merge_comms_test vc xs ys = comms xs ys vc.merge

public export
merge_assoc_test : (Crdt (Vect 3 Nat)) -> (xs, ys, zs : (Vect 3 Nat)) -> Bool
merge_assoc_test vc xs ys zs = assoc xs ys zs vc.merge

public export
merge_idempotent_test : (Crdt (Vect 3 Nat)) -> (xs : (Vect 3 Nat)) -> Bool
merge_idempotent_test vc xs = idempotent xs vc.merge

public export
merge_identity : (Crdt (Vect 3 Nat)) -> (xs, neutral : (Vect 3 Nat)) -> Bool
merge_identity vc xs neutral = identity xs neutral vc.merge

tests1 : List Bool
tests1 = [
  merge_comms_test c1 c2
, merge_assoc_test c1 c2 c3
, merge_idempotent_test c1
, merge_identity c1 c0
]