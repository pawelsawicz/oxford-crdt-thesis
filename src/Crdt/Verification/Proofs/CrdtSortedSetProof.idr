module Verification.Proofs.CrdtSortedSetProof

import public Data.SortedSet

import Core.Semilattice

import Verification.CommutativeMonoid

union_commutes :  (x, y : (SortedSet a)) ->
  (foldr Data.SortedSet.insert x y) = (foldr Data.SortedSet.insert y x)

-- union_commutes
-- union_commutes (SetWrapper m) (SetWrapper n) = ?hole

-- lub_sortedset_commutes : (x, y : (SortedSet a)) -> lub x y = lub y x
-- lub_sortedset_commutes x y = ?hole

lub_sortedset_idempotent : (x : (SortedSet a)) -> lub x x = x
lub_sortedset_idempotent xs = ?hole2

lub_sortedset_assoc : (x, y, z : (SortedSet a)) -> (lub x (lub y z)) = (lub (lub x y) z)
lub_sortedset_assoc xs ys zs = ?hole3

commutativeMonoidProof : CommutativeMonoid (SortedSet a)
commutativeMonoidProof =
  MkCMon
  ?h1
  ?h2
  ?h3
  ?h4
  ?h5
  ?h6
