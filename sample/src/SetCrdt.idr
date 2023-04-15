module SetCrdt

import Data.List
import Data.Vect
import Data.Nat

import Data.SortedSet

import Crdt
import TestExtensions
import GrowOnlySet
import TwoPhaseSet

%default total

public export
createGrowOnlySet : {a : Type} -> (Eq a) => Crdt (SortedSet a)
createGrowOnlySet =
  MkCrdt
  (List a)
  (SortedSet a -> (ele : a) -> SortedSet a)
  query
  update
  mergeCrdt

public export
createTwoPhaseSet : {a : Type} -> (Eq a) => Crdt (SortedSet a, SortedSet a)
createTwoPhaseSet =
  MkCrdt
  (List a)
  (SetOperation -> (SortedSet a, SortedSet a) -> (ele : a) -> SortedSet a)
  query
  update
  mergeCrdt

goSet : Crdt (SortedSet Nat)
goSet = createGrowOnlySet

set1 : SortedSet Nat
set1 = fromList [1,2,3]

set2 : SortedSet Nat
set2 = fromList [3,4,5]

set3 : SortedSet Nat
set3 = fromList [4,5,6]

setId : SortedSet Nat
setId = fromList []

merge_comms_test : (Eq a) => (Crdt (SortedSet a)) -> (xs, ys : (SortedSet a)) -> Bool
merge_comms_test set xs ys = comms xs ys set.merge

merge_assoc_test : (Eq a) => (Crdt (SortedSet a)) -> (xs, ys, zs : (SortedSet a)) -> Bool
merge_assoc_test set xs ys zs = assoc xs ys zs set.merge

merge_idempotent_test : (Eq a) => (Crdt (SortedSet a)) -> (xs : (SortedSet a)) -> Bool
merge_idempotent_test set xs = idempotent xs set.merge

merge_identity : (Eq a) => (Crdt (SortedSet a)) -> (xs, neutral : (SortedSet a)) -> Bool
merge_identity set xs neutral = identity xs neutral set.merge

tests : List Bool
tests = [
  merge_comms_test goSet set1 set2
, merge_assoc_test goSet set1 set2 set3
, merge_idempotent_test goSet set1
, merge_identity goSet set1 setId
]

