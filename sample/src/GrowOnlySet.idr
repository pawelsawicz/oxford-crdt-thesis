module GrowOnlySet

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat

import Data.SortedSet

import TestExtensions

%default total

public export
query : SortedSet a -> List a
query xs = Data.SortedSet.toList xs

public export
update : SortedSet a -> (ele : a) -> SortedSet a
update xs ele = insert ele xs

public export
||| lub - Least Upper Bound
mergeCrdt : (Eq a) => (xs, ys: (SortedSet a)) -> SortedSet a
mergeCrdt xs ys = union xs ys

set1 : SortedSet Nat
set1 = fromList [1,2,3]

set2 : SortedSet Nat
set2 = fromList [2,4,5]

set3 : SortedSet Nat
set3 = fromList [3,8,7]

setId : SortedSet Nat
setId = empty

merge_comms_test : (Eq a) => (xs, ys : (SortedSet a)) -> Bool
merge_comms_test xs ys = comms xs ys mergeCrdt

merge_assoc_test : (Eq a) => (xs, ys, zs : (SortedSet a)) -> Bool
merge_assoc_test xs ys zs = assoc xs ys zs mergeCrdt

merge_idempotent_test : (Eq a) => (xs : (SortedSet a)) -> Bool
merge_idempotent_test xs = idempotent xs mergeCrdt

merge_identity : (Eq a) => (xs, neutral : (SortedSet a)) -> Bool
merge_identity xs neutral = identity xs neutral mergeCrdt

tests : List Bool
tests = [
  merge_comms_test set1 set2
, merge_assoc_test set1 set2 set3
, merge_idempotent_test set1
, merge_identity set1 setId
]