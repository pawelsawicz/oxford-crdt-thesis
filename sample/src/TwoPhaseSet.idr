module TwoPhaseSet

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat

import Data.SortedSet

import TestExtensions
import GrowOnlySet

%default total

public export
data SetOperation = Add | Remove

public export
query : (Eq a) => (SortedSet a, SortedSet a) -> List a
query (a, t) = Data.SortedSet.toList (difference a t)

--public export
add : (SortedSet a, SortedSet a) -> (ele : a) -> SortedSet a
add (xs, _) ele = update xs ele 

--public export
remove : (SortedSet a, SortedSet a) -> (ele : a) -> SortedSet a
remove (_, ys) ele = update ys ele

public export
update : SetOperation -> 
  (SortedSet a, SortedSet a) -> (ele : a) -> SortedSet a

update Add = add
update Remove = remove

public export
||| lub - Least Upper Bound
mergeCrdt : (Eq a) => (xs, ys: (SortedSet a, SortedSet a)) ->
  (SortedSet a, SortedSet a)
mergeCrdt (a1, r1) (a2, r2) =
  let add_merge = mergeCrdt a1 a2 in
  let remove_merge = mergeCrdt r1 r2 in
    (add_merge, remove_merge)

twoset1 : (SortedSet Nat, SortedSet Nat)
twoset1 = (fromList [1,2,3], fromList [3])

twoset2 : (SortedSet Nat, SortedSet Nat)
twoset2 = (fromList [3,4,5], fromList [4])

twoset3 : (SortedSet Nat, SortedSet Nat)
twoset3 = (fromList [5,6,7], fromList [5])

twosetId : (SortedSet Nat, SortedSet Nat)
twosetId = (empty, empty)

merge_comms_test : (Eq a) => (xs, ys : (SortedSet a, SortedSet a)) -> Bool
merge_comms_test xs ys = comms xs ys mergeCrdt

merge_assoc_test : (Eq a) => (xs, ys, zs : (SortedSet a, SortedSet a)) -> Bool
merge_assoc_test xs ys zs = assoc xs ys zs mergeCrdt

merge_idempotent_test : (Eq a) => (xs : (SortedSet a, SortedSet a)) -> Bool
merge_idempotent_test xs = idempotent xs mergeCrdt

merge_identity : (Eq a) => (xs, neutral : (SortedSet a, SortedSet a)) -> Bool
merge_identity xs neutral = identity xs neutral mergeCrdt

tests : List Bool
tests = [
  merge_comms_test twoset1 twoset2
, merge_assoc_test twoset1 twoset2 twoset3
, merge_idempotent_test twoset1
, merge_identity twoset1 twosetId
]