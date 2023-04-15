module Core.TwoPhaseSetCrdt

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat

import Data.SortedSet

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