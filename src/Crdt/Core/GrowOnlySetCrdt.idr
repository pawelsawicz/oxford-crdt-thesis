module Core.GrowOnlySetCrdt

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat

import Data.SortedSet

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