module ExpCrdt

import Data.List
import Data.Vect
import Data.Nat

record IndexCrdt (n : Nat) (a : Type) where
  constructor MkIndexCrdt
  Observation : Type
  --op
  initial : IndexCrdt n a
  query : IndexCrdt n a -> Observation
  update : Fin n -> IndexCrdt n a -> IndexCrdt n a
  merge : IndexCrdt n a -> IndexCrdt n a -> IndexCrdt n a

record SetCrdt (a : Type) where
  constructor MkSetCrdt
  Observation : Type
  --op
  initial : SetCrdt a
  query : SetCrdt a -> Observation
  update : (ele : a) -> SetCrdt a -> SetCrdt a
  merge : SetCrdt a -> SetCrdt a -> SetCrdt a

--merge is commutative, associative, idempotent;
--updates commute with each other (?)
--updates commute with merge (?)
--queries behave “as expected” with respect to update and merge 
--(what does that mean? I guess there are functions u, m such that

-- query (update i x) = u (query x)
-- query (merge x y) = m (query x) (query y)

--Oh, and I think “query” answers my question about equivalence: you shouldn’t worry about whether
--update i (update j x) = update j (update i x)


--but only that
--query (update i (update j x)) = query (update j (update i x))