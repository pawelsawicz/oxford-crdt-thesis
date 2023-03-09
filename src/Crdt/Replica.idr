module Replica

import Crdt

import Data.List
import Data.Vect
import Data.Nat

data Test = MkTest String Bool

record Replica (a : Type) where
  constructor MkReplica
  payload : (Crdt a) => a

merge : (Crdt a) => Replica a -> Replica a -> Replica a
merge r1 r2 = let newState = mergeCrdt r1.payload r2.payload in
  {payload := newState} r1

-- increment : Replica (Vect k Nat) -> Replica (Vect k Nat)
-- increment r1 r2 = ?hole

-- query : Replica a -> a
-- query = ?hole

counterReplica : Replica (Vect 3 Nat)
counterReplica = MkReplica [0,8,0]

setReplica : Replica (List Nat)
setReplica = MkReplica [0,1,2]

mergeIdempotentTest : Test
mergeIdempotentTest =
  let test = (merge counterReplica counterReplica).payload == [0,8,0] in
    MkTest "Merge of same replicas is idempotent" test

tests : List Test
tests = [mergeIdempotentTest]