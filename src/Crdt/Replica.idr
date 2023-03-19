module Replica

import Crdt
import Log
import VectorClock

import Data.List
import Data.Vect
import Data.Nat

record Replica (a : Type) where
  constructor MkReplica
  payload : (Crdt a) => a

merge : (Crdt a) => Replica a -> Replica a -> Replica a
merge r1 r2 = let newState = merge r1.payload r2.payload in
  {payload := newState} r1

setReplica : Replica (List Nat)
setReplica = MkReplica [0,1,2]

vectorReplica : Replica (Vect 3 Nat)
vectorReplica = MkReplica [0,8,0]

logReplica : Replica (Log 3 String)
logReplica = MkReplica (MkLog [] ((MkVectorClock 0 defaultClock)))

data Test = MkTest String Bool

mergeIdempotentTest : Test
mergeIdempotentTest =
  let test = (merge vectorReplica vectorReplica).payload == [0,8,0] in
    MkTest "Merge of same replicas is idempotent" test

tests : List Test
tests = [mergeIdempotentTest]