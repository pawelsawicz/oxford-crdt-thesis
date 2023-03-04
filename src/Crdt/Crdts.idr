module Crdts

import Crdt

record Replica (a : Type) where
    constructor MkReplica
    payload : (Crdt a) => a

merge : (Crdt a) => Replica a -> Replica a -> Replica a
merge r1 r2 = let newState = mergeCrdt r1.payload r2.payload in
                {payload := newState} r1

-- query : Replica a -> a
-- query = ?hole

-- increment : Replica (Vect k Nat) -> Replica (Vect k Nat)
-- increment r1 r2 = ?hole
