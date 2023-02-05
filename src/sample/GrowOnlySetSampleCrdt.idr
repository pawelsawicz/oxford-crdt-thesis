module GrowOnlySetSampleCrdt

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat



record Replica (size : Nat) (a : Type) where
    constructor GrowSetCrdt
    replicaIndex : Fin size
    state : Vect size (List a)

createReplica : (ind: Fin len) -> (xs : Vect len (List a)) -> Replica len a
createReplica ind xs = GrowSetCrdt ind xs

testReplica : Replica 2 Nat
testReplica = createReplica 1 [[1],[2]]

-- query

query : Replica len ele -> List ele
query r = Data.Vect.index r.replicaIndex r.state

-- add

add : Replica len ele -> (a : ele) -> Replica len ele
add r a = let newSetState = Data.List.snoc (index r.replicaIndex r.state) a in
            let newState = replaceAt r.replicaIndex newSetState r.state in
                createReplica r.replicaIndex newState

-- merge

merge : List a -> List a -> List a
merge = ?hole