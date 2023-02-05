module GrowOnlySampleCrdt

import Data.Vect
import Data.Fin
import Data.Nat

-- G-Only Counter

record Replica (size : Nat) where
    constructor CounterReplica
    replicaIndex : Fin size
    state : Vect size Nat

createReplica : (i : Fin len) -> (xs : Vect len Nat) -> Replica len
createReplica ind xs = CounterReplica ind xs

testReplica : Replica 3
testReplica = createReplica 1 [0, 0, 0]

-- read part
value : Replica len -> Nat
value r = sum r.state

-- write part
update : Replica len -> Replica len
update r = let updatedState = 
                    updateAt r.replicaIndex (+1) r.state in
                createReplica r.replicaIndex updatedState

--merge
merge : Vect len Nat -> Vect len Nat -> Vect len Nat
merge [] [] = []
merge (x::xs) (y::ys) = (if x > y then x else y) 
                                :: merge xs ys