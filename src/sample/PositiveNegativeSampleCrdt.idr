module PositiveNegativeSampleCrdt

import Data.Vect
import Data.Fin
import Data.Nat


record Replica (size: Nat) where
    constructor PNCounterReplica
    replicaIndex : Fin size
    pState : Vect size Nat
    nState : Vect size Nat

data UpdateOperation = Positive | Negative

createReplica : (i: Fin len) 
                    -> (xs : Vect len Nat) -> (ys : Vect len Nat)
                    -> Replica len
createReplica ind xs ys = PNCounterReplica ind xs ys

testReplica : Replica 3
testReplica = createReplica 1 [0, 0, 0] [0, 1, 1]

-- read

query : Replica len -> Integer
query r = let pValue = natToInteger (sum r.pState) in 
            let nValue = natToInteger (sum r.nState) in
                pValue - nValue

-- write part

updatePositive : Replica len -> Replica len
updatePositive r = let newState = updateAt r.replicaIndex (+1) r.pState in
                    createReplica r.replicaIndex newState r.nState

updateNegative : Replica len -> Replica len
updateNegative r = let newState = updateAt r.replicaIndex (+1) r.nState in
                    createReplica r.replicaIndex r.pState newState

update : UpdateOperation -> Replica len -> Replica len
update Positive r = let newState = updateAt r.replicaIndex (+1) r.pState in
                    createReplica r.replicaIndex newState r.nState
update Negative r = let newState = updateAt r.replicaIndex (+1) r.nState in
                    createReplica r.replicaIndex r.pState newState

--merge
merge : Vect len Nat -> Vect len Nat -> Vect len Nat
merge [] [] = []
merge (x::xs) (y::ys) = (if x > y then x else y) 
                                :: merge xs ys