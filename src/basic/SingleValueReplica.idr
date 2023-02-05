module SingleValueReplica

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat

record Replica (size : Nat) (a : Type) where
    constructor MkReplica
    replicaIndex : Fin size
    state : Vect size a
    queryFunc : Replica size a -> a
    updateFunc : Replica size a -> (a -> a -> a) -> a -> Replica size a
    mergeFunc : Vect size a -> Vect size a -> Vect size a

createReplica : Fin size -> Vect size a -> (Replica size a -> a)
                            -> (Replica size a -> (a -> a -> a) -> a -> Replica size a)
                            -> (Vect size a -> Vect size a -> Vect size a)
                            -> Replica size a
createReplica ind xs qf uf mf = MkReplica ind xs qf uf mf

updateLocal : Replica size Nat -> (Nat -> Nat -> Nat) -> Nat -> Replica size Nat
updateLocal r f e = let newState = updateAt r.replicaIndex (f e) r.state in
                { state := newState } r

mergeStrategy : Nat -> Nat -> Nat
mergeStrategy x y = case x `compare` y of
                            LT => y
                            GT => x
                            EQ => x

mergeLocal : Vect len Nat -> Vect len Nat -> Vect len Nat
mergeLocal [] [] = []
mergeLocal (x::xs) (y::ys) = (mergeStrategy x y) :: mergeLocal xs ys

testReplica : Replica 3 Nat
testReplica = createReplica 1 [0,0,0] (\r => sum r.state) (\r => (\f => (\ele => updateLocal r f ele))) mergeLocal

testReplicaEarlyState : Replica 3 Nat
testReplicaEarlyState = createReplica 1 [0,0,3] (\r => sum r.state) (\r => (\f => (\ele => updateLocal r f ele))) mergeLocal

testReplicaLateState : Replica 3 Nat
testReplicaLateState = createReplica 1 [5,6,0] (\r => sum r.state) (\r => (\f => (\ele => updateLocal r f ele))) mergeLocal

-- must return local state
query : Replica size a -> a
query r = r.queryFunc r

-- must monotically increase
update : Replica size Nat -> Replica size Nat
update r = r.updateFunc r (+) 1

-- must be commutative, associatvie and idempotent
merge : Replica size Nat -> Replica size Nat -> Replica size Nat
merge r1 r2 =  let newState = r1.mergeFunc r1.state r2.state in
                    { state := newState} r1



-- Generalise to work with any single value.....
-- Show how two phase could work.....


testQuery : (String, Bool)
testQuery = let test = query testReplicaEarlyState == 3 in
                ("Query for Replica Nat equals 3", test)

testUpdate : (String, Bool)
testUpdate = let test = (update testReplicaEarlyState).state == [0,1,3] in
                ("Update for Replica Nat equals [0,1,3]", test)

testMerge : (String, Bool)
testMerge = let test = (merge testReplicaEarlyState testReplicaLateState).state == [5,6,3] in
                ("Merge two replicas equals to [5,6,3]", test)

tests : List (String, Bool)
tests = [testQuery, testUpdate, testMerge]