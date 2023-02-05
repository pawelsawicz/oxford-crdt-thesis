module SetReplica

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat

record Replica (size : Nat) (a : Type) where
    constructor MkReplica
    replicaIndex : Fin size
    state : Vect size a
    queryFunc : Replica size a -> a

createReplica : Fin size -> Vect size a -> (Replica size a -> a)->
                 Replica size a
createReplica ind xs qf = MkReplica ind xs qf

testSetNatReplica : Replica 3 (List Nat)
testSetNatReplica = createReplica 1 [[1],[2],[3]] (\r => Data.Vect.index r.replicaIndex r.state)

testReplicaEarlyState : Replica 3 (List Nat)
testReplicaEarlyState = createReplica 1 [[1],[2],[3]] (\r => Data.Vect.index r.replicaIndex r.state)

testReplicaLateState : Replica 3 (List Nat)
testReplicaLateState = createReplica 1 [[1],[2,8],[3,5]] (\r => Data.Vect.index r.replicaIndex r.state)

query : Replica size a -> a
query r = r.queryFunc r

update : Replica len (List ele) -> (List ele -> ele -> List ele) -> ele -> Replica len (List ele)
update r f a = let newSet = f (index r.replicaIndex r.state) a in
                    let newState = replaceAt r.replicaIndex newSet r.state in
                        { state := newState } r

mergeFunc : (Eq ele) => Vect size (List ele) -> Vect size (List ele) -> Vect size (List ele)
mergeFunc [] [] = []
mergeFunc (x::xs) (y::ys) = (union x y) :: mergeFunc xs ys

merge : (Eq ele) => Replica size (List ele) -> Replica size (List ele) -> Replica size (List ele)
merge r1 r2 = let newState = mergeFunc r1.state r2.state in
                    { state := newState} r1

testQuery : (String, Bool)
testQuery = let test = query testSetNatReplica == [2] in 
                ("Query for Replica Set equals [2]", test)

testUpdate : (String, Bool)
testUpdate = let test = (update testSetNatReplica snoc 5).state == [[1],[2,5],[3]] in
                ("Update for Replica Set equals [2,5]", test)

testMerge : (String, Bool)
testMerge = let test = (merge testReplicaEarlyState testReplicaLateState).state == [[1],[2,8],[3,5]] in
                ("Merge for Replica set equals [[1],[2,8],[3,5]]", test)

tests : List (String, Bool)
tests = [testQuery, testUpdate, testMerge]