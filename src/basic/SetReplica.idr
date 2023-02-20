module SetReplica

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat

data Test = MkTest String Bool

record Replica (a : Type) where
    constructor MkReplica
    addSet : List a

createReplica : List a -> Replica a
createReplica xs = MkReplica xs

-- testSetNatReplica : Replica 3 (List Nat)
-- testSetNatReplica = createReplica 1 [[1],[2],[3]] (\r => Data.Vect.index r.replicaIndex r.state)

-- testReplicaEarlyState : Replica 3 (List Nat)
-- testReplicaEarlyState = createReplica 1 [[1],[2],[3]] (\r => Data.Vect.index r.replicaIndex r.state)

-- testReplicaLateState : Replica 3 (List Nat)
-- testReplicaLateState = createReplica 1 [[1],[2,8],[3,5]] (\r => Data.Vect.index r.replicaIndex r.state)

query : Replica a -> List a
query r = r.addSet

update : Replica a -> a -> Replica a
update r1 elem = let newSet = snoc r1.addSet elem in
                {addSet := newSet} r1

merge : (Eq a) => Replica a -> Replica a -> Replica a
merge r1 r2 = let newSet = union r1.addSet r2.addSet in
                {addSet := newSet} r1

-- update : Replica len (List ele) -> (List ele -> ele -> List ele) -> ele -> Replica len (List ele)
-- update r f a = let newSet = f (index r.replicaIndex r.state) a in
--                     let newState = replaceAt r.replicaIndex newSet r.state in
--                         { state := newState } r

-- mergeFunc : (Eq ele) => Vect size (List ele) -> Vect size (List ele) -> Vect size (List ele)
-- mergeFunc [] [] = []
-- mergeFunc (x::xs) (y::ys) = (union x y) :: mergeFunc xs ys

-- merge : (Eq ele) => Replica size (List ele) -> Replica size (List ele) -> Replica size (List ele)
-- merge r1 r2 = let newState = mergeFunc r1.state r2.state in
--                     { state := newState} r1

test : Replica Nat
test = createReplica []

testReplica1 : Replica Nat
testReplica1 = createReplica [1,2]

testReplica2 : Replica Nat
testReplica2 = createReplica [3,4]

testQuery : Test
testQuery = let test = query testReplica1 == [1,2] in 
                MkTest "Query for Replica Set equals [1,2]" test

testUpdate : Test
testUpdate = let test = (update testReplica1 5).addSet == [1,2,5] in
                MkTest "Update for Replica Set equals [1,2,5]" test

testMerge : Test
testMerge = let test = (merge testReplica1 testReplica2).addSet == [1,2,3,4] in
                MkTest "Merge for Replica set equals [1,2,3,4]" test

tests : List Test
tests = [testQuery, testUpdate, testMerge]