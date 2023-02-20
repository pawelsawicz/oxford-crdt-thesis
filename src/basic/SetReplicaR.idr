module SetReplicaR

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat

data Test = MkTest String Bool

record Replica (a : Type) where
    constructor MkReplica
    state : List a

createReplica : List a -> Replica a
createReplica xs = MkReplica xs

testReplica : Replica a
testReplica = createReplica [] []

testReplica1 : Replica Nat
testReplica1 = createReplica [] []

testReplica2 : Replica a
testReplica2 = createReplica [] []

-- returns set difference between A \ R (R tombstone set).
-- client can remove from R if element exists in R
query : Replica ele -> List ele
query r = r.state1 \\ r.state2

add : (Eq ele) => ele -> Replica ele -> Replica ele
add x replica = let newState = snoc replica.state1 x in
                    { state1 := newState } replica

remove : (Eq ele) => ele -> TwoPhaseReplica ele -> TwoPhaseReplica ele
remove x replica = let newState = snoc replica.state2 x in
                    { state2 := newState } replica

testQuery : Test
testQuery = let test = (query testReplica1) == [] in
                MkTest "Query returns []" test

testAdd : Test
testAdd = let test = (add 2 testReplica1).state1 == [2] in
                MkTest "Add returnes [2]" test

testRemove : Test
testRemove = let test = (remove 2 testReplica1).state2 == [2] in
                MkTest "Remove returns [2]" test

tests : List Test
tests = [testQuery, testAdd, testRemove]