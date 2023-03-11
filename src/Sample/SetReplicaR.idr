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

testReplica1 : Replica Nat
testReplica1 = createReplica []

query : Replica ele -> List ele
query r = r.state

add : (Eq ele) => ele -> Replica ele -> Replica ele
add x replica = let newState = snoc replica.state x in
                    { state := newState } replica

-- remove : (Eq ele) => ele -> TwoPhaseReplica ele -> TwoPhaseReplica ele
-- remove x replica = let newState = snoc replica.state2 x in
--                     { state2 := newState } replica

testQuery : Test
testQuery = let test = (query testReplica1) == [] in
                MkTest "Query returns []" test

testAdd : Test
testAdd = let test = (add 2 testReplica1).state == [2] in
                MkTest "Add returnes [2]" test

-- testRemove : Test
-- testRemove = let test = (remove 2 testReplica1).state2 == [2] in
--                 MkTest "Remove returns [2]" test

tests : List Test
tests = [testQuery, testAdd]