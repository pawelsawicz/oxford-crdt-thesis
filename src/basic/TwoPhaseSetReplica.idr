module TwoPhaseSetReplica

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat

data Test = MkTest String Bool

record Replica (a : Type) where
    constructor MkReplica
    addSet : List a
    removeSet : List a

createReplica : List a -> List a -> Replica a
createReplica xs ys = MkReplica xs ys

testReplica : Replica a
testReplica = createReplica [] []

testReplica1 : Replica Nat
testReplica1 = createReplica [] []

testReplica2 : Replica a
testReplica2 = createReplica [] []

-- returns set difference between A \ R (R tombstone set).
-- client can remove from R if element exists in R
query : (Eq ele) => Replica ele -> List ele
query r = r.addSet \\ r.removeSet

add : (Eq ele) => ele -> Replica ele -> Replica ele
add x replica = let newState = snoc replica.addSet x in
                    { addSet := newState } replica

remove : (Eq ele) => ele -> Replica ele -> Replica ele
remove x replica = let newState = snoc replica.removeSet x in
                    { removeSet := newState } replica

merge : (Eq ele) => Replica ele -> Replica ele -> Replica ele
merge r1 r2 = let newAddSet = union r1.addSet r2.addSet in
                let newRemoveSet = union r1.removeSet r2.removeSet in
                    { addSet := newAddSet, removeSet := newRemoveSet} r1

testQuery : Test
testQuery = let test = (query testReplica1) == [] in
                MkTest "Query returns []" test

testAdd : Test
testAdd = let test = (add 2 testReplica1).addSet == [2] in
                MkTest "Add returnes [2]" test

testRemove : Test
testRemove = let test = (remove 2 testReplica1).removeSet == [2] in
                MkTest "Remove returns [2]" test

tests : List Test
tests = [testQuery, testAdd, testRemove]