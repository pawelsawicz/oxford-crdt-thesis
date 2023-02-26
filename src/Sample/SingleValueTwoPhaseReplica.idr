module SingleValueTwoPhaseReplica

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat

record Replica (size : Nat) where
    constructor MkReplica
    replicaIndex : Fin size
    pState : Vect size Nat
    nState : Vect size Nat

createReplica : Fin size -> Vect size Nat -> Vect size Nat -> Replica size
createReplica ind xs ys = MkReplica ind xs ys

testReplica : Replica 3
testReplica = createReplica 1 [0,0,0] [0,0,0]

query : Replica size -> Integer
query r = let pValue = natToInteger (sum r.pState) in
          let nValue = natToInteger (sum r.nState) in
            pValue - nValue

increase : Replica size -> Replica size
increase r = let newState = updateAt r.replicaIndex (+1) r.pState in
                { pState := newState } r

decrease : Replica size -> Replica size
decrease r = let newState = updateAt r.replicaIndex (+1) r.nState in
                { nState := newState } r

mergeStrategy : Nat -> Nat -> Nat
mergeStrategy x y = case x `compare` y of
                            LT => y
                            GT => x
                            EQ => x

mergeLocal : Vect len Nat -> Vect len Nat -> Vect len Nat
mergeLocal [] [] = []
mergeLocal (x::xs) (y::ys) = (mergeStrategy x y) :: mergeLocal xs ys

merge : Replica size -> Replica size -> Replica size
merge r1 r2 = let pNewState = mergeLocal r1.pState r2.pState in
                let nNewState = mergeLocal r1.nState r2.nState in
                    { pState := pNewState, nState := nNewState} r1

testReplica1 : Replica 3
testReplica1 = createReplica 1 [1,0,0] [2,0,0]

testReplica2 : Replica 3
testReplica2 = createReplica 1 [0,1,0] [0,0,2]

data Test = MkTest String Bool

testQuery : Test
testQuery = let test = query testReplica == 0 in
                MkTest "Query should equal to 0" test

testIncrease : Test
testIncrease = let test = (increase testReplica).pState == [0,1,0] in
                MkTest "Increase should equal to [0,1,0]" test

testDecrease : Test
testDecrease = let test = (decrease testReplica).nState == [0,1,0] in
                MkTest "Decrease should equal to [0,1,0]" test

testMergePositive : Test
testMergePositive = let test = (merge testReplica1 testReplica2).pState == [1,1,0] in
                MkTest "Merge should equal to [1,1,0]" test

testMergeNegative : Test
testMergeNegative = let test = (merge testReplica1 testReplica2).nState == [2,0,2] in
                MkTest "Merge should equal to [1,1,0]" test

tests : List Test
tests = [testQuery, testIncrease, testDecrease, testMergePositive, testMergeNegative]