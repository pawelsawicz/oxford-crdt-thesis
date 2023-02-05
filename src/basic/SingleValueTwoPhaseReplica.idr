module SingleValueTwoPhaseReplica

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat

record TwoPhaseReplica (size : Nat) (a : Type) where
    constructor MkTwoPhaseReplica
    replicaIndex : Fin size
    state1 : Vect size a
    state2 : Vect size a

createReplica : Fin size -> Vect size a -> Vect size a -> TwoPhaseReplica size a
createReplica ind xs ys = MkTwoPhaseReplica ind xs ys

testReplica : TwoPhaseReplica 3 Nat
testReplica = createReplica 1 [0,0,0] [0,0,0]

query : TwoPhaseReplica size Nat -> Integer
query r = let pValue = natToInteger (sum r.state1) in
          let nValue = natToInteger (sum r.state2) in
            pValue - nValue

increase : TwoPhaseReplica size Nat -> TwoPhaseReplica size Nat
increase r = let newState = updateAt r.replicaIndex (+1) r.state1 in
                { state1 := newState } r

decrease : TwoPhaseReplica size Nat -> TwoPhaseReplica size Nat
decrease r = let newState = updateAt r.replicaIndex (+1) r.state2 in
                { state2 := newState } r

mergeStrategy : Nat -> Nat -> Nat
mergeStrategy x y = case x `compare` y of
                            LT => y
                            GT => x
                            EQ => x

mergeLocal : Vect len Nat -> Vect len Nat -> Vect len Nat
mergeLocal [] [] = []
mergeLocal (x::xs) (y::ys) = (mergeStrategy x y) :: mergeLocal xs ys

testReplica1 : TwoPhaseReplica 3 Nat
testReplica1 = createReplica 1 [1,0,0] [2,0,0]

testReplica2 : TwoPhaseReplica 3 Nat
testReplica2 = createReplica 1 [0,1,0] [0,0,2]

merge : TwoPhaseReplica size Nat -> TwoPhaseReplica size Nat -> TwoPhaseReplica size Nat
merge r1 r2 = let pNewState = mergeLocal r1.state1 r2.state1 in
                let nNewState = mergeLocal r1.state2 r2.state2 in
                    { state1 := pNewState, state2 := nNewState} r1

data Test = MkTest String Bool

testQuery : Test
testQuery = let test = query testReplica == 0 in
                MkTest "Query should equal to 0" test

testIncrease : Test
testIncrease = let test = (increase testReplica).state1 == [0,1,0] in
                MkTest "Increase should equal to [0,1,0]" test

testDecrease : Test
testDecrease = let test = (decrease testReplica).state2 == [0,1,0] in
                MkTest "Decrease should equal to [0,1,0]" test

testMergePositive : Test
testMergePositive = let test = (merge testReplica1 testReplica2).state1 == [1,1,0] in
                MkTest "Merge should equal to [1,1,0]" test

testMergeNegative : Test
testMergeNegative = let test = (merge testReplica1 testReplica2).state2 == [2,0,2] in
                MkTest "Merge should equal to [1,1,0]" test

tests : List Test
tests = [testQuery, testIncrease, testDecrease, testMergePositive, testMergeNegative]