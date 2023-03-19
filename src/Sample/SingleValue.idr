module SingleValue

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat

data Test = MkTest String Bool

record Replica (size : Nat) where
  constructor MkReplica
  replicaIndex : Fin size
  state : Vect size Nat

createReplica : Fin size -> Vect size Nat -> Replica size
createReplica ind xs = MkReplica ind xs

testReplica : Replica 3
testReplica = createReplica 1 [0,0,0]

queryVector : Replica size -> Vect size Nat
queryVector r = r.state

query : Replica size -> Nat
query r = sum r.state

update : Replica size -> Replica size
update r = 
  let newState = updateAt r.replicaIndex (+1) r.state in
    { state := newState } r

mergeStrategy : Nat -> Nat -> Nat
mergeStrategy x y = 
  case x `compare` y of
    LT => y
    GT => x
    EQ => x

mergeLocal : Vect len Nat -> Vect len Nat -> Vect len Nat
mergeLocal [] [] = []
mergeLocal (x::xs) (y::ys) = (mergeStrategy x y) :: mergeLocal xs ys

merge : Replica size -> Replica size -> Replica size
merge r1 r2 =
  let newState = mergeLocal r1.state r2.state in
    { state := newState} r1

testReplica1 : Replica 3
testReplica1 = createReplica 1 [0,0,3]

testReplica2 : Replica 3
testReplica2 = createReplica 1 [5,6,0]

testQuery : Test
testQuery = 
  let test = query testReplica1 == 3 in
    MkTest "Query for Replica equals 3" test

testUpdate : Test
testUpdate = 
  let test = (update testReplica1).state == [0,1,3] in
    MkTest "Update for Replica equals [0,1,3]" test

testMerge : Test
testMerge =
  let test = (merge testReplica1 testReplica2).state == [5,6,3] in
    MkTest "Merge for Replica Nat equals [5,6,3]" test

tests : List Test
tests = [testQuery, testUpdate, testMerge]