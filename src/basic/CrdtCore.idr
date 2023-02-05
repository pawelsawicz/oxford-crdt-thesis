module CrdtCore

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat

-- interface Ord a => EnableCrdt a where
--     incrementCrdt : a -> a


record Replica (size : Nat) (a : Type) where
    constructor MkReplica
    replicaIndex : Fin size
    state : Vect size a
    queryFunc : Replica size a -> a
    --updateFunc : Replica size a -> (a -> a -> a) -> Replica size a

createReplica : Fin size -> Vect size a -> (Replica size a -> a)->
                 Replica size a
createReplica ind xs qf = MkReplica ind xs qf

testNatReplica : Replica 3 Nat
testNatReplica = createReplica 1 [0,0,0] (\r => sum r.state)

testSetReplica : Replica 3 (List a)
testSetReplica = createReplica 1 [[],[],[]] (\r => Data.Vect.index r.replicaIndex r.state)

--createTestStringReplica : Replica 3 String
--createTestStringReplica = createReplica 1 ["a", "a", "a"]

-- Query

query : Replica size a -> a
query r = r.queryFunc r

queryTest : Bool
queryTest = query testNatReplica == 0

--queryNat : Vect size Nat -> Nat
--queryNat xs = sum xs 

--queryList : Vect size (List a) -> List a
--queryList = Data.Vect.index r.replicaIndex r.state
-- Update

updateNat : SingleValueReplica size Nat -> (Nat -> Nat -> Nat) -> Nat -> SingleValueReplica size Nat
updateNat r f e = let newState = updateAt r.replicaIndex (f e) r.state in
                { state := newState } r

update : Replica size a -> Replica size a
update r = ?hole

updateSetLocal : List a -> a -> List a
updateSetLocal = Data.List.snoc