import Data.Vect
import Data.Fin
import Data.Nat

--------------------
--- DataType: Nat
--- Incremental function: (+1)
--- Merge function: if x > y then x else y



--interface; ad-hoc polymorphism and extension
--data; data structure
--record; database like structure with read/update capability

-- how to split data / persistance matter from behaviour matter ?

-- Nat

-- value
---- allow obtain some value from state
--interface CrdtPrintable 

-- increment/update
----monotonically increase; can we provide any proof on that ?

-- merge
----associative; (a + b) + c <=> a + (b + c)
----commutative; a + b <=> b + a
----idempotent; a + b <=> b + a <=> b <=> a

-- partial order

interface Ord a => CrdtMergable a where
    mergeStrategy : a -> a -> a

--- proof: associative
--- proof: commutative
--- proof: idempotent
CrdtMergable Nat where
    mergeStrategy x y = case x `compare` y of
                            LT => y
                            GT => x
                            EQ => x

--interface Crdt a where
--    value : [a] -> a
--    update : Vect len a -> Vect len a
--    merge : Vect len a -> Vect len a -> Vect len a

---data CrdtGCounter

--------------------

record Replica (size : Nat) where
    constructor CounterReplica
    replicaIndex : Fin size
    state : Vect size Nat

-- constructor
createReplica : (i: Fin len) -> (xs : Vect len Nat) -> Replica len
createReplica ind xs = CounterReplica ind xs

testReplica : Replica 3
testReplica = createReplica 1 [0, 0, 0]

-- read part

value : Replica len -> Nat
value r = sum r.state


-- write part
increment : Replica len -> Replica len
increment r = let updatedState = updateAt r.replicaIndex (+1) r.state in
                createReplica r.replicaIndex updatedState


-- write part merging
-- mergeStrategy : (Ord elem) => elem -> elem -> elem
-- mergeStrategy x y = case x `compare` y of
--                         LT => y
--                         GT => x
--                         EQ => x

mergeLocal : Vect len Nat -> Vect len Nat -> Vect len Nat
mergeLocal [] [] = []
--mergeLocal (x::[]) (y::[]) = [mergeStrategy x y]
mergeLocal (x::xs) (y::ys) = (mergeStrategy x y) :: mergeLocal xs ys


-- top level merge

merge : Replica len -> Vect len Nat -> Replica len
merge r ys = let newState = mergeLocal r.state ys in
                createReplica r.replicaIndex newState


--------------
---PN Counter
--- DataType: Nat, Nat
--- Incremental function: (+1) on Positive
--- Decrement function: (+1) on Negative
--- Merge function: if x > y then x else y

-- Can we somehow use previously defined and proved G-Counter as NP-Counter ?

--Vect len Nat; Vect len Nat
--increment; (+1) on 1
--decrement; (+1) on 2

record PNReplica (size: Nat) where
    constructor PNCounterReplica
    replicaIndex : Fin size
    pState : Vect size Nat
    nState : Vect size Nat

-- constructor
createPNReplica : (i: Fin len) -> (xs : Vect len Nat) -> (ys : Vect len Nat) -> PNReplica len
createPNReplica ind xs ys = PNCounterReplica ind xs ys

testPNReplica : PNReplica 3
testPNReplica = createPNReplica 1 [0, 0, 0] [0, 1, 1]

-- read part

valuePN : PNReplica len -> Integer
valuePN r = let pValue = natToInteger (sum r.pState) in 
            let nValue = natToInteger (sum r.nState) in
                pValue - nValue

-- write part

--incrementFunc : (+1)
incrementFunc : Nat -> Nat
incrementFunc = (+1)

incrementP : PNReplica len -> PNReplica len
incrementP r = let newState = updateAt r.replicaIndex incrementFunc r.pState in
                createPNReplica r.replicaIndex newState r.nState

decrementN : PNReplica len -> PNReplica len
decrementN r = let newState = updateAt r.replicaIndex incrementFunc r.nState in
                createPNReplica r.replicaIndex r.pState newState


incrementPNTest : PNReplica 3
incrementPNTest = incrementP $ incrementP $ incrementP testPNReplica

decrementPNTest : PNReplica 3
decrementPNTest = decrementN $ decrementN $ decrementN testPNReplica

---can there be written proof for Integer ?

addPlusOneTest : Integer -> Integer
addPlusOneTest = (+1)

testProof : (addPlusOneTest 1) = 2
testProof = Refl



--testProof = Refl
---decrement <=> incremenet, just acting on different constituents of pn-counter
--- decrement/increment should be generalised, also can we update values via lenses ?


---- merge pn-counter

----- do merge on each crdt; merge p and merge n