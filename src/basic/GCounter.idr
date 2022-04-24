module GCounter

import Data.Vect

-- data
record DReplica (size: Nat) where
    constructor DCounterReplica
    replicaIndex : Fin size
    state : Vect size Nat

record Replica (size: Nat) where
    constructor CounterReplica
    --replicaIndex : Fin size
    state : Vect size Nat

-- state
state : Vect 5 Nat
state = [0, 0, 0, 0, 0]

statePosition : Nat
statePosition = 1

--associative
--commutative
--indepotent

--read

value : Vect k Nat -> Nat
value ve = sum ve


--write

increment : Vect k Nat -> Vect k Nat
--increment ve = updateAt 1 (+1) ve

merge : Nat -> Nat -> Nat
merge x y = x + y