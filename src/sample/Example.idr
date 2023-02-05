module Example

import Data.Vect
import Data.Fin
import Data.Nat

interface Ord a => EnableCrdt a where
    incrementCrdt : a -> a

record (EnableCrdt a ) => Replica (size : Nat) (a : Type) where
    constructor GrowSetCrdt
    replicaIndex : Fin size
    state : Vect size a

EnableCrdt Nat where
    incrementCrdt = (+1)


query : Replica len ele -> ele
query r = ?hole

update : Replica len ele -> Replica len ele
update r = let updatedState = updateAt updateAt r.replicaIndex incrementCrdt r.state in
            createReplica r.replicaIndex updatedState

merge : Replica len ele -> Replica len ele -> Replica len ele
merge = ?hole