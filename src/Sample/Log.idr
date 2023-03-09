module Log

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat

record Replica (size : Nat) (a : Type) where
    constructor MkReplica
    replicaIndex : Fin size
    state : Vect size (List a)

createReplica : Fin size -> Vect size (List a) -> Replica size a
createReplica ind xs = MkReplica ind xs