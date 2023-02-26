module Replica

public export

record Replica (a : Type) where
    constructor MkReplica
    payload : (Crdt a) => a