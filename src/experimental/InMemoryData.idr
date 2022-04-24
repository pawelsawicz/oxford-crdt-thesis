------In memory database for testing purpose

record CrdtReplica (size : Nat) where
    constructor CrdtReplica
    replicaIndex : Fin size
    state : ?hole -- some state ???


