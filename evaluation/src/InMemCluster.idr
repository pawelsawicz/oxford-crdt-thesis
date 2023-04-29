module InMemCluster

import Core.Crdt

record Replica where
  constructor MkReplica
  replicaIndex : 
  crdt : Crdt a
  state : a