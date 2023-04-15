module Crdt

import Data.List
import Data.Vect
import Data.Nat

import Data.SortedSet

import TestExtensions
import Log

%default total

public export
record Crdt (a : Type) where
  constructor MkCrdt
  QueryType : Type
  UpdateType : Type

  query : a -> QueryType
  update : UpdateType
  merge : a -> a -> a

createLog : {k : Nat} -> 
  Crdt (SortedSet (LogEvent k), (Vect k Nat))

createLog = MkCrdt
  (SortedSet (LogEvent k))
  ((Fin k) -> (SortedSet (LogEvent k), (Vect k Nat)) -> 
    (ele : String) -> (SortedSet (LogEvent k), (Vect k Nat)))
  query
  update
  mergeCrdt

log : Crdt (SortedSet (LogEvent 3), (Vect 3 Nat))
log = createLog

log1 : (SortedSet (LogEvent 3), (Vect 3 Nat))
log1 = (fromList [MkLogEvent "p" [1,0,0]], [1,0,0])

log2 : (SortedSet (LogEvent 3), (Vect 3 Nat))
log2 = (fromList [MkLogEvent "a" [0,1,0]], [0,1,0])

log3 : (SortedSet (LogEvent 3), (Vect 3 Nat))
log3 = (fromList [MkLogEvent "w" [0,0,1]], [0,0,1])

logId : (SortedSet (LogEvent 3), (Vect 3 Nat))
logId = (empty, [0,0,0])

merge_comms_test : (Eq a) => (Crdt a) -> (xs, ys : a) -> Bool
merge_comms_test set xs ys = comms xs ys set.merge

merge_assoc_test : (Eq a) => (Crdt a) -> (xs, ys, zs : a) -> Bool
merge_assoc_test set xs ys zs = assoc xs ys zs set.merge

merge_idempotent_test : (Eq a) => (Crdt a) -> (xs : a) -> Bool
merge_idempotent_test set xs = idempotent xs set.merge

merge_identity : (Eq a) => (Crdt a) -> (xs, neutral : a) -> Bool
merge_identity set xs neutral = identity xs neutral set.merge

tests : List Bool
tests = [
  merge_comms_test log log1 log2
, merge_assoc_test log log1 log2 log3
, merge_idempotent_test log log1
, merge_identity log log1 logId
]