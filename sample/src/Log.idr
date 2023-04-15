module Log

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat

import TestExtensions
import GrowOnlyCounter
import GrowOnlySet

import Data.SortedSet

%default total

public export
record LogEvent k where
  constructor MkLogEvent
  body : String
  clock : Vect k Nat

public export
Eq (LogEvent k) where
  l1 == l2 = l1.clock == l2.clock

public export
Ord (LogEvent k) where
  l1 `compare` l2 = l1.clock `compare` l2.clock

public export
query : (SortedSet (LogEvent k), (Vect k Nat)) ->
  SortedSet (LogEvent k)

query (l, c) = l

public export
update : (Fin k) -> (SortedSet (LogEvent k), (Vect k Nat)) ->
  (ele : String) ->
  (SortedSet (LogEvent k), (Vect k Nat))

update i (l, c) ele =
  let updatedClock = update i c in
  let logEvent = MkLogEvent ele updatedClock in
  let updatedLog = update l logEvent in
   (updatedLog, updatedClock)

public export
mergeCrdt : (xs, ys : (SortedSet (LogEvent k), (Vect k Nat))) ->
  (SortedSet (LogEvent k), (Vect k Nat))

mergeCrdt (l1, c1) (l2, c2) =
  let clock_merge = mergeCrdt c1 c2 in
  let log_merge = mergeCrdt l1 l2 in
    (log_merge, clock_merge)

log1 : (SortedSet (LogEvent 3), (Vect 3 Nat))
log1 = (fromList [(MkLogEvent "p" [1,0,0])], [1,0,0])

log2 : (SortedSet (LogEvent 3), (Vect 3 Nat))
log2 = (fromList [(MkLogEvent "a" [0,1,0])], [0,1,0])

log3 : (SortedSet (LogEvent 3), (Vect 3 Nat))
log3 = (fromList [(MkLogEvent "w" [0,0,1])], [0,0,1])

logid : (SortedSet (LogEvent 3), (Vect 3 Nat))
logid = (empty, [0,0,0])

merge_comms_test : (xs, ys : (SortedSet (LogEvent k), (Vect k Nat))) -> Bool
merge_comms_test xs ys = comms xs ys mergeCrdt

merge_assoc_test : (xs, ys, zs : (SortedSet (LogEvent k), (Vect k Nat))) -> Bool
merge_assoc_test xs ys zs = assoc xs ys zs mergeCrdt

merge_idempotent_test : (xs : (SortedSet (LogEvent k), (Vect k Nat))) -> Bool
merge_idempotent_test xs = idempotent xs mergeCrdt

merge_identity : (xs, neutral : (SortedSet (LogEvent k), (Vect k Nat))) -> Bool
merge_identity xs neutral = identity xs neutral mergeCrdt

tests : List Bool
tests = [
  merge_comms_test log1 log2
, merge_assoc_test log1 log2 log3
, merge_idempotent_test log1
, merge_identity log1 logid
]