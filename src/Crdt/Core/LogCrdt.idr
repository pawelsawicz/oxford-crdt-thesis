module Core.LogCrdt

import Data.Vect

import Core.Log.LogEvent
import Core.GrowOnlyCounterCrdt
import Core.GrowOnlySetCrdt

import Data.SortedSet

%default total

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