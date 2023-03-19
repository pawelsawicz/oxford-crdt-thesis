module Log

import Crdt
import VectorClock

import Data.List
import Data.Nat
import Data.Vect

public export
record LogEvent a k where
  constructor MkLogEvent
  eventBody : a
  eventClock : Vect k Nat

public export
record Log k a where
  constructor MkLog
  payload : List (LogEvent a k)
  clock : VectorClock k

public export
Eq (LogEvent a k) where
  l1 == l2 = l1.eventClock == l2.eventClock

public export
Ord (LogEvent a k) where
  compare l1 l2 = compare l1.eventClock l2.eventClock

public export
Crdt (List (LogEvent a k)) where
  mergeCrdt xs ys = unionBy (\l1, l2 => l1.eventClock == l2.eventClock) xs ys

public export 
Eq (Log k event) where
  l1 == l2 = ?hole2

public export
Ord (Log k event) where
  compare l1 l2 = ?hole1

public export
Crdt (Log k event) where
  mergeCrdt l1 l2 = 
    let newClock = mergeCrdt l1.clock l2.clock in
    let newPayload = mergeCrdt l1.payload l2.payload in
      {payload := newPayload, clock := newClock} l1

createLog1 : Log 3 String
createLog1 = MkLog [] (MkVectorClock 0 defaultClock)

createLog2 : Log 3 String
createLog2 = MkLog [] (MkVectorClock 1 defaultClock)

createLog3 : Log 3 String
createLog3 = MkLog [] (MkVectorClock 2 defaultClock)

add : Log k a -> a -> Log k a
add l event =
  let newClock = increment l.clock in
  let logEvent = MkLogEvent event newClock.theClock in
  let newPayload = snoc l.payload logEvent in
    {clock := newClock, payload := newPayload} l

queryLog : Log k a -> ((List (LogEvent a k)), VectorClock k)
queryLog l = (l.payload, l.clock)

query : Log k a -> List a
query l = let events = l.payload in
  map (\e => e.eventBody) events

test : (List (LogEvent String 3), VectorClock 3)
test =
  let log1 = add (add createLog1 "p") "a" in
  let log2 = add (add createLog2 "w") "e" in
  let log3 = add createLog3 "l" in
  let mergeLogs = merge (merge log1 log2) log3 in
    queryLog mergeLogs

test1 : List String
test1 =
  let log1 = add (add createLog1 "p") "a" in
  let log2 = add (add createLog2 "w") "e" in
  let log3 = add createLog3 "l" in
  let mergeLogs = merge (merge log1 log2) log3 in
    query mergeLogs