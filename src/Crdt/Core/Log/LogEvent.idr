
module Core.Log.LogEvent

import Data.Vect

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