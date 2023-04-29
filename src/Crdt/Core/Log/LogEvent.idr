
module Core.Log.LogEvent

import Data.Vect

%default total

public export
record LogEvent k a where
  constructor MkLogEvent
  body : a
  clock : Vect k Nat

public export
Eq (LogEvent k a) where
  l1 == l2 = l1.clock == l2.clock

public export
Ord (LogEvent k a) where
  l1 `compare` l2 = l1.clock `compare` l2.clock