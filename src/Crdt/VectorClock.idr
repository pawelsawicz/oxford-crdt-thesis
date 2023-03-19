module VectorClock

import Crdt
import Data.Nat
import Data.Vect

public export
defaultClock : Vect 3 Nat
defaultClock = [0,0,0]

public export
record VectorClock k where
  constructor MkVectorClock
  index : Fin k
  theClock : Vect k Nat

public export
Eq (VectorClock k) where
  vc1 == vc2 = vc1.theClock == vc2.theClock

public export
Ord (VectorClock k) where
  compare vc1 vc2 = compare vc1.theClock vc2.theClock

public export
Crdt (VectorClock k) where
  mergeCrdt vc1 vc2 = 
    let clock = mergeCrdt vc1.theClock vc2.theClock in
      {theClock := clock} vc1

public export
increment : VectorClock k -> VectorClock k
increment vc = 
  let clock = updateAt vc.index (+1) vc.theClock in
    {theClock := clock} vc