module Main

import Data.List
import Data.SOP
import Data.String
import Data.Vect
import Hedgehog

import CrdtTests.GrowOnlyCounterTests
import CrdtTests.GrowOnlySetTests
import CrdtTests.TwoPhaseSetTests

%default total

main : IO ()
main = test [
  MkGroup "Grow Only Counter" [
    ("Merge Idempotent" , GrowOnlyCounterTests.property_merge_idempotent)
  , ("Merge Comms", GrowOnlyCounterTests.property_merge_comms)
  , ("Merge Assoc", GrowOnlyCounterTests.property_merge_assoc)
  , ("Merge Identity", GrowOnlyCounterTests.property_merge_identity)
  ],
  MkGroup "Grow Only Set" [
    ("Merge Idempotent" , GrowOnlySetTests.property_merge_idempotent)
  , ("Merge Comms", GrowOnlySetTests.property_merge_comms)
  , ("Merge Assoc", GrowOnlySetTests.property_merge_assoc)
  , ("Merge Identity", GrowOnlySetTests.property_merge_identity)
  ],
  MkGroup "Two Phase Set" [
    ("Merge Idempotent" , TwoPhaseSetTests.property_merge_idempotent)
  , ("Merge Comms", TwoPhaseSetTests.property_merge_comms)
  , ("Merge Assoc", TwoPhaseSetTests.property_merge_assoc)
  , ("Merge Identity", TwoPhaseSetTests.property_merge_identity)
  ]
]