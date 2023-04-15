module CrdtTests.TwoPhaseSetTests

import Data.List
import Data.SOP
import Data.String
import Data.Vect
import Hedgehog

import Data.SortedSet

import TwoPhaseSet
import Helpers.PropertyExtension

%default total

sortedset : Gen (SortedSet Nat, SortedSet Nat)
sortedset = pure (fromList [1,2,3,4], fromList [])

set_get : Gen (SortedSet Nat, SortedSet Nat)
set_get = sortedset

public export
property_merge_comms : Property
property_merge_comms = comm_property set_get set_get mergeCrdt

public export
property_merge_assoc : Property
property_merge_assoc = assoc_property set_get set_get set_get mergeCrdt

public export
property_merge_idempotent : Property
property_merge_idempotent = idempotent_property set_get mergeCrdt

public export
property_merge_identity : Property
property_merge_identity = identity_property set_get set_get mergeCrdt