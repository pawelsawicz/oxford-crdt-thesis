module CrdtTests.GrowOnlyCounterTests

import Data.List
import Data.SOP
import Data.String
import Data.Vect
import Hedgehog

import GrowOnlyCounter
import Helpers.PropertyExtension

%default total

vec_gen : Gen (Vect 5 Nat)
vec_gen = vect 5 $ nat (linear 0 20)

vec_gen_neutral : Gen (Vect 5 Nat)
vec_gen_neutral = vect 5 $ nat (linear 0 0)

public export
property_merge_comms : Property
property_merge_comms = comm_property vec_gen vec_gen mergeCrdt

public export
property_merge_assoc : Property
property_merge_assoc = assoc_property vec_gen vec_gen vec_gen mergeCrdt

public export
property_merge_idempotent : Property
property_merge_idempotent = idempotent_property vec_gen mergeCrdt

public export
property_merge_identity : Property
property_merge_identity = identity_property vec_gen vec_gen_neutral mergeCrdt
