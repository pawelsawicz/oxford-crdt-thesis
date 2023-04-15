module GrowOnlyCounter

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat

import TestExtensions

%default total

public export
query : Vect n Nat -> Vect n Nat
query xs = xs

public export
queryCounter : Vect n Nat -> Nat
queryCounter xs = sum xs

public export
update : Fin n -> Vect n Nat -> Vect n Nat
update i xs = updateAt i (+1) xs

public export
||| lub - Least Upper Bound
mergeCrdt : Vect n Nat -> Vect n Nat -> Vect n Nat
mergeCrdt [] [] = []
mergeCrdt (x::xs) (y::ys) = 
  let lub = if x >= y then x else y in
    lub :: (mergeCrdt xs ys)

vect1 : Vect 3 Nat
vect1 = [0,0,0]

vect2 : Vect 3 Nat
vect2 = [0,1,0]

vect3 : Vect 3 Nat
vect3 = [0,0,1]

vectId : Vect 3 Nat
vectId = [0,0,0]

public export
merge_comms_test : (xs, ys : (Vect n Nat)) -> Bool
merge_comms_test xs ys = comms xs ys mergeCrdt

public export
merge_assoc_test : (xs, ys, zs : (Vect n Nat)) -> Bool
merge_assoc_test xs ys zs = assoc xs ys zs mergeCrdt

public export
merge_idempotent_test : (xs : (Vect n Nat)) -> Bool
merge_idempotent_test xs = idempotent xs mergeCrdt

public export
merge_identity : (xs, neutral : Vect n Nat) -> Bool
merge_identity xs neutral = identity xs neutral mergeCrdt

tests : List Bool
tests = [
  merge_comms_test vect1 vect2
, merge_assoc_test vect1 vect2 vect3
, merge_idempotent_test vect1
, merge_identity vect1 vectId
]