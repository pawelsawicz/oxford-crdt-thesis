module PNCounter

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat

import GrowOnlyCounter
import TestExtensions

%default total

public export
data CrdtOperation = Increase | Decrease

public export
query : (Vect n Nat, Vect n Nat) -> Integer
query (xs, ys) =
  let pSum = natToInteger (queryCounter xs) in
  let nSum = natToInteger (queryCounter ys) in
    pSum - nSum

increase : Fin n -> (Vect n Nat, Vect n Nat) -> Vect n Nat
increase i (xs, _) = update i xs

decrease : Fin n -> (Vect n Nat, Vect n Nat) -> Vect n Nat
decrease i (_, ys) = update i ys

public export
update : CrdtOperation -> Fin n -> 
  (Vect n Nat, Vect n Nat) -> Vect n Nat

update Increase = increase
update Decrease = decrease

public export
||| lub - Least Upper Bound
mergeCrdt : (c1, c2 : (Vect n Nat, Vect n Nat)) ->
  (Vect n Nat, Vect n Nat)

mergeCrdt (p1, n1) (p2, n2) =
  let p_merge = mergeCrdt p1 p2 in
  let n_merge = mergeCrdt n1 n2 in
    (p_merge, n_merge)

pn1 : (Vect 3 Nat, Vect 3 Nat)
pn1 = ([0,0,0], [0,0,0])

pn2 : (Vect 3 Nat, Vect 3 Nat)
pn2 = ([0,0,0], [0,1,0])

pn3 : (Vect 3 Nat, Vect 3 Nat)
pn3 = ([1,0,0], [0,0,1])

pnId : (Vect 3 Nat, Vect 3 Nat)
pnId = ([0,0,0], [0,0,0])

merge_comms_test : (xs, ys : (Vect n Nat, Vect n Nat)) -> Bool
merge_comms_test xs ys = comms xs ys mergeCrdt

merge_assoc_test : (xs, ys, zs : (Vect n Nat, Vect n Nat)) -> Bool
merge_assoc_test xs ys zs = assoc xs ys zs mergeCrdt

merge_idempotent_test : (xs : (Vect n Nat, Vect n Nat)) -> Bool
merge_idempotent_test xs = idempotent xs mergeCrdt

merge_identity : (xs, neutral : (Vect n Nat, Vect n Nat)) -> Bool
merge_identity xs neutral = identity xs neutral mergeCrdt

tests : List Bool
tests = [
  merge_comms_test pn1 pn2
, merge_assoc_test pn1 pn2 pn3
, merge_idempotent_test pn1
, merge_identity pn1 pnId
]