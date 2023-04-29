module Core.PNCounterCrdt

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat

import Core.GrowOnlyCounterCrdt

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