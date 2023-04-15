module Core.GrowOnlyCounterCrdt

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat

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