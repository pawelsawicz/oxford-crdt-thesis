module Core.Crdt

import Data.List
import Data.Vect
import Data.Nat

import Core.GrowOnlyCounterCrdt
import Core.GrowOnlySetCrdt
import Core.PNCounterCrdt
import Core.TwoPhaseSetCrdt
import Core.LogCrdt

import Core.Log.LogEvent

import Data.SortedSet

%default total

public export
record Crdt (a : Type) where
  constructor MkCrdt
  QueryType : Type
  UpdateType : Type

  query : a -> QueryType
  update : UpdateType
  merge : a -> a -> a

public export
createVectorClock : {n : Nat} -> Crdt (Vect n Nat)

createVectorClock =
  MkCrdt
  (Vect n Nat)
  (Fin n -> Vect n Nat -> Vect n Nat)
  query
  update
  mergeCrdt

public export
createGrowOnlyCounter : {n : Nat} -> Crdt (Vect n Nat)

createGrowOnlyCounter =
  MkCrdt
  Nat
  (Fin n -> Vect n Nat -> Vect n Nat)
  queryCounter
  update
  mergeCrdt

public export
pnCounter : {n : Nat} -> Crdt (Vect n Nat, Vect n Nat)

pnCounter =
  MkCrdt
  Integer
  (CrdtOperation -> Fin n -> (Vect n Nat, Vect n Nat) -> Vect n Nat)
  query
  update
  mergeCrdt

public export
createGrowOnlySet : {a : Type} -> (Eq a) => Crdt (SortedSet a)

createGrowOnlySet =
  MkCrdt
  (List a)
  (SortedSet a -> (ele : a) -> SortedSet a)
  query
  update
  mergeCrdt

public export
createTwoPhaseSet : {a : Type} -> (Eq a) => Crdt (SortedSet a, SortedSet a)

createTwoPhaseSet =
  MkCrdt
  (List a)
  (SetOperation -> (SortedSet a, SortedSet a) -> (ele : a) -> SortedSet a)
  query
  update
  mergeCrdt

public export
createLog : {k : Nat} -> 
  Crdt (SortedSet (LogEvent k), (Vect k Nat))

createLog = MkCrdt
  (SortedSet (LogEvent k))
  ((Fin k) -> (SortedSet (LogEvent k), (Vect k Nat)) -> 
    (ele : String) -> (SortedSet (LogEvent k), (Vect k Nat)))
  query
  update
  mergeCrdt