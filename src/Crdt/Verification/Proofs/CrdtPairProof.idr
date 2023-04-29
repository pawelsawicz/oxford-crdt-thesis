
||| Proofs for pair type CRDT (a,b) i.e Positive-Negative Counter or Two Phase Set
module Verification.Proofs.CrdtPairProof

import Data.Vect
import Data.Nat

import Verification.CommutativeMonoid
import Verification.Proofs.CrdtVectProof

import Core.Semilattice

%default total

lub_pair_commutes : (xs, ys : ((Vect k Nat), (Vect k Nat))) ->
  lub xs ys = lub ys xs

lub_pair_commutes ([],[]) ([],[]) = Refl
lub_pair_commutes (x1,y1) (x2,y2) = 
  let rec = lub_vect_commutes x1 x2 in
  let rec2 = lub_vect_commutes y1 y2 in
    rewrite rec in
    rewrite rec2 in Refl

lub_pair_idempotent : (xs : ((Vect k Nat), (Vect k Nat))) ->
  lub xs xs = xs

lub_pair_idempotent ([],[]) = Refl
lub_pair_idempotent (x1,y1) =
  let rec = lub_vect_idempotent x1 in
  let rec2 = lub_vect_idempotent y1 in
    rewrite rec in
    rewrite rec2 in Refl

lub_pair_identity : {k : Nat} -> (xs : ((Vect k Nat), (Vect k Nat))) ->
  lub (Data.Vect.replicate k Z, Data.Vect.replicate k Z) xs = xs

lub_pair_identity ([],[]) = Refl
lub_pair_identity (x1,y1) =
  let rec = lub_vect_identity x1 in
  let rec2 = lub_vect_identity y1 in
    rewrite rec in
    rewrite rec2 in Refl

lub_pair_assoc : (xs, ys, zs : ((Vect k Nat), (Vect k Nat))) ->
  lub xs (lub ys zs) = lub (lub xs ys) zs

lub_pair_assoc ([],[]) ([],[]) ([],[]) = Refl
lub_pair_assoc (x1,y1) (x2,y2) (x3,y3) =
  let rec = lub_vect_assoc x1 x2 x3 in
  let rec2 = lub_vect_assoc y1 y2 y3 in
    rewrite rec in
    rewrite rec2 in Refl

public export
commutativeMonoidProof : {k : Nat} -> 
  CommutativeMonoid ((Vect k Nat),(Vect k Nat))

commutativeMonoidProof =
  MkCMon
    lub
    (Data.Vect.replicate k Z, Data.Vect.replicate k Z)
    lub_pair_commutes
    lub_pair_idempotent
    lub_pair_identity
    lub_pair_assoc
