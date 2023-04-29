module Verification.Proofs.CrdtVectProof

import Data.Vect
import Data.Nat
import Verification.CommutativeMonoid

import Verification.Proofs.CrdtNatProof

import Core.Semilattice

%default total

public export
lub_vect_commutes : (xs, ys : (Vect n Nat)) ->
  lub xs ys = lub ys xs

lub_vect_commutes [] [] = Refl
lub_vect_commutes (x::xs) (y::ys) = 
  let rec = lub_nat_commutes x y in
  let rec2 = lub_vect_commutes xs ys in
    rewrite rec in
    rewrite rec2 in Refl

public export
lub_vect_idempotent : (xs : (Vect n Nat)) -> 
  lub xs xs = xs

lub_vect_idempotent [] = Refl
lub_vect_idempotent (x::xs) = 
  let rec = lub_nat_idempotent x in
  let rec2 = lub_vect_idempotent xs in
    rewrite rec in
    rewrite rec2 in Refl

public export
lub_vect_identity : {n : Nat} -> (xs : (Vect n Nat)) ->
  lub (Data.Vect.replicate n Z) xs = xs

lub_vect_identity [] = Refl
lub_vect_identity (x::xs) = 
  let rec = lub_nat_identity x in
  let rec2 = lub_vect_identity xs in
    rewrite rec in
    rewrite rec2 in Refl

public export
lub_vect_assoc : (xs, ys, zs : (Vect n Nat)) ->
  lub xs (lub ys zs) = lub (lub xs ys) zs

lub_vect_assoc [] [] [] = Refl
lub_vect_assoc (x::xs) (y::ys) (z::zs) = 
  let rec = lub_nat_assoc x y z in
  let rec2 = lub_vect_assoc xs ys zs in
    rewrite rec in
    rewrite rec2 in Refl

public export
commutativeMonoidProof : {n : Nat} -> CommutativeMonoid (Vect n Nat)
commutativeMonoidProof = 
  MkCMon
    lub
    (Data.Vect.replicate n Z)
    lub_vect_commutes
    lub_vect_idempotent
    lub_vect_identity
    lub_vect_assoc