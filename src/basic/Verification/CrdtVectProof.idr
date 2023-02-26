module CrdtVectProof

import Data.Vect
import Data.Nat
import CommutativeMonoid
import VerifiedCrdt
import CrdtNatProof

%default total

merge_vect_commutes : (xs, ys : (Vect n Nat)) -> mergeCrdt xs ys = mergeCrdt ys xs
merge_vect_commutes [] [] = Refl
merge_vect_commutes (x::xs) (y::ys) = let rec = mergeCrdt_nat_commutes x y in 
                                      let rec2 = merge_vect_commutes xs ys in
                                        rewrite rec in
                                        rewrite rec2 in Refl

merge_vect_idempotent : (xs : (Vect n Nat)) -> mergeCrdt xs xs = xs
merge_vect_idempotent [] = Refl
merge_vect_idempotent (x::xs) = let rec = mergeCrdt_nat_idempotent x in
                                let rec2 = merge_vect_idempotent xs in
                                  rewrite rec in
                                  rewrite rec2 in Refl

merge_vect_identity : {n : Nat} -> (xs : (Vect n Nat)) -> mergeCrdt (Data.Vect.replicate n Z) xs = xs
merge_vect_identity [] = Refl
merge_vect_identity (x::xs) = let rec = mergeCrdt_nat_identity x in
                              let rec2 = merge_vect_identity xs in
                              rewrite rec in
                              rewrite rec2 in Refl

merge_vect_assoc : (xs, ys, zs : (Vect n Nat)) -> mergeCrdt xs (mergeCrdt ys zs) = mergeCrdt (mergeCrdt xs ys) zs
merge_vect_assoc [] [] [] = Refl
merge_vect_assoc (x::xs) (y::ys) (z::zs) = let rec = mergeCrdt_nat_assoc x y z in
                                           let rec2 = merge_vect_assoc xs ys zs in
                                           rewrite rec in
                                           rewrite rec2 in Refl

vectNatCrdtCommutativeMonoid : {n : Nat} -> CommutativeMonoid (Vect n Nat)
vectNatCrdtCommutativeMonoid = MkCMon 
                                mergeCrdt
                                (Data.Vect.replicate n Z)
                                merge_vect_commutes
                                merge_vect_idempotent
                                merge_vect_identity
                                merge_vect_assoc