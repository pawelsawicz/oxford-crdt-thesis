
||| Proofs for pair type CRDT (a,b) i.e Positive-Negative Counter or Two Phase Set
module CrdtPairProof

import Data.Vect
import Data.Nat
import CommutativeMonoid
import CoreCrdt.Crdt
import CrdtVectProof

%default total

merge_pair_commutes : (xs, ys : ((Vect k Nat), (Vect k Nat))) -> mergeCrdt xs ys = mergeCrdt ys xs
merge_pair_commutes ([],[]) ([],[]) = Refl
merge_pair_commutes (x1,y1) (x2,y2) = let rec = merge_vect_commutes x1 x2 in
                                      let rec2 = merge_vect_commutes y1 y2 in
                                        rewrite rec in
                                        rewrite rec2 in Refl

merge_pair_idempotent : (xs : ((Vect k Nat), (Vect k Nat))) -> mergeCrdt xs xs = xs
merge_pair_idempotent ([],[]) = Refl
merge_pair_idempotent (x1,y1) = let rec = merge_vect_idempotent x1 in
                                let rec2 = merge_vect_idempotent y1 in
                                  rewrite rec in
                                  rewrite rec2 in Refl

merge_pair_identity : {k : Nat} -> (xs : ((Vect k Nat), (Vect k Nat))) -> mergeCrdt (Data.Vect.replicate k Z, Data.Vect.replicate k Z) xs = xs
merge_pair_identity ([],[]) = Refl
merge_pair_identity (x1,y1) = let rec = merge_vect_identity x1 in
                              let rec2 = merge_vect_identity y1 in
                                rewrite rec in
                                rewrite rec2 in Refl

merge_pair_assoc : (xs, ys, zs : ((Vect k Nat), (Vect k Nat))) -> mergeCrdt xs (mergeCrdt ys zs) = mergeCrdt (mergeCrdt xs ys) zs
merge_pair_assoc ([],[]) ([],[]) ([],[]) = Refl
merge_pair_assoc (x1,y1) (x2,y2) (x3,y3) = let rec = merge_vect_assoc x1 x2 x3 in
                                           let rec2 = merge_vect_assoc y1 y2 y3 in
                                            rewrite rec in
                                            rewrite rec2 in Refl

pairVectCommutativeMonoid : {k : Nat} -> CommutativeMonoid ((Vect k Nat),(Vect k Nat))
pairVectCommutativeMonoid = MkCMon
                              mergeCrdt
                              (Data.Vect.replicate k Z, Data.Vect.replicate k Z)
                              merge_pair_commutes
                              merge_pair_idempotent
                              merge_pair_identity
                              merge_pair_assoc
