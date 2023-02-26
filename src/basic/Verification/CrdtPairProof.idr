
||| Proofs for pair type CRDT (a,b) i.e Positive-Negative Counter or Two Phase Set
module CrdtPairProof

import Data.Vect
import Data.Nat
import CommutativeMonoid
import VerifiedCrdt
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

public export
merge_pair_assoc : (xs, ys, zs : ((Vect k Nat), (Vect k Nat))) -> mergeCrdt xs (mergeCrdt ys zs) = mergeCrdt (mergeCrdt xs ys) zs
merge_pair_assoc ([],[]) ([],[]) ([],[]) = Refl
merge_pair_assoc xs ys zs = ?holelet