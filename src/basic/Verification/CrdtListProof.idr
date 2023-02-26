module CrdtListProof

import Data.Vect
import Data.Nat
import CommutativeMonoid
import VerifiedCrdt
import CrdtNatProof

%default total

merge_list_commutes : (xs, ys : (List Nat)) -> mergeCrdt xs ys = mergeCrdt ys xs
merge_list_commutes [] [] = Refl
merge_list_commutes xs ys = ?hole 
