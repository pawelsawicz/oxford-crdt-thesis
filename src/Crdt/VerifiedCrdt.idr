module VerifiedCrdt

import Verification.CommutativeMonoid

import Data.List
import Data.Vect
import Data.Nat

import Verification.Proofs.CrdtNatProof
import Verification.Proofs.CrdtVectProof
import Verification.Proofs.CrdtPairProof

%default total

vectNatCommutativeMonoid : {n : Nat} -> CommutativeMonoid (Vect n Nat)
vectNatCommutativeMonoid = CrdtVectProof.commutativeMonoidProof

pairVectCommutativeMonoid : {k : Nat} -> CommutativeMonoid ((Vect k Nat),(Vect k Nat))
pairVectCommutativeMonoid = CrdtPairProof.commutativeMonoidProof