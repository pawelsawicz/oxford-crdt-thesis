module VerifiedCrdt

import Verification.CommutativeMonoid

import Data.List
import Data.Vect
import Data.Nat

import CrdtNatProof
import CrdtVectProof
import CrdtPairProof

%default total

natCommutativeMonoid : CommutativeMonoid Nat
natCommutativeMonoid = CrdtNatProof.commutativeMonoidProof

vectNatCommutativeMonoid : {n : Nat} -> CommutativeMonoid (Vect n Nat)
vectNatCommutativeMonoid = CrdtVectProof.commutativeMonoidProof

pairVectCommutativeMonoid : {k : Nat} -> CommutativeMonoid ((Vect k Nat),(Vect k Nat))
pairVectCommutativeMonoid = CrdtPairProof.commutativeMonoidProof