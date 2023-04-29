module Verification.Proofs.CrdtNatProof

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat

import Core.Semilattice

import Verification.CommutativeMonoid

%default total

public export
lub_nat_commutes : (n, m : Nat) -> lub n m = lub m n
lub_nat_commutes Z Z = Refl
lub_nat_commutes Z (S k) = Refl
lub_nat_commutes (S k) Z = Refl
lub_nat_commutes (S j) (S k) = rewrite lub_nat_commutes j k in Refl

public export
lub_nat_idempotent : (n : Nat) -> lub n n = n
lub_nat_idempotent Z = Refl
lub_nat_idempotent (S j) = rewrite lub_nat_idempotent j in Refl

public export
lub_nat_assoc : (n, m, o : Nat) -> lub n (lub m o) = lub (lub n m) o
lub_nat_assoc Z Z Z = Refl
lub_nat_assoc Z Z (S k) = Refl
lub_nat_assoc Z (S j) Z = Refl
lub_nat_assoc (S i) Z Z = Refl
lub_nat_assoc (S i) (S j) Z = Refl
lub_nat_assoc (S i) Z (S k) = Refl
lub_nat_assoc Z (S j) (S k) = Refl
lub_nat_assoc (S i) (S j) (S k) = rewrite lub_nat_assoc i j k in Refl

public export
lub_nat_identity : (n : Nat) -> lub Z n = n
lub_nat_identity Z = Refl
lub_nat_identity (S k) = Refl

public export
commutativeMonoidProof : CommutativeMonoid Nat
commutativeMonoidProof =
  MkCMon
    lub
    Z
    lub_nat_commutes
    lub_nat_idempotent
    lub_nat_identity
    lub_nat_assoc