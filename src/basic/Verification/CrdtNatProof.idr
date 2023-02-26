module CrdtNatProof

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat
import CommutativeMonoid
import VerifiedCrdt

mergeCrdt_nat_commutes : (n, m : Nat) -> mergeCrdt n m = mergeCrdt m n
mergeCrdt_nat_commutes Z Z = Refl
mergeCrdt_nat_commutes Z (S k) = Refl
mergeCrdt_nat_commutes (S k) Z = Refl
mergeCrdt_nat_commutes (S j) (S k) = rewrite mergeCrdt_nat_commutes j k in Refl

mergeCrdt_nat_idempotent : (n : Nat) -> mergeCrdt n n = n
mergeCrdt_nat_idempotent Z = Refl
mergeCrdt_nat_idempotent (S j) = rewrite mergeCrdt_nat_idempotent j in Refl

mergeCrdt_lub_assoc : (n, m, o : Nat) -> mergeCrdt n (mergeCrdt m o) = mergeCrdt (mergeCrdt n m) o
mergeCrdt_lub_assoc Z Z Z = Refl
mergeCrdt_lub_assoc Z Z (S k) = Refl
mergeCrdt_lub_assoc Z (S j) Z = Refl
mergeCrdt_lub_assoc (S i) Z Z = Refl
mergeCrdt_lub_assoc (S i) (S j) Z = Refl
mergeCrdt_lub_assoc (S i) Z (S k) = Refl
mergeCrdt_lub_assoc Z (S j) (S k) = Refl
mergeCrdt_lub_assoc (S i) (S j) (S k) = rewrite mergeCrdt_lub_assoc i j k in Refl

mergeCrdt_lub_identity : (n : Nat) -> mergeCrdt Z n = n
mergeCrdt_lub_identity Z = Refl
mergeCrdt_lub_identity (S k) = Refl

total
natCrdtCommutativeMonoid : CommutativeMonoid Nat
natCrdtCommutativeMonoid = MkCMon
                             mergeCrdt
                             Z
                             mergeCrdt_nat_commutes
                             mergeCrdt_nat_idempotent
                             mergeCrdt_lub_identity
                             mergeCrdt_lub_assoc