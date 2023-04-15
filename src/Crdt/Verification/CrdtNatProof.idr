module Verification.CrdtNatProof

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat

import Verification.CommutativeMonoid

import Core.Crdt

%default total

-- public export
-- mergeCrdt_nat_commutes : (n, m : Nat) -> mergeCrdt n m = mergeCrdt m n
-- mergeCrdt_nat_commutes Z Z = Refl
-- mergeCrdt_nat_commutes Z (S k) = Refl
-- mergeCrdt_nat_commutes (S k) Z = Refl
-- mergeCrdt_nat_commutes (S j) (S k) = rewrite mergeCrdt_nat_commutes j k in Refl

-- public export
-- mergeCrdt_nat_idempotent : (n : Nat) -> mergeCrdt n n = n
-- mergeCrdt_nat_idempotent Z = Refl
-- mergeCrdt_nat_idempotent (S j) = rewrite mergeCrdt_nat_idempotent j in Refl

-- public export
-- mergeCrdt_nat_assoc : (n, m, o : Nat) -> mergeCrdt n (mergeCrdt m o) = mergeCrdt (mergeCrdt n m) o
-- mergeCrdt_nat_assoc Z Z Z = Refl
-- mergeCrdt_nat_assoc Z Z (S k) = Refl
-- mergeCrdt_nat_assoc Z (S j) Z = Refl
-- mergeCrdt_nat_assoc (S i) Z Z = Refl
-- mergeCrdt_nat_assoc (S i) (S j) Z = Refl
-- mergeCrdt_nat_assoc (S i) Z (S k) = Refl
-- mergeCrdt_nat_assoc Z (S j) (S k) = Refl
-- mergeCrdt_nat_assoc (S i) (S j) (S k) = rewrite mergeCrdt_nat_assoc i j k in Refl

-- public export
-- mergeCrdt_nat_identity : (n : Nat) -> mergeCrdt Z n = n
-- mergeCrdt_nat_identity Z = Refl
-- mergeCrdt_nat_identity (S k) = Refl

-- public export
-- commutativeMonoidProof : CommutativeMonoid Nat
-- commutativeMonoidProof = MkCMon
--                              mergeCrdt
--                              Z
--                              mergeCrdt_nat_commutes
--                              mergeCrdt_nat_idempotent
--                              mergeCrdt_nat_identity
--                              mergeCrdt_nat_assoc