module VerifiedCrdt

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat

-- Least Upper Bound
-- bear in mind that we should use natCompare which is optimised for O(1)
lubNat : Nat -> Nat -> Nat
lubNat Z Z = Z
lubNat Z (S k) = S k
lubNat (S k) Z = S k
lubNat (S j) (S k) = S (lubNat j k)

-- Least Upper Bound
lubBool : Bool -> Bool -> Bool
lubBool True _ = True
lubBool _ True = True
lubBool _ _ = False

lubVect : Vect m Nat -> Vect m Nat -> Vect m Nat
lubVect [] [] = []
lubVect (x::xs) (y::ys) = (lubNat x y) :: lubVect xs ys

public export
interface Ord ty => Crdt ty where
    mergeCrdt : ty -> ty -> ty

public export
Crdt Nat where
    mergeCrdt Z Z = Z
    mergeCrdt Z (S k) = S k
    mergeCrdt (S k) Z = S k
    mergeCrdt (S j) (S k) = S (mergeCrdt j k)

public export
Crdt Bool where
    mergeCrdt = lubBool

public export
Crdt a => Crdt (Vect m a) where
    mergeCrdt [] [] = []
    mergeCrdt (x::xs) (y::ys) = (mergeCrdt x y) :: mergeCrdt xs ys

public export
record Replica (a : Type) where
    constructor MkReplica
    payload : (Crdt a) => a

-- record CommutativeMonoid (carrier : Type) where
--   constructor MkCMon
--   op : carrier -> carrier -> carrier
--   neutral : carrier
--   commutative : (x, y : carrier) -> op x y = op y x
--   idempotent : (x : carrier) -> op x x = x
--   identity : (x : carrier) -> op neutral x = x
--   associativitiy : (x, y, z : carrier) -> op x (op y z) = op (op x y) z

-- mergeCrdt_nat_commutes : (n, m : Nat) -> mergeCrdt n m = mergeCrdt m n
-- mergeCrdt_nat_commutes Z Z = Refl
-- mergeCrdt_nat_commutes Z (S k) = Refl
-- mergeCrdt_nat_commutes (S k) Z = Refl
-- mergeCrdt_nat_commutes (S j) (S k) = rewrite mergeCrdt_nat_commutes j k in Refl

-- mergeCrdt_nat_idempotent : (n : Nat) -> mergeCrdt n n = n
-- mergeCrdt_nat_idempotent Z = Refl
-- mergeCrdt_nat_idempotent (S j) = rewrite mergeCrdt_nat_idempotent j in Refl

-- mergeCrdt_lub_assoc : (n, m, o : Nat) -> mergeCrdt n (mergeCrdt m o) = mergeCrdt (mergeCrdt n m) o
-- mergeCrdt_lub_assoc Z Z Z = Refl
-- mergeCrdt_lub_assoc Z Z (S k) = Refl
-- mergeCrdt_lub_assoc Z (S j) Z = Refl
-- mergeCrdt_lub_assoc (S i) Z Z = Refl
-- mergeCrdt_lub_assoc (S i) (S j) Z = Refl
-- mergeCrdt_lub_assoc (S i) Z (S k) = Refl
-- mergeCrdt_lub_assoc Z (S j) (S k) = Refl
-- mergeCrdt_lub_assoc (S i) (S j) (S k) = rewrite mergeCrdt_lub_assoc i j k in Refl

-- mergeCrdt_lub_identity : (n : Nat) -> mergeCrdt Z n = n
-- mergeCrdt_lub_identity Z = Refl
-- mergeCrdt_lub_identity (S k) = Refl

-- total
-- natCrdtCommutativeMonoid : CommutativeMonoid Nat
-- natCrdtCommutativeMonoid = MkCMon
--                              mergeCrdt
--                              Z
--                              mergeCrdt_nat_commutes
--                              mergeCrdt_nat_idempotent
--                              mergeCrdt_lub_identity
--                              mergeCrdt_lub_assoc

-- lub_commutes : (n, m : Nat) -> lubNat n m = lubNat m n
-- lub_commutes Z Z = Refl
-- lub_commutes Z (S k) = Refl
-- lub_commutes (S k) Z = Refl
-- lub_commutes (S j) (S k) = rewrite lub_commutes j k in Refl

-- lubVect_commutes : (xs, ys : Vect a Nat) -> lubVect xs ys = lubVect ys xs
-- lubVect_commutes [] [] = Refl
-- lubVect_commutes xs ys = rewrite lubVect_commutes xs ys in Refl

--mergeCrdt_lubVect_commutes_rhs : mergeCrdt 0 m = mergeCrdt m 0