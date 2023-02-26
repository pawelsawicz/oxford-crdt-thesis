module ExperimentalSetReplica

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

lubBool : Bool -> Bool -> Bool
lubBool True _ = True
lubBool _ True = True
lubBool _ _ = False

interface Ord ty => Crdt ty where
    mergeCrdt : ty -> ty -> ty

Crdt Nat where
    mergeCrdt = lubNat

Crdt Bool where
    mergeCrdt = lubBool

Crdt a => Crdt (Vect m a) where
    mergeCrdt [] [] = []
    mergeCrdt (x::xs) (y::ys) = (mergeCrdt x y) :: mergeCrdt xs ys

Crdt a => Crdt (List a) where
    mergeCrdt [] [] = []
    mergeCrdt xs ys = union xs ys

Crdt a => Crdt b => Crdt (a, b) where
    mergeCrdt (x1, y1) (x2, y2) = ((mergeCrdt x1 x2), (mergeCrdt y1 y2))


vec0 : Vect 3 Nat
vec0 = [0,0,0]

vec1 : Vect 3 Nat
vec1 = [1,0,6]

vec2 : Vect 3 Nat
vec2 = [0,5,0]

vec1b : Vect 3 Bool
vec1b = [True, False, False]

vec2b : Vect 3 Bool
vec2b = [False, True, True]

list0 : List Nat
list0 = []

list1 : List Nat
list1 = [1,2,3,4]

list2 : List Nat
list2 = [7,8,1,1]

-- Crdt a => Crdt (List a) where
--     mergeStrategy = compare

record Replica (a : Type) where
    constructor MkReplica
    payload : (Crdt a) => a

singleCounter1 : Replica (Vect 3 Nat)
singleCounter1 = MkReplica [0,8,0]

singleCounter2 : Replica (Vect 3 Nat)
singleCounter2 = MkReplica [5,5,5]

pnCounter1 : Replica (Vect 3 Nat, Vect 3 Nat)
pnCounter1 = MkReplica ([0,8,0], [0,5,0])

pnCounter2 : Replica (Vect 3 Nat, Vect 3 Nat)
pnCounter2 = MkReplica ([1,0,5], [2,0,3])

singleSet : Replica (List Nat)
singleSet = MkReplica list0

twoPhaseSet1 : Replica (List Nat, List Nat)
twoPhaseSet1 = MkReplica ([1,2], [2])

twoPhaseSet2 : Replica (List Nat, List Nat)
twoPhaseSet2 = MkReplica ([4,5,6], [5])

merge : (Crdt a) => Replica a -> Replica a -> Replica a
merge r1 r2 = let newState = mergeCrdt r1.payload r2.payload in
                {payload := newState} r1

---https://ncatlab.org/nlab/show/commutative+monoid
---https://ncatlab.org/nlab/show/semilattice
-- record CommutativeMonoid (carrier : Type) where
--   constructor MkCMon
--   op : (Crdt carrier) => carrier -> carrier -> carrier
  --neutral : carrier
  --commutative : (Crdt carrier) => (x, y : carrier) -> op x y = op y x
  --identity : (x : carrier) -> op neutral x = x
  --associativitiy : (x, y, z : carrier) -> op x (op y z) = op (op x y) z

-- mergeCrdt_commutes : (Crdt carrier) => (x, y : carrier) -> op x y = op y x

-- natCommutativeMonoid : CommutativeMonoid Nat
-- natCommutativeMonoid = MkCMon mergeCrdt 

--data Weekdays = Mon | Tus | Wed
-- plus_commutes_Z : (n : Nat) -> plus Z n = n
-- plus_commutes_Z Z = Refl
-- plus_commutes_Z (S k) = let rec = plus_commutes_Z k in
--                             rewrite sym rec in Refl

plus_reduces_Z : (m : Nat) -> plus Z m = m
plus_reduces_Z m = Refl

plus_reduces_Sk : (k, m : Nat) -> plus (S k) m = S (plus k m)
plus_reduces_Sk k m = Refl

plus_commutes_Z : (m : Nat) -> m = plus m Z
plus_commutes_Z Z = Refl
plus_commutes_Z (S k) = let rec = plus_commutes_Z k in
                            rewrite sym rec in Refl

-- total
plus_commutes_S : (k, m : Nat) -> S (plus m k) = plus m (S k)
plus_commutes_S k m = ?plus_commutes_S_rhs
-- plus_commutes_S k Z = Refl
-- plus_commutes_S k (S j) = rewrite plus_commutes_S k j in Refl

plus_commutes : (n, m : Nat) -> plus n m = plus m n
plus_commutes Z m = plus_commutes_Z m
plus_commutes (S k) m = rewrite plus_commutes k m in plus_commutes_S k m

-- lubCommutativeMonoid : CommutativeMonoid Nat
-- lubCommutativeMonoid = MkCommute plus plus_commutes

-- plusCommute : (x, y : Nat) -> (x + y) = (y + x)
-- plusCommute x y = Refl

-- natCommutativeMonoid : CommutativeMonoid Nat
-- natCommutativeMonoid = MkCMon partialOrdering


-- interface CommutativeMonoid (carrier : Type) where
--   (<+>) : carrier -> carrier -> carrier
--   neutral : carrier

lub_commutes : (n, m : Nat) -> lubNat n m = lubNat m n
lub_commutes Z Z = Refl
lub_commutes Z (S k) = Refl
lub_commutes (S k) Z = Refl
lub_commutes (S j) (S k) = rewrite lub_commutes j k in Refl

lub_idempotent : (n : Nat) -> lubNat n n = n
lub_idempotent Z = Refl
lub_idempotent (S j) = rewrite lub_idempotent j in Refl

lub_assoc : (n, m, o : Nat) -> lubNat n (lubNat m o) = lubNat (lubNat n m) o
lub_assoc Z Z Z = Refl
lub_assoc Z Z (S k) = Refl
lub_assoc Z (S j) Z = Refl
lub_assoc (S i) Z Z = Refl
lub_assoc (S i) (S j) Z = Refl
lub_assoc (S i) Z (S k) = Refl
lub_assoc Z (S j) (S k) = Refl
lub_assoc (S i) (S j) (S k) = rewrite lub_assoc i j k in Refl

lub_identity : (n : Nat) -> lubNat Z n = n
lub_identity Z = Refl
lub_identity (S k) = Refl

record CommutativeMonoid (carrier : Type) where
  constructor MkCMon
  op : carrier -> carrier -> carrier
  neutral : carrier
  commutative : (x, y : carrier) -> op x y = op y x
  idempotent : (x : carrier) -> op x x = x
  identity : (x : carrier) -> op neutral x = x
  associativitiy : (x, y, z : carrier) -> op x (op y z) = op (op x y) z

lubCommutativeMonoid : CommutativeMonoid Nat
lubCommutativeMonoid = MkCMon lubNat Z lub_commutes lub_idempotent lub_identity lub_assoc

--natCrdtLubCommutativeMonoid : CommutativeMonoid (Crdt Nat)
--natCrdtLubCommutativeMonoid = MkCMon mergeCrdt Z lub_commutes lub_idempotent lub_identity lub_assoc

