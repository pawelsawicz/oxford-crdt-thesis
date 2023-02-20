module ExperimentalSetReplica

import Data.Vect
import Data.List
import Data.Fin
import Data.Nat

interface Ord ty => Crdt ty where
    mergeCrdt : ty -> ty -> ty

Crdt Nat where
    mergeCrdt x y = case x `compare` y of
                            LT => y
                            GT => x
                            EQ => x

Crdt Bool where
    mergeCrdt x y = case x `compare` y of
                            LT => y
                            GT => x
                            EQ => x

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
record CommutativeMonoid (carrier : Type) where
  constructor MkCMon
  op : carrier -> carrier -> carrier
  commutative : (x, y : carrier) -> op x y = op y x -- the commutativity proof
--  neutral : carrier
--   identity : (x : carrier) -> op neutral x = x -- the identity proof
--   associativitiy : (x, y, z : carrier) -> op x (op y z) = op (op x y) z -- the associativitiy proof
--   commutative : (x, y : carrier) -> op x y = op y x -- the commutativity proof

partialOrdering : Nat -> Nat -> Nat
partialOrdering x y = case x `compare` y of
                            LT => y
                            GT => x
                            EQ => x

plusReduces : (n : Nat) -> Z + n = n
plusReduces n = Refl

plusReducesZ : (n : Nat) -> n = n + Z
plusReducesZ Z = Refl
plusReducesZ (S k) = cong S (plusReducesZ k)

-- plusCommute : (x, y : Nat) -> (x + y) = (y + x)
-- plusCommute x y = Refl

-- natCommutativeMonoid : CommutativeMonoid Nat
-- natCommutativeMonoid = MkCMon partialOrdering


-- interface CommutativeMonoid (carrier : Type) where
--   (<+>) : carrier -> carrier -> carrier
--   neutral : carrier