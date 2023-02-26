module CrdtVectProof

-- mergeCrdt_vect_commutes : (xs, ys : (Vect n Nat)) -> mergeCrdt xs ys = mergeCrdt ys xs
-- mergeCrdt_vect_commutes [] [] = Refl
-- mergeCrdt_vect_commutes xs ys = rewrite mergeCrdt_vect_commutes xs ys in Refl

-- mergeCrdt_vect_idempotent : (xs : (Vect n Nat)) -> mergeCrdt xs xs = xs
-- mergeCrdt_vect_idempotent [] = Refl
-- mergeCrdt_vect_idempotent xs = rewrite mergeCrdt_vect_idempotent xs in Refl

-- mergeCrdt_vect_identity : {n : Nat} -> (xs : (Vect n Nat)) -> mergeCrdt (Data.Vect.replicate n Z) xs = xs
-- mergeCrdt_vect_identity [] = Refl
-- mergeCrdt_vect_identity xs = ?hole --Refl

-- mergeCrdt_vect_assoc : (xs, ys, zs : (Vect n Nat)) -> mergeCrdt xs (mergeCrdt ys zs) = mergeCrdt (mergeCrdt xs ys) zs
-- mergeCrdt_vect_assoc [] [] [] = Refl
-- mergeCrdt_vect_assoc xs ys zs = rewrite mergeCrdt_vect_assoc xs ys zs in Refl

vectNatCrdtCommutativeMonoid : {n : Nat} -> CommutativeMonoid (Vect n Nat)
vectNatCrdtCommutativeMonoid = MkCMon 
                                mergeCrdt
                                (Data.Vect.replicate n Z)
                                mergeCrdt_vect_commutes
                                mergeCrdt_vect_idempotent
                                mergeCrdt_vect_identity
                                mergeCrdt_vect_assoc