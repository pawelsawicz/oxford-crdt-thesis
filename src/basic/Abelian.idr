module Abelian

||| Sets equipped with a single binary operation that is associative, along with
||| a neutral element for that binary operation.  Must satisfy the following
||| laws:
|||
||| + Associativity of `<+>`:
|||     forall a b c, a <+> (b <+> c) == (a <+> b) <+> c
||| + Neutral for `<+>`:
|||     forall a, a <+> neutral == a
|||     forall a, neutral <+> a == a
||| + Commutativity of `<+>`:
|||     forall a b, a <+> b == b <+> a
public export
interface Monoid ty => Abelian ty where
    constructor MkAbelian

-- public export
-- Abelian a => Abelian b => Abelian (a, b) where
