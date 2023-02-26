module Verification.CommutativeMonoid

%default total

public export
record CommutativeMonoid (carrier : Type) where
  constructor MkCMon
  op : carrier -> carrier -> carrier
  neutral : carrier
  commutative : (x, y : carrier) -> op x y = op y x
  idempotent : (x : carrier) -> op x x = x
  identity : (x : carrier) -> op neutral x = x
  associativitiy : (x, y, z : carrier) -> op x (op y z) = op (op x y) z