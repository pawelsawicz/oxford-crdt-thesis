module Verification.Proofs.CrdtLubElementWiseProof

import Core.Semilattice
import Verification.CommutativeMonoid

lub_commutes : (JoinSemilattice a) => (JoinSemilattice b) =>
  (x, y : (a, b)) -> lub x y = lub y x
lub_commutes (x1, y1) (x2, y2) = ?hole