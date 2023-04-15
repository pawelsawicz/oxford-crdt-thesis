build-samples:
	idris2 --build sample/sample-crdt.ipkg

build-verified:
	idris2 --build verified_crdt.ipkg

build-lib:
	idris2 --build crdt.ipkg

open-test:
	pack --with-ipkg sample_crdt.ipkg repl src/Sample/PropEx.idr