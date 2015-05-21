.PHONY: build populate_constraints clean

build:
	cabal sandbox init
	cabal install -O2

populate_constraints:
	scripts/populate_constraints.sh

clean:
	rm -rf cabal.sandbox.config .cabal-sandbox dist

