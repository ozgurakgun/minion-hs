.PHONY: install clean

install: .cabal-sandbox
	cabal install -O2

clean:
	rm -rf cabal.sandbox.config .cabal-sandbox dist

.cabal-sandbox:
	cabal sandbox init

