# Author: Viktor Miroshnikov

sandbox:
	cabal sandbox init 
	cabal update

hackage-deps:
	cabal install --only-dependencies

deps: hackage-deps

bootstrap: sandbox deps 

build: 
	cabal build

clean:
	rm -rf .cabal-sandbox cabal.sandbox.config dist
