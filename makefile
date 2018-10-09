
install: 
	cabal configure --bindir=./bin --libexecdir=./bin --htmldir=./docs --docdir=./docs
	cabal build
	cabal haddock --executable
	cabal copy

clean:
	cabal clean
	rm -f -R ./bin ./docs