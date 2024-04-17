help:
	@cabal run -v0 exe:ft_ality -- --help

run:
	@cabal run -v0 exe:ft_ality -- grammars/valid/mk9.gmr

install:
	cabal update && cabal install --overwrite-policy=always

clean:
	rm -rf build dist-newstyle .cabal* .ghc.environment* __pycache__ */__pycache__