help:
	@echo TODO

install:
	cabal update && cabal install --overwrite-policy=always

run:
	@cabal run -v0 exe:ft_ality -- grammars/mk9.gmr

clean:
	rm -rf build dist-newstyle .cabal* .ghc.environment*