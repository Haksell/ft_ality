install:
	cabal update && cabal install

run:
	@cabal run -v0 exe:ft_ality --

clean:
	rm -rf dist-newstyle