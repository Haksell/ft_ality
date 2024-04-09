help:
	@echo TODO

install:
	cabal update && cabal install --overwrite-policy=always

run:
	@cabal run -v0 exe:ft_ality -- 

clean:
	rm -rf build dist-newstyle .cabal*