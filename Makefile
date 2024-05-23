RUN := cabal run -v0 exe:ft_ality --
FILE := grammars/valid/debug.gmr

help:
	@$(RUN) --help

run:
	@$(RUN) $(FILE)

debug:
	@$(RUN) $(FILE) --debug

gui:
	@$(RUN) $(FILE) --gui

install:
	cabal update && cabal install --overwrite-policy=always

clean:
	rm -rf build dist-newstyle .cabal* .ghc.environment* __pycache__ */__pycache__

loc:
	@find srcs -name '*.hs' | sort | xargs wc -l