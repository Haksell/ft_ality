RUN := cabal run -v0 exe:ft_ality --
KEYBOARD_FILE := grammars/valid/debug.gmr
GAMEPAD_FILE := grammars/valid/debugamepad.gmr

help:
	@$(RUN) --help

run:
	@$(RUN) $(KEYBOARD_FILE)

debug:
	@$(RUN) $(KEYBOARD_FILE) --debug

gamepad:
	@$(RUN) $(GAMEPAD_FILE) --gamepad

debugamepad:
	@$(RUN) $(GAMEPAD_FILE) --gamepad --debug

install:
	cabal update && cabal install --overwrite-policy=always

clean:
	rm -rf build dist-newstyle .cabal* .ghc.environment* __pycache__ */__pycache__

loc:
	@find srcs -name '*.hs' | sort | xargs wc -l

.PHONY: help run debug gamepad debugamepad install clean loc