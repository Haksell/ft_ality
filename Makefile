RUN := cabal run -v0 exe:ft_ality --
KEYBOARD_FILE := grammars/valid/debug.gmr
GAMEPAD_FILE := grammars/valid/debugamepad.gmr

help:
	@$(RUN) --help

run:
	@$(RUN) $(KEYBOARD_FILE)

gamepad:
	@$(RUN) $(GAMEPAD_FILE) --gamepad

gamepadebug:
	@$(RUN) $(GAMEPAD_FILE) --gamepad --debug

debug:
	@$(RUN) $(KEYBOARD_FILE) --debug

gui:
	@$(RUN) $(KEYBOARD_FILE) --gui

install:
	cabal update && cabal install --overwrite-policy=always

clean:
	rm -rf build dist-newstyle .cabal* .ghc.environment* __pycache__ */__pycache__

loc:
	@find srcs -name '*.hs' | sort | xargs wc -l

.PHONY: help run gamepad debug gui install clean loc