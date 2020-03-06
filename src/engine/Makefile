WABT = ~/git/wabt

all: wam.wasm

%.wasm: %.wat
	mkdir -p ../../build/engine;$(WABT)/bin/wat2wasm --debug-names $< -o ../../build/engine/$@
