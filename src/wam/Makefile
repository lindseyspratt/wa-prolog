WABT = ~/git/wabt

all: language0.wasm

%.wasm: %.wat
	mkdir -p ../../build/wam;$(WABT)/bin/wat2wasm $< -o ../../build/wam/$@
