WABT = ~/git/wabt

all: language0.wasm language1.wasm

%.wasm: %.wat
	mkdir -p ../../build/wam;$(WABT)/bin/wat2wasm --debug-names $< -o ../../build/wam/$@
