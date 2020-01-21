WABT = ~/git/wabt

all: l0_put_structure.wasm

%.wasm: %.wat
	mkdir -p ../../build/wam;$(WABT)/bin/wat2wasm $< -o ../../build/wam/$@
