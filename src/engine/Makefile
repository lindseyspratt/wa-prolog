WABT = ~/git/wabt

all: wam.wasm

%.wasm: %.wat
	mkdir -p ../../build/engine;$(WABT)/bin/wat2wasm --debug-names $< -o ../../build/engine/$@

test:
	node testNode.js

ftb:
	node testFileToBytesNode.js