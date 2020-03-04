WABT = ~/git/wabt

all: language0.wasm language1.wasm language2.wasm language3.wasm \
		language3a.wasm language3b.wasm language3c.wasm language3d.wasm language3e.wasm

%.wasm: %.wat
	mkdir -p ../../../build/tutorial/wam;$(WABT)/bin/wat2wasm --debug-names $< -o ../../../build/tutorial/wam/$@
