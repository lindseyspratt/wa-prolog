WABT = ~/git/wabt

all: test_add.wasm test_add_call.wasm test_import.wasm test_import_result.wasm test_memory.wasm\
	test_shared0.wasm test_shared1.wasm test_table.wasm

%.wasm: %.wat
	mkdir -p ../../build/test;$(WABT)/bin/wat2wasm $< -o ../../build/test/$@
