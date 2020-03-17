const fsp = require('fs').promises;

const util = require('utilities');
const ftb = require('file_to_bytes');

function testFTB() {

    fsp.readFile('../../build/engine/wam.wasm').then(response =>
        response.buffer
    ).then(bytes =>
        WebAssembly.instantiate(bytes, util.importObject)
    ).then(obj => {
        util.initialize_op_codes(obj);
        util.registerProgram(util.lookupIndicator('file_to_bytes', 2), ftb.createFileToBytesProgram());
        util.registerProgram(util.lookupIndicator('file_to_bytes1', 2), ftb.createFileToBytes1Program());
        util.runQuery(1, ftb.createFileToBytesQuery(), obj);
    });
}

testFTB();

