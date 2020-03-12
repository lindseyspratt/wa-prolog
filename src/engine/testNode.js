const fsp = require('fs').promises;

const util = require('utilities');
const testOps = require('testOps');

function testOp() {

    fsp.readFile('../../build/engine/wam.wasm').then(response =>
        response.buffer
    ).then(bytes =>
        WebAssembly.instantiate(bytes, util.importObject)
    ).then(obj => {
        util.initialize_op_codes(obj);
        let tests = testOps.create_ops();
        for(let i = 0;i < tests.length;i++) {
            console.log('\n===');
            console.log('Start ' + i);
            let test = tests[i];
            let info;
            if(typeof test === 'object' && test.seq ) {
                let query = test.seq.concat([util.opCodes.halt]);
                info = util.runQuery(1, query, obj);
            } else {
                info = util.runOp(test, obj);
            }

            util.log_results( i + ' results', 16, info);
        }
    });
}

testOp();

