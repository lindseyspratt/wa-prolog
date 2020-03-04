// noinspection NodeJsCodingAssistanceForCoreModules
const fs = require('fs');
const nrev = require('./nrev.wam.js');
const util = require('./utilities.wam.js');

const buf = fs.readFileSync('build/wam/language3a.wasm');

// noinspection ES6ModulesDependencies
WebAssembly.instantiate(new Uint8Array(buf), util.importObject)
    .then(res => {
        util.initialize_op_codes(res);
        console.log(JSON.stringify(util.opCodes));
        console.log(JSON.stringify(nrev.nrevProgram()));
        util.registerProgram(util.lookupIndicator('nrev', 2), nrev.nrevProgram());

        console.log(JSON.stringify(nrev.appendProgram()));
        util.registerProgram(util.lookupIndicator('append', 3), nrev.appendProgram());

        console.log(JSON.stringify(nrev.query1()));
        util.runQuery('nrev', nrev.query1(), res);
    });
