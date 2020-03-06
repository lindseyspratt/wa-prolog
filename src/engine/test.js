const fsp = require('fs').promises;

const util = require('./utilities.wam.js');

function create_program_p_1() {

    util.validate_op_codes();

    return util.process_labels([
        util.opCodes.get_atom, util.lookup_atom('a'), 1,
        util.opCodes.proceed
    ]);
}

function create_ops() {
    util.validate_op_codes();

    util.registerProgram(util.lookupIndicator("p", 1), create_program_p_1());

    return [
        [util.opCodes.nop],                                                         // nop
        [util.opCodes.put_structure, util.lookupIndicator("f", 1), 1],  // put_structure, f/1, A1
        [util.opCodes.set_variable, 1],                                             // setVariable, A1
        [util.opCodes.put_variable, 0, 1, 1],                                       // put_variable X1,A1
        [util.opCodes.get_variable, 0, 2, 1],                                       // getVariable Xtype, temporary target register ID 2, source (argument temporary) register ID
        [util.opCodes.set_value, 1],                                                // setValue 1
        [util.opCodes.put_value, 0, 2, 1],                                          // putValue Xtype, temporary target register ID 2, source (argument temporary) register ID
        {seq:[util.opCodes.set_variable, 1,
              util.opCodes.get_value, 1, 2]},                                       // getValue X1, A2
        [util.opCodes.get_structure, util.lookupIndicator("f", 1), 1],  // getStructure f/1, A1
        [util.opCodes.unify_variable, 1],                                           // unifyVariable, X1
        [util.opCodes.unify_value, 1],                                              // unifyValue, X1
        [util.opCodes.call, util.lookupIndicator("p", 1), 0],           // call p/1, 0
        [util.opCodes.proceed],                                                     // proceed
        [util.opCodes.allocate],                                                    // allocate
        {seq:[util.opCodes.allocate,
                util.opCodes.deallocate]},                                          // deallocate
        // [util.opCodes.try_me_else, 1],                                           // try_me_else
        // [util.opCodes.retry_me_else, 1],                                         // retry_me_else
        // [util.opCodes.trust_me],                                                 // trust_me
        [util.opCodes.put_atom, util.lookup_atom("a"), 1],                    // put_atom 'a',1
        [util.opCodes.put_integer, 1, 1],                                           // put_integer 1,1
        [util.opCodes.put_float, util.lookup_float(1.2), 1],                  // put_float 1.2,1
        {seq:[util.opCodes.set_variable, 1,
                util.opCodes.get_atom, util.lookup_atom('a'), 1]},            // get_atom 'a',1
        {seq:[util.opCodes.set_variable, 1,
                util.opCodes.get_integer,5, 1]},                                    // get_integer 5,1
        {seq:[util.opCodes.set_variable, 1,
                util.opCodes.get_float, util.lookup_float(1.2), 1]},          // get_float 1.2,1
        [util.opCodes.set_atom, util.lookup_atom("a")],                       // set_atom 'a'
        [util.opCodes.set_integer, 1],                                              // set_integer 1
        [util.opCodes.set_float, util.lookup_float(1.2)],                     // set_float 1.2
        {seq:[util.opCodes.set_variable, 1,
              util.opCodes.get_structure, util.lookupIndicator("f", 1), 1,
                util.opCodes.unify_atom, util.lookup_atom('a')]},            // unify_atom 'a',1
        {seq:[util.opCodes.set_variable, 1,
                util.opCodes.get_structure, util.lookupIndicator("f", 1), 1,
                util.opCodes.unify_integer,5]},                                    // unify_integer 5,1
        {seq:[util.opCodes.set_variable, 1,
                util.opCodes.get_structure, util.lookupIndicator("f", 1), 1,
                util.opCodes.unify_float, util.lookup_float(1.2)]},          // unify_float 1.2,1
        [util.opCodes.put_list, 1],                                                 // put_list 1
        {seq:[util.opCodes.set_variable, 1,
                util.opCodes.get_list, 1]},                                         // get_list 1
        [util.opCodes.set_void, 3],                                                 // set_void 3; set voids (anonymous variables) in top three heap slots.
        {seq:[util.opCodes.set_variable, 1,
                util.opCodes.get_structure, util.lookupIndicator("f", 1), 1,
                util.opCodes.unify_void,1]},                                        // unify_void 1
        [util.opCodes.execute, util.lookupIndicator("p", 1)],           // execute p/1
        {seq: [util.opCodes.put_atom, util.lookup_atom("a"), 1,
                util.opCodes.get_variable, 1, 2, 1,
                util.opCodes.put_unsafe_value, 2, 3]},                              // put_unsafe_value Y1,A1
        {seq: [util.opCodes.put_atom, util.lookup_atom("a"), 1,
                util.opCodes.get_variable, 1, 2, 1,
                util.opCodes.set_local_value, 1, 2]},                              // set_local_value Y2
        {seq:[util.opCodes.put_atom, util.lookup_atom("a"), 1,
                util.opCodes.get_variable, 1, 2, 1,
                util.opCodes.set_variable, 2,
                util.opCodes.get_structure, util.lookupIndicator("f", 1), 2,
                util.opCodes.unify_local_value, 1, 2]},                             // unify_local_value Y2
        // [util.opCodes.try, 1],                                                   // try 1
        // [util.opCodes.retry, 1],                                                 // retry 1
        // [util.opCodes.trust, 1],                                                 // trust 1
        // util.opCodes.switch_on_term,
        // util.opCodes.switch_on_constant,
        // util.opCodes.switch_on_structure
        // util.opCodes.neck_cut,
        // util.opCodes.get_level,
        // util.opCodes.cut,
        [util.opCodes.halt],                                                        // halt
    ];
}


function testOp() {

    fsp.readFile('../../build/engine/wam.wasm').then(response =>
        response.buffer
    ).then(bytes =>
        WebAssembly.instantiate(bytes, util.importObject)
    ).then(obj => {
        util.initialize_op_codes(obj);
        let tests = create_ops();
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

