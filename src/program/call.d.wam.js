const util = require('./utilities.d.wam.js');
/*
This file implements the WAM codes for a (silly) call/1 predicate.
This implementation is based on chapter 5 section 10, [AK99, pp.75-81].

S1{
call(X or Y) :-call(X).
call(trace) :- trace.
call(X or Y) :- call(Y).
call(notrace) :- notrace.
call(nl) :- nl.
}
S2{
call(X) :- builtin(X).
}
S3{
call(X) :- extern(X).
}
S4{
call(call(X)) :- call(X).
call(repeat).
call(repeat) :- call(repeat).
call(true).
}

Schema for compilation of call/1:

call/1  :   try_me_else S2
            indexed code for S1
S2      :   retry_me_else S3    % call(X)
            execute builtin/1   %   :- builtin(X)
S3      :   retry_me_else S4    % call(X)
            execute extern/1    %   :- extern(X)
S4      :   trust_me
            indexed code for S4


Schema for S1:

first level indexing for S1
second level indexing for S1
third level indexing for S1

S11     :   try_me_else S12
            code for ‘call(X or Y) :- call(X).’

S12     :   retry_me_else S13
            code for ‘call(trace) :- trace.’

S13     :   retry_me_else S14
            code for ‘call(X or Y) :- call(Y).’

S14     :   retry_me_else S15
            code for ‘call(notrace) :- notrace.’

S15     :   trust_me
            code for ‘call(nl) :- nl.’


 */

function callProgram() {
    return util.process_labels(util.flatten([
        util.opCodes.try_me_else, {ref: 'S2'},
        {codes: s1Base()},
        {label: 'S2'},
        util.opCodes.retry_me_else, {ref: 'S3'},
        util.opCodes.execute, util.lookupIndicator('builtin', 1),
        {label: 'S3'},
        util.opCodes.retry_me_else, {ref: 'S4'},
        util.opCodes.execute, util.lookupIndicator('extern', 1),
        {label: 'S4'},
        util.opCodes.trust_me,
        {codes: s4Base()}
    ]));
}

function s1Base() {
    return util.expand_hash([
        util.opCodes.switch_on_term, {ref: 'S11'}, {ref: 'C1'}, 0, {ref: 'F1'},
        {label: 'C1'},
        util.opCodes.switch_on_constant, 3, {hash: [
            util.lookup_atom('trace'), {ref: 'S1b'},
            util.lookup_atom('notrace'), {ref: 'S1d'},
            util.lookup_atom('nl'), {ref: 'S1c'}]},
        {label: 'F1'},
        util.opCodes.switch_on_structure, 1, {hash: [
            util.lookupIndicator('or',2), {ref: 'F11'}]},
        {label: 'F11'},
        util.opCodes.try, {ref: 'S1a'},
        util.opCodes.trust, {ref: 'S1c'},
        {label: 'S11'},
        util.opCodes.try_me_else, {ref: 'S12'},
        {label: 'S1a'},
        util.opCodes.get_structure, util.lookupIndicator('or', 2), 1,
        util.opCodes.unify_variable, 1,
        util.opCodes.unify_void, 1,
        util.opCodes.execute, util.lookupIndicator('call', 1),

        {label: 'S12'},
        util.opCodes.retry_me_else, {ref: 'S13'},
        {label: 'S1b'},
        util.opCodes.get_constant, util.lookup_atom('trace'), 1,
        util.opCodes.execute, util.lookupIndicator('trace', 0),

        {label: 'S13'},
        util.opCodes.retry_me_else, {ref: 'S14'},
        {label: 'S1c'},
        util.opCodes.get_structure, util.lookupIndicator('or', 2), 1,
        util.opCodes.unify_void, 1,
        util.opCodes.unify_variable, 1,
        util.opCodes.execute, util.lookupIndicator('call', 1),

        {label: 'S14'},
        util.opCodes.retry_me_else, {ref: 'S15'},
        {label: 'S1d'},
        util.opCodes.get_constant, util.lookup_atom('notrace'), 1,
        util.opCodes.execute, util.lookupIndicator('notrace', 0),

        {label: 'S15'},
        util.opCodes.trust_me,
        {label: 'S1e'},
        util.opCodes.get_constant, util.lookup_atom('nl'), 1,
        util.opCodes.execute, util.lookupIndicator('nl', 0)
    ]);
}

function s4Base() {
    return util.expand_hash([
        {label: 'S4'},
        util.opCodes.switch_on_term, {ref: 'S41'}, {ref: 'C4'}, 0, {ref: 'F4'},
        {label: 'C4'},
        util.opCodes.switch_on_constant, 2, {hash:
            [util.lookup_atom('repeat'), {ref: 'C41'},
            util.lookup_atom('true'), {ref: 'S4d'}]},
        {label: 'F4'},
        util.opCodes.switch_on_structure, 1,
            {hash: [util.lookupIndicator('call',1), {ref: 'S41'}]},
        {label: 'C41'},
        util.opCodes.try, {ref: 'S4b'},
        util.opCodes.trust, {ref: 'S4c'},
        {label: 'S41'},
        util.opCodes.try_me_else, {ref: 'S42'},
        {label: 'S4a'},
        util.opCodes.get_structure, util.lookupIndicator('call',1), 1,
        util.opCodes.unify_variable, 1,
        util.opCodes.execute, util.lookupIndicator('call', 1),
        {label: 'S42'},
        util.opCodes.retry_me_else, {ref: 'S43'},
        {label: 'S4b'},
        util.opCodes.get_constant, util.lookup_atom('repeat'),1,
        util.opCodes.proceed,
        {label: 'S43'},
        util.opCodes.retry_me_else, {ref: 'S44'},
        {label: 'S4c'},
        util.opCodes.get_constant, util.lookup_atom('repeat'),1,
        util.opCodes.put_constant, util.lookup_atom('repeat'),1,
        util.opCodes.execute, util.lookupIndicator('call', 1),
        {label: 'S44'},
        util.opCodes.trust_me,
        {label: 'S4d'},
        util.opCodes.get_constant, util.lookup_atom('true'),1,
        util.opCodes.proceed
    ]);
}

function builtinProgram() {
    // builtin(b).
    return [
        util.opCodes.get_constant, util.lookup_atom('b'), 1,
        util.opCodes.proceed
    ];
}

function externProgram() {
    // extern(e).
    return [
        util.opCodes.get_constant, util.lookup_atom('e'), 1,
        util.opCodes.proceed
    ];
}

function queryTrue() {
    return [
        util.opCodes.put_constant, util.lookup_atom('true'), 1,
        util.opCodes.execute, util.lookupIndicator('call', 1)
    ];
}

module.exports.callProgram = callProgram;
module.exports.builtinProgram = builtinProgram;
module.exports.externProgram = externProgram;
module.exports.queryTrue = queryTrue;