let table = new WebAssembly.Table({initial: 100, element: "anyfunc"});
let memory = new WebAssembly.Memory({initial: 1});
let atable = [];
let code = [];
let indicators = [];
let programs = [];

/*
memory layout: The single memory space is an array of words.
Before section 5.8.1: The lowest addresses (1 to REGISTER_SIZE) are the Registers.
The layout in order from lowest addresses to highest was: Registers, PDL, Stack, Trail, Heap.

To make variable binding management the most efficient and still correct, there are WAM binding rules
from [AK99, p.63]:
WAM BINDING RULE 1: Always make the variable of higher address reference that of lower address.
WAM BINDING RULE 2: Heap variables must never be set to a reference into the stack.
WAM BINDING RULE 3: The stack must be allocated at higher addresses than the heap, in the same global address space.
WAM BINDING RULE 4: "...we may assume that X registers conveniently reside at the highest end of the global store." [AK99, p.66, footnote 5].

Since the WAM registers in this implementation are actually in the memory array,
they should be located at the highest end as specified in rule 4.

After sections 5.8.1 and 5.8.2: PDL, Trail, Heap, Stack, Registers
 */
const PDL_SIZE = 256;
const TRAIL_SIZE = 1024;
const STACK_SIZE = 4096;
const REGISTER_SIZE = 256;
const HEAP_SIZE = Math.trunc(memory.buffer.byteLength/4) - (PDL_SIZE + TRAIL_SIZE + STACK_SIZE + REGISTER_SIZE);

const MIN_PDL = 0;
const PDL_START = MIN_PDL * 4;

const MIN_TRAIL = MIN_PDL + PDL_SIZE + 1;
const TRAIL_START = (MIN_TRAIL*4);

const MIN_HEAP = MIN_TRAIL + TRAIL_SIZE + 1;
const HEAP_START = (MIN_HEAP*4);

const MIN_STACK = MIN_HEAP + HEAP_SIZE + 1;
const STACK_START = (MIN_STACK*4);

const MIN_REGISTER = MIN_STACK + STACK_SIZE + 1;
const REGISTER_START = (MIN_REGISTER*4);

let importObject = {js:
        {mem: memory,
            table: table,
            minRegister: MIN_REGISTER,
            registerSize: REGISTER_SIZE,
            registerStart: REGISTER_START,
            minPDL: MIN_PDL,
            pdlStart: PDL_START,
            pdlSize: PDL_SIZE,
            minStack: MIN_STACK,
            stackStart: STACK_START,
            stackSize: STACK_SIZE,
            minTrail: MIN_TRAIL,
            trailStart: TRAIL_START,
            trailSize: TRAIL_SIZE,
            minHeap: MIN_HEAP,
            heapStart: HEAP_START,
            heapSize: HEAP_SIZE,
            lookupAtom: lookupAtomWA,
            getCodeFromProgram: getCodeFromProgramWA,
            getCode: getCodeWA,
            setCode: setCodeWA,
            getIndicatorArity: getIndicatorArityWA,
            traceInstLog0: traceInstLog0,
            traceInstLog1: traceInstLog1,
            traceInstLog2: traceInstLog2,
            traceInstLog3: traceInstLog3,
            traceInstLog4: traceInstLog4,
            traceInstSwitchLog: traceInstSwitchLog,
            traceStoreZero: traceStoreZero,
            traceDerefZero: traceDerefZero,
            traceStoreTrailToReg: traceStoreTrailToReg,
            traceStore: traceStore,
            warnMaxStack: warnMaxStack,
            warnMaxTrail: warnMaxTrail,
            warnInvalidMemoryLayout: warnInvalidMemoryLayout,
            warnInvalidSwitchTag: warnInvalidSwitchTag,
        }};

function lookupAtomWA(start, length){
    var bytes = new Uint8Array(memory.buffer, start, length);
    var string = new TextDecoder('utf8').decode(bytes);
    return lookup_atom(string);
}

function getCodeFromProgramWA(codeOfst, indicator) {
    let codes = programs[indicator];
    return codes[codeOfst];
}

let currentWord;
function getCodeWA(codeOfst) {
    currentWord = codeOfst;
    return code[codeOfst];
}

let currentIndicator;
function setCodeWA(indicator) {
    currentIndicator = indicator;
    code = programs[indicator];
}

function getIndicatorArityWA(indicator) {
    return indicators[indicator][1];
}

function lookup_atom(name)
{
    if(typeof name === 'number') {
        name = Number(name).toString();
    }

    if(typeof name !== 'string') {
        throw 'invalid lookup_atom. name must have type of string, but is ' + typeof name + '. name = ' + name;
    }

    var i;
    for (i = 0; i < atable.length; i++)
    {
        if (atable[i] === name)
            return i;
    }
    i = atable.length;
    atable[i] = name;
    return i;
}

function find_atom(internalID) {
    return atable[internalID];
}

function lookupIndicator(name, arity) {
    let nameID = lookup_atom(name);

    let i;
    for (i = 0; i < indicators.length; i++) {
        if (indicators[i][0] === nameID
            && indicators[i][1] === arity) {
            console.log(name + '/' + arity + ' found ' + i);
            return i;
        }
    }
    i = indicators.length;
    indicators[i] = [nameID, arity];
    console.log(name + '/' + arity + ' created ' + i);
    return i;
}

function indicator_to_string(indicator) {
    let pair = indicators[indicator];
    let functorName = find_atom(pair[0]);
    return functorName + '/' + pair[1];
}

function registerProgram(indicator, code) {
    programs[indicator] = code;
}

// create two level hash table from list of key/value pairs.
// level 1: [B0, ..., Bn-1] where Bi is the offset of
// bucket I.
// level 2: [Vi0, ..., Vi(m-1)] where Vij is a value in bucket i. These values are *not* ordered (yet).
// mask = n-1;
// i = integer(mask & value)

let hashCounter = 0; // the hashCounter ensures that all hash table labels are distinct.

function expand_hash(program) {
    let result = [];
    for(let i = 0;i < program.length; i++) {
        if(typeof program[i] === 'object' && program[i].hash) {
            let table = program[i].hash;
            let mask = 3; // 4 bucket hash; buckets 0 through 3.
            let buckets = [[],[],[],[]];
            for(let k = 0;k < table.length;k+=2) {
                let key = table[k];
                let value = table[k+1];
                let bucketID = key & mask;
                let bucket = buckets[bucketID];
                bucket.push(key);
                bucket.push(value);
            }

            // put bucket hash level 1 in result.
            let labels = [];
            for(let j = 0;j < buckets.length;j++) {
                if(buckets[j].length > 0) {
                    labels[j] = 'Hash' + hashCounter++;
                    result.push({ref: labels[j]});
                } else {
                    result.push(0);
                }
            }

            // put bucket hash level 2 in result.
            for(let j = 0;j < buckets.length;j++) {
                let bucket = buckets[j];
                if(bucket.length > 0) {
                    result.push({label: labels[j]});
                    result.push(bucket.length / 2); // size of table is the number of key/value pairs, which is half the length of the bucket.
                    for (let m = 0; m < bucket.length; m++) {
                        result.push(bucket[m]);
                    }
                }
            }
        } else {
            result.push(program[i]);
        }
    }

    return result;
}

function flatten(program) {
    let result = [];
    for(let i = 0;i < program.length; i++) {
        if(typeof program[i] === 'object' && program[i].codes) {
            let subCodes = flatten(program[i].codes);
            for(let k = 0;k < subCodes.length;k++) {
                result.push(subCodes[k]);
            }
        } else {
            result.push(program[i]);
        }
    }
    return result;
}
function process_labels(program) {
    let map = {};
    let result = [];
    let j = 0;
    // find labels
    for(let i = 0;i < program.length; i++) {
        if(typeof program[i] === 'object') {
            if(program[i].label) {
                map[program[i].label] = j;
                continue; // skip to next word after label word.
            }
        }
        result[j] = program[i];
        j++;
    }

    // replace label references
    for(let k = 0;k < result.length; k++) {
        if(typeof result[k] === 'object') {
            if(result[k].ref) {
                result[k] = map[result[k].ref];
            }
        }
    }

    return result;
}

function runQuery(limit, queryCode, obj) {
    let inferences = 0;
    let i32Initial = new Uint32Array(memory.buffer);
    for(let i = 0; i < MIN_HEAP + 1000;i++) {
        i32Initial[i] = 0;
    }
    let start = Date.now();
    for(let i = 0;i < limit;i++) {
        code = queryCode;
        obj.instance.exports.run(0);
        inferences += obj.instance.exports.getInferences();
    }
    let end = Date.now();
    let total = (end - start);
    return {duration:total, inferences:inferences};
}

let opCodes = {};

function initialize_op_codes(obj) {
    opCodes.nop = obj.instance.exports.nop_opcode();
    opCodes.put_structure = obj.instance.exports.put_structure_opcode();
    opCodes.get_structure = obj.instance.exports.get_structure_opcode();
    opCodes.set_variable = obj.instance.exports.set_variable_opcode();
    opCodes.put_variable = obj.instance.exports.put_variable_opcode();
    opCodes.get_variable = obj.instance.exports.get_variable_opcode();
    opCodes.set_value = obj.instance.exports.set_value_opcode();
    opCodes.put_value = obj.instance.exports.put_value_opcode();
    opCodes.get_value = obj.instance.exports.get_value_opcode();
    opCodes.unify_variable = obj.instance.exports.unify_variable_opcode();
    opCodes.unify_value = obj.instance.exports.unify_value_opcode();
    opCodes.call = obj.instance.exports.call_opcode();
    opCodes.proceed = obj.instance.exports.proceed_opcode();
    opCodes.allocate = obj.instance.exports.allocate_opcode();
    opCodes.deallocate = obj.instance.exports.deallocate_opcode();
    opCodes.try_me_else = obj.instance.exports.try_me_else_opcode();
    opCodes.retry_me_else = obj.instance.exports.retry_me_else_opcode();
    opCodes.trust_me = obj.instance.exports.trust_me_opcode();
    opCodes.put_constant = obj.instance.exports.put_constant_opcode();
    opCodes.get_constant = obj.instance.exports.get_constant_opcode();
    opCodes.set_constant = obj.instance.exports.set_constant_opcode();
    opCodes.unify_constant = obj.instance.exports.unify_constant_opcode();
    opCodes.put_list = obj.instance.exports.put_list_opcode();
    opCodes.get_list = obj.instance.exports.get_list_opcode();
    opCodes.set_void = obj.instance.exports.set_void_opcode();
    opCodes.unify_void = obj.instance.exports.unify_void_opcode();
    opCodes.execute = obj.instance.exports.execute_opcode();
    opCodes.put_unsafe_value = obj.instance.exports.put_unsafe_value_opcode();
    opCodes.set_local_value = obj.instance.exports.set_local_value_opcode();
    opCodes.unify_local_value = obj.instance.exports.unify_local_value_opcode();
    opCodes.try = obj.instance.exports.try_opcode();
    opCodes.retry = obj.instance.exports.retry_opcode();
    opCodes.trust = obj.instance.exports.trust_opcode();
    opCodes.switch_on_term = obj.instance.exports.switch_on_term_opcode();
    opCodes.switch_on_constant = obj.instance.exports.switch_on_constant_opcode();
    opCodes.switch_on_structure = obj.instance.exports.switch_on_structure_opcode();
}

function validate_op_codes() {
    if (typeof opCodes.put_structure === 'undefined') {
        alert('opCodes not initialized.');
    }

    for(let i = 0;i < opCodes.length;i++) {
        if (typeof opCodes[i] !== 'number') {
            alert('opCode[' + i + '] not a number.');
        }
    }
}


function getOpCodeName(opCode) {
    let keys = Object.keys(opCodes);
    for(let i = 0;i < keys.length;i++) {
        let key = keys[i];
        if(opCodes[key] === opCode) {
            return key;
        }
    }
    return 'undefined';
}


function display_results(test, length, stats) {
    let results = new Uint32Array(memory.buffer);
    let element1 = document.getElementById('testName');
    element1.innerText = 'Test: ' + test;
    let elementStats = document.getElementById('stats');
    elementStats.innerText = JSON.stringify(stats);
    let element2 = document.getElementById('result');
    element2.innerText = 'startingWord=' + MIN_HEAP + ': ' + JSON.stringify(results.slice(MIN_HEAP, MIN_HEAP+length));
    let element3 = document.getElementById('resultInterpretationRegisters');
    element3.innerText = 'registers: ' + interpret_memory(results, MIN_REGISTER, MIN_REGISTER+length);
    let element4 = document.getElementById('resultInterpretationHeap');
    element4.innerText = 'heap: ' + interpret_memory(results, MIN_HEAP, MIN_HEAP+length);
}

// function console_results(test, results, startingWord, length) {
//     console.log('Test: ' + test);
//     console.log('startingWord=' + startingWord + ': ' + JSON.stringify(results.slice(startingWord, startingWord+length)));
//     console.log('registers: ' + interpret_memory(results, 1, 16));
//     console.log('heap: ' + interpret_memory(results, startingWord));
// }

function interpret_memory(results, startingWord, explicitLength) {
    let output = '';
    let i = startingWord;
    while(i < results.length) {
        let info = interpret_memory_item(results, i);
        if((!explicitLength && info.string === '_0')||(explicitLength && i >= explicitLength)) {
            // fake variable reference
            break;
        }
        output += i + ': ' + info.string + '; ';
        i = info.nextItemOfst;
    }
    return output;
}
function interpret_memory_item(results, itemOfst) {
    let word = results[itemOfst];
    if(word === 0) {
        return {string:'invalid address 0', nextItemOfst:itemOfst+1};
    }
    let tag = get_tag(word);
    switch(tag) {
        case TAG_REF: //var/REFerence
        {
            let string = '_' + get_val(word);
            if(string === '_0') {
                return {string: string, nextItemOfst: itemOfst+1};
            }
            let term = results[get_val(word)];
            if(term === 0) {
                string += "->_0";
            } else if(term !== word) {
                let stringInfo = interpret_memory_item(results, get_val(word));
                string = "->" + stringInfo.string;
            }
            return {string: string, nextItemOfst: itemOfst+1}; // variable represented by '_N' where N is an arbitrary integer.
        }
        case TAG_PRE: //PREdicate = f/n
        {
            let indicator = get_val(word);
            let result = '';
            result += '{' + itemOfst + '} ';
            return interpret_memory_structure(results, result, indicator, 0, itemOfst);
        }
        case TAG_STR: //STRucture
        {
            // STR value is address of indicator word, which is immediately followed by arity(indicator) argument words.
            // Each argument word is a (tagged) term word
            // structure = f(a1, ..., an)
            // The indicator word may directly follow the 'structure' word, or it may be elsewhere.
            // If it is directly after the structure word, then the intrepret_memory processing will skip
            // to the end of the structure to continue to the next item.
            let result = '';
            let structureWordOfst = get_val(word);
            result += '{' + structureWordOfst + '} ';
            let indicatorTerm = results[structureWordOfst];
            if(get_tag(indicatorTerm) !== TAG_PRE){
                return {string: 'unrecognized: ' + word + ' (tag = ' + get_tag(indicatorTerm) + ', val = ' + get_val(indicatorTerm) + ') at ' + structureWordOfst,
                    nextItemOfst: itemOfst+1};
            }
            return interpret_memory_structure(results, result, get_val(indicatorTerm), structureWordOfst, itemOfst);
        }
        case TAG_LIS: //LISt
        {
            // LIS value is address of head term word, which is immediately followed by tail term word.
            // list = [head|tail]
            // The head word may directly follow the 'list' word, or it may be elsewhere.
            // If it is directly after the list word, then the intrepret_memory processing will skip
            // to the end of the list to continue to the next item.
            // <LIS,i>, i:Term, i+1:Tail:<LIS,i+2>, i+2:Term, i+3:Tail2:<LIS,i+4>, ..., i+2*n:Tailn:'[]'
            let result = '';
            let initialListOfst = get_val(word);
            result += '{' + initialListOfst + '}';
            let finalListOfst = itemOfst;
            let items = '';
            let listOrTail = word;
            let tailOfst;
            while(get_tag(listOrTail) === TAG_LIS) {
                let headOfst = get_val(listOrTail);
                if(headOfst === finalListOfst+2) {
                    finalListOfst += 2;
                }
                let info = interpret_memory_item(results, headOfst);
                if(items !== '') {
                    items += ', ';
                }
                items += info.string;
                tailOfst = headOfst+1;
                listOrTail = results[tailOfst];
            }

            let next;
            if(get_tag(listOrTail) === TAG_CON && lookup_atom('[]') === get_val(listOrTail)) {
                result = '[' + items + ']';
                if(tailOfst === finalListOfst+1) {
                    next = tailOfst + 1;
                } else {
                    next = finalListOfst+1;
                }
            } else {
                let info = interpret_memory_item(results, tailOfst);
                result = '[' + items + '|' + info.string + ']';
                if(tailOfst === finalListOfst+1) {
                    next = info.nextItemOfst;
                } else {
                    next = finalListOfst+1;
                }
            }

            return {string: result, nextItemOfst: next};
        }
        case TAG_CON: // CONstant: 'a' instead of a/0.
        {
            let internalID = get_val(word);
            let constant = find_atom(internalID);
            let result = '';
            result +=  "'" + constant + "'";
            return {string: result, nextItemOfst: itemOfst+1};
        }
        default:
        {
            return {string: 'unrecognized: ' + word + ' (tag = ' + tag + ', val = ' + get_val(word) + ') at ' + itemOfst,
                nextItemOfst: itemOfst+1};
        }
    }
}

function interpret_memory_structure(results, result, indicator, structureWordOfst, itemOfst) {
    let pair = indicators[indicator];
    let functorID = pair[0];
    result += atable[functorID] + '(';
    let arity = pair[1];
    for(let argOfst = 0;argOfst < arity;argOfst++) {
        let argInfo = interpret_memory_item(results, structureWordOfst + 1 + argOfst);
        if(argOfst > 0) {
            result += ', ';
        }

        if(argInfo.nextItemOfst !== structureWordOfst + 1 + argOfst + 1) {
            throw 'invalid structure words';
        }

        result += argInfo.string;
    }
    result += ')';
    let nextOfst = (structureWordOfst === 0 || structureWordOfst === itemOfst+1) ? itemOfst + arity + 1 : itemOfst + 1;
    return  {string: result, nextItemOfst: nextOfst}
}

function traceInstPrefix(argCount) {
    return 'inst: ' + currentIndicator + '.' + (currentWord-argCount) + ' ';
}

function traceInstLog0 (opCode) {
    let opName = getOpCodeName(opCode);
    console.log(traceInstPrefix(0) + opName + ";");
}

function traceInstLog1 (opCode, arg1) {
    let opName = getOpCodeName(opCode);
    console.log(traceInstPrefix(1) + opName + " " + arg1 + ";");
}

function traceInstLog2 (opCode, arg1, arg2) {
    let opName = getOpCodeName(opCode);
    console.log(traceInstPrefix(2) + opName + " " + arg1 + ", " + arg2 + ";");
}

function traceInstLog3 (opCode, arg1, arg2, arg3) {
    let opName = getOpCodeName(opCode);
    console.log(traceInstPrefix(3) + opName + " " + arg1 + ", " + arg2 + ", " + arg3 + ";");
}

function traceInstLog4 (opCode, arg1, arg2, arg3, arg4) {
    let opName = getOpCodeName(opCode);
    console.log(traceInstPrefix(4) + opName + " " + arg1 + ", " + arg2 + ", " + arg3 + ", " + arg4 + ";");
}

function traceInstSwitchLog (codeOfst, programOfst) {
    // codeOfst + 1 = N, size
    // codeOfst + 2 = T, table
    let result = '';
    let codes = programs[programOfst];
    let inst = codes[codeOfst];
    let size = codes[codeOfst+1];
    let instName = getOpCodeName(inst);
    let tableOfst = codeOfst + 2;

    // 4-bucket hash two levels: level 1 bucket locations, level 2 non-empty buckets.
    let bucketLocations = [];
    for(let i = 0;i < 4;i++) {
        bucketLocations.push(codes[tableOfst+i]);
    }

    for(let i = 0;i < bucketLocations.length;i++) {
        let bucketLocation = bucketLocations[i];
        if(bucketLocation !== 0) {
            let size = codes[bucketLocation]; // size is the number of key/value pairs in the bucket. There are size*2 items in the bucket: [key1, value1, key2, value2, ..., keySize, valueSize]
            result += i + '(' + bucketLocation + ')[';
            let localResult = '';
            for(let j = bucketLocation+1;j < bucketLocation+1+size*2;j+=2) {
                let key = codes[j];
                let value = codes[j+1];
                if(localResult !== '') {
                    localResult += ', ';
                }
                let keyName;

                if (instName === 'switch_on_constant') {
                    keyName = find_atom(key);
                } else {
                    // must be switch_on_structure
                    keyName = indicator_to_string(key);
                }
                localResult += keyName + '(' + key + ')' + ': ' + value;
            }
            result += localResult + '] ';
        }
    }
    console.log('inst: ' + currentIndicator + '.' + codeOfst + ' ' + instName + ' ' + size + ' ' + result);
}

function traceStoreZero (addr) {
    console.log ('warning: storing 0 to address ' + addr);
}
function traceDerefZero () {
    console.log ('warning: dereferencing address 0.');
}
function traceStoreTrailToReg() {
    console.log ('warning: storing $minTrail address to $reg 1.')
}
function traceStore(addr, val) {
    console.log ('    store ' + val + ' to ' + addr + '.')
}
function warnMaxStack(addr, max) {
    console.log('warning: address ' + addr + ' exceeds max stack ' + max + '.');
    alert('warning: address ' + addr + ' exceeds max stack ' + max + '.');
}
function warnMaxTrail(addr, max) {
    let msg = 'warning: address ' + addr + ' exceeds max trail ' + max + '.';
    console.log(msg);
    alert(msg);
}
function warnInvalidMemoryLayout(heapMin, heapMax, stackMin, stackMax, registerMin, registerMax) {
    let msg = 'warning: invalid memory layout.\n' +
        'It should be heap min/max < stack min/max < register min/maxaddress.\n' +
        'heap = ' + heapMin + '/' + heapMax + ', stack = ' + stackMin + '/' + stackMax + ', register = ' +
        registerMin + '/' + registerMax + '.';

    console.log(msg);
    alert(msg);
}
function warnInvalidSwitchTag(tag) {
    let msg = 'warning: switch tag is invalid = ' + tag + '.';
    console.log(msg);
    alert(msg);

}
const TAG_REF = 0; // 0x00000000
const TAG_STR = 1; // 0x08000000
const TAG_LIS = 2; // 0x10000000
//const TAG_INT = 3; // 0x18000000
const TAG_CON = 4; // 0x20000000 // was TAG_ATM.
//const TAG_FLT = 5; // 0x28000000
const TAG_PRE = 6; // 0x30000000
const WORD_BITS = 27;
const TAG_MASK = 7;

// function add_tag(value) {
//     return value ^ (TAG_STR << WORD_BITS)
// }

function get_tag(p)
{
    // >>> is unsigned-right-shift. Nice.
    return (p >>> WORD_BITS) & TAG_MASK;
}
function get_val(p)
{
    return p & ((1 << WORD_BITS)-1);
}

// function displayProgram(codes) {
//     for(let i = 0;i < codes.length;i++) {
//         i = displayInstruction(i, codes);
//     }
// }
//
// function displayInstruction(i, codes) {
//
// }

module.exports.importObject = importObject;
module.exports.runQuery = runQuery;
module.exports.initialize_op_codes = initialize_op_codes;
module.exports.validate_op_codes = validate_op_codes;
module.exports.opCodes = opCodes;
module.exports.lookup_atom = lookup_atom;
module.exports.find_atom = find_atom;
module.exports.lookupIndicator = lookupIndicator;
module.exports.registerProgram = registerProgram;
module.exports.process_labels = process_labels;
module.exports.flatten = flatten;
module.exports.expand_hash = expand_hash;
module.exports.getOpCodeName = getOpCodeName;
module.exports.display_results = display_results;
module.exports.interpret_memory = interpret_memory;
module.exports.get_tag = get_tag;
module.exports.get_val = get_val;
