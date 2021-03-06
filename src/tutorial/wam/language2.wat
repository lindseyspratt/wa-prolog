(;
Terms: a term cell value is an i32 that is either a tagged value, written <TAG, VALUE>, or an 'indicator'.
The tagged value has a TAG of either 0 (for a REFerence) or 1 (for a STRucture). The VALUE portion of the term cell
value is currently always an address in (word-indexed) memory.

The tagged value has the lower WORD_BITS (= 27)
bits to the VALUE and the upper 5 bits to hold the TAG. For a REF term cell value the term cell value = VALUE since TAG = 0.

An indicator value is an untagged integer that uniquely identifies a functor (unique in its combination of
name and arity). It is an identifier that can be used with the imported getIndicatorArity function to determine the arity for
the indicated functor.

Memory layout: first $maxRegister*4 slots hold the registers, followed by 'PDL' slots, followed by the 'heap' slots.
Each register is one i32 word, which is four 'byte' slots of memory.
Each heap slot is one i32 word, which is four 'byte' slots of memory.

Addresses are either register locations or heap locations. Register addresses are 1 through $maxRegister.
Heap addresses are $minHeap and greater.
All addresses are word locations in memory.

Atom interning: The host environment implements $lookupAtom (perhaps with a simple array of strings).

Code: the word code for WAM instructions is held in the host environment. The (call $getCode P) function returns the code word at 'P'.
The (call $setCode pred) function sets the host code to the code for the predicate program with identifier 'pred'.

;)
(module
    (import "js" "mem" (memory 1))
    (import "js" "table" (table 2 funcref))
    (import "js" "maxRegister" (global $maxRegister i32))
    (import "js" "pdlStart" (global $pdlStart i32))
    (import "js" "maxPDL" (global $maxPDL i32))
    (import "js" "stackStart" (global $stackStart i32))
    (import "js" "minStack" (global $minStack i32))
    (import "js" "heapStart" (global $heapStart i32))
    (import "js" "minHeap" (global $minHeap i32))
    (import "js" "lookupAtom" (func $lookupAtom (param i32) (param i32) (result i32)))
    (import "js" "getIndicatorArity" (func $getIndicatorArity (param i32) (result i32)))
    (import "js" "getCode" (func $getCode (param i32) (result i32)))
    (import "js" "setCode" (func $setCode (param i32)))
;;    (import "js" "consoleLog" (func $consoleLog (param i32)))

    (global $TAG_REF i32 (i32.const 0))
    (global $TAG_STR i32 (i32.const 1))
    (global $TAG_LST i32 (i32.const 2))
    (global $TAG_INT i32 (i32.const 3))
    (global $TAG_ATM i32 (i32.const 4))
    (global $TAG_FLT i32 (i32.const 5))
    (global $TAG_MASK i32 (i32.const 7))
    (global $WORD_BITS i32 (i32.const 27))

    (global $WRITE_MODE i32 (i32.const 1))
    (global $READ_MODE i32 (i32.const 2))

    (global $FALSE i32 (i32.const 0))
    (global $TRUE i32 (i32.const 1))

    (global $H (mut i32) (global.get $minHeap)) ;; $H is the current top of the heap.
    (global $P (mut i32) (i32.const -1)) ;; $P is the 'program' instruction counter. The instruction is at (call $getCode (global.get $P)).
    (global $PPred (mut i32) (i32.const -1)) ;; $PPred is the predicate code identifier for the current code. This identifier is used to set the host 'code' that is referenced be getCode func.
    (global $PDL (mut i32) (i32.const -1)) ;; $PDL is offset of the top of a push down list for use in unification.
    (global $S (mut i32) (i32.const 0)) ;; $S is current structure argument address (on heap). Used when $mode = $READ_MODE.
    (global $CP (mut i32) (i32.const 0)) ;; $CP is the Continuation Program instruction counter - to continue after returning from a call.
    (global $CPPred (mut i32) (i32.const 0)) ;; $CPPred is the predicate code identifier for the continuation code. This identifier is used to set the host 'code' that is referenced be getCode func.
    (global $E (mut i32) (global.get $minStack)) ;; $E is the Environment location in the Stack.

    (global $ECE i32 (i32.const 0))          ;; Continuation Environment
    (global $ECP i32 (i32.const 1))          ;; Continuation Program-instruction.
    (global $ECPPred i32 (i32.const 2))      ;; Continuation Program-instruction Predicate code identifier.
    (global $En i32 (i32.const 3))           ;; number of permanent variables
    (global $EPermVarBase i32 (i32.const 3)) ;; add i to locate the i'th permanent variable.

    (global $fail (mut i32) (i32.const 0))
    (global $mode (mut i32) (i32.const 0))

    (func $getH (result i32)
        global.get $H
    )

    (func $setH (param $val i32)
        local.get $val
        global.set $H
    )

    (func $shiftTag (param $tag i32) (result i32)
        local.get $tag
        global.get $WORD_BITS
        i32.shl)
    (func $tagInteger (param $val i32) (result i32)
        global.get $TAG_INT
        local.get $val
        call $applyTag
    )
    (func $tagStructure (param $val i32) (result i32)
        global.get $TAG_STR
        local.get $val
        call $applyTag
    )
    (func $tagReference (param $val i32) (result i32)
        global.get $TAG_REF
        local.get $val
        call $applyTag
    )
    (func $applyTag (param $tag i32) (param $val i32) (result i32)
        local.get $tag
        call $shiftTag
        local.get $val
        i32.xor
    )
    (func $termTag (param $term i32) (result i32)
        ;; (p >>> WORD_BITS) & TAG_MASK; unsigned right shift.
        (i32.and (global.get $TAG_MASK)
            (i32.shr_u (local.get $term) (global.get $WORD_BITS))
        )
    )
    (func $termVal (param $term i32) (result i32)
        ;; p & ((1 << WORD_BITS)-1);
        (i32.and
            (local.get $term)
            (i32.sub
                (i32.shl (i32.const 1) (global.get $WORD_BITS))
                (i32.const 1)
            )
        )
    )
    (func $wordToByteOffset (param $val i32) (result i32)
        local.get $val
        i32.const 4
        i32.mul
    )
    (func $wordToHeapByteOffset (param $addr i32) (result i32)
        local.get $addr
        call $wordToByteOffset
    )
    (func $wordToPDLByteOffset (param $val i32) (result i32)
        local.get $val
        call $wordToByteOffset
        global.get $pdlStart
        i32.add
    )
    (func $stackRegisterAddress (param $Yreg i32) (result i32)
        (i32.add (local.get $Yreg)
            (i32.add (global.get $EPermVarBase) (global.get $E)))
    )
    (func $increment (param $val i32) (result i32)
        i32.const 1
        local.get $val
        i32.add
    )
    (func $addToH (param $val i32)
        global.get $H
        local.get $val
        i32.add

        global.set $H
    )
    (func $addToP (param $val i32)
        global.get $P
        local.get $val
        i32.add

        global.set $P
    )
    (func $getCodeArg (param $val i32) (result i32)
        (call $getCode (i32.add (global.get $P) (local.get $val)))
    )
    (func $storeStructureAtHeapTop
        (call $storeToHeap (global.get $H) (call $tagStructure (call $increment (global.get $H))))
    )
    (func $storeIndicatorAtHeapTopPlus (param $fn i32)
        (call $storeToHeap (call $increment (global.get $H)) (local.get $fn))
    )
    (func $storeReferenceAtHeapTop
        (call $storeToHeap (global.get $H) (call $tagReference (global.get $H)))
    )
    (func $loadFromRegister (param $reg i32) (result i32)
        (call $loadFromAddress (local.get $reg))
    )

    (func $storeToRegister (param $reg i32) (param $val i32)
        (call $storeToAddress (local.get $reg) (local.get $val))
    )

    (func $storeToHeap (param $addr i32) (param $val i32)
         (call $storeToAddress (local.get $addr) (local.get $val))
    )

    (func $storeToStack (param $addr i32) (param $val i32)
         (call $storeToAddress (local.get $addr) (local.get $val))
    )

    (func $storeToAddress (param $addr i32) (param $val i32)
        (i32.store (call $wordToByteOffset (local.get $addr)) (local.get $val))
    )

    (func $storeHeapToHeap (param $sourceAddr i32) (param $destinationAddr i32)
        (call $storeToHeap (local.get $destinationAddr) (call $loadFromHeap (local.get $sourceAddr)))
    )
    (func $storeAddressToAddress (param $sourceAddr i32) (param $destinationAddr i32)
        (call $storeToAddress (local.get $destinationAddr) (call $loadFromAddress (local.get $sourceAddr)))
    )

   (func $storeRegisterToHeapTop (param $reg i32)
        (call $storeToHeap 
            (global.get $H)
            (call $loadFromRegister (local.get $reg))
        )
    )

    (func $storeHeapTopToRegister (param $reg i32)
        (call $storeToRegister (local.get $reg) (call $loadFromHeap (global.get $H)))
    )
    (func $loadFromHeap (param $addr i32) (result i32)
        (call $loadFromAddress (local.get $addr))
    )
    (func $loadFromStack (param $addr i32) (result i32)
        (call $loadFromAddress (local.get $addr))
    )
    (func $loadFromAddress (param $addr i32) (result i32)
        (i32.load (call $wordToByteOffset (local.get $addr)))
    )

    (func $pushPDL (param $val i32)
        (global.set $PDL
            (i32.add
                (global.get $PDL)
                (i32.const 1)
            )
        )

        (i32.store
            (call $wordToPDLByteOffset (global.get $PDL))
            (local.get $val)
        )
    )

    (func $popPDL (result i32)
        (local $popResult i32)
        (local.set $popResult (i32.load (call $wordToPDLByteOffset (global.get $PDL))))

        (global.set $PDL
            (i32.sub
                (global.get $PDL)
                (i32.const 1)
            )
        )

        (local.get $popResult)
    )

    (func $emptyPDL (result i32)
        (i32.eq (global.get $PDL) (i32.const -1))
    )

    ;; The store is the generic term for both registers and heap.
    ;; All addresses (registers and heap cells) are word indexes into
    ;; memory. Register addresses are words from 0 to maxRegister.
    ;; Heap addresses are words from minHeap on up.
    (func $loadFromStore (param $addr i32) (result i32)
        (i32.load
            (call $wordToByteOffset (local.get $addr))
        )
    )

    ;; The 'store' (register or heap cell) address may
    ;; contain a 'structure' term or a 'reference' (variable)
    ;; that is bound to some other term or is unbound.
    ;; If the $addr points to a bound reference term
    ;; then the deref function chases the bound terms until
    ;; it encounters an unbound reference term or an indicator term.
    ;; If the $addr points to an indicator term
    ;; or an unbound reference term then
    ;; deref returns the $addr.

    (func $deref (param $addr i32) (result i32)
        (local $term i32)
        (local.set $term (call $loadFromStore (local.get $addr)) )
        (if (result i32)
            (i32.and
                (i32.eq (global.get $TAG_REF) (call $termTag (local.get $term)))
                (i32.ne (local.get $addr) (call $termVal (local.get $term))))
            (then (call $deref (call $termVal (local.get $term))) )
            (else (local.get $addr))
        )
    )

    ;; The bind function binds togther the terms pointed to by two
    ;; addresses (where an address is a word cell in memory, either
    ;; a register or a heap cell).

    (func $bind (param $a1 i32) (param $a2 i32)
        (local $term1 i32) (local $term2 i32)
        (local $t1 i32) (local $t2 i32)
        (local.set $term1 (call $loadFromStore (local.get $a1)))
        (local.set $term2 (call $loadFromStore (local.get $a2)))
        (local.set $t1 (call $termTag (local.get $term1)))
        (local.set $t2 (call $termTag (local.get $term2)))
        (if
            (i32.and
                (i32.eq (local.get $t1) (global.get $TAG_REF))
                (i32.or
                    (i32.ne (local.get $t2) (global.get $TAG_REF))
                    (i32.lt_u (local.get $a2) (local.get $a1))
                )
            )
            (then
                (call $storeAddressToAddress (local.get $a2) (local.get $a1)) ;; value at source $a2 to store (memory) word at destination $a1
                ;; not yet implemented: (call $trail (local.get $a1))
            )
            (else
                (call $storeAddressToAddress (local.get $a1) (local.get $a2)) ;; value at source $a1 to store (memory) word at destination $a2
                ;; not yet implemented: (call $trail (local.get $a2))
            )
        )
    )

    ;; The unify function unifies two terms at the specified input parameter addresses, $a1 and $a2.
    ;; The PushDownList (PDL) is a stack of addresses (memory word offsets for registers and heap cells).
    ;; The unify function puts the addresses of two terms to be unified consecutively on the top of the
    ;; PDL. When the terms being unified are structures (which must have the same number of arguments
    ;; in order to successfully unify), the corresponding pairs of addresses of argument terms are put
    ;; on the top of the PDL, then the unify processing loops and takes the top two term addresses
    ;; from the top of the PDL and repeats.

    (func $unify (param $a1 i32) (param $a2 i32)
        ;; $d1 and $d2 are dereferenced addresses of terms.
        (local $d1 i32)
        (local $d2 i32)

        (call $pushPDL (local.get $a1))
        (call $pushPDL (local.get $a2))
        (global.set $fail (global.get $FALSE))
        (block
            (loop
                (local.set $d1 (call $deref (call $popPDL)))
                (local.set $d2 (call $deref (call $popPDL)))
                (if
                    (i32.ne (local.get $d1) (local.get $d2))
                    (then
                        (global.set $fail
                            (call $unifyTerms (local.get $d1) (local.get $d2)))
                    )
                )
                (br_if 1 (i32.or (call $emptyPDL) (i32.eq (global.get $fail) (global.get $TRUE))))
                (br 0)
            )
        )
    )

    (func $unifyTerms (param $d1 i32) (param $d2 i32) (result i32)
        (local $term1 i32) (local $term2 i32)
        (local $tag1 i32) (local $val1 i32)
        (local $tag2 i32) (local $val2 i32)
        (local $indicator1 i32) (local $indicator2 i32)
        (local $arity i32)
        (local $argIdx i32)

        (local.set $term1 (call $loadFromStore (local.get $d1)) )

        (local.set $tag1 (call $termTag (local.get $term1)))
        (local.set $val1 (call $termVal (local.get $term1)))

        (local.set $term2 (call $loadFromStore (local.get $d2)) )

        (local.set $tag2 (call $termTag (local.get $term2)))
        (local.set $val2 (call $termVal (local.get $term2)))

        (if (result i32)
            (i32.or
                (i32.eq (global.get $TAG_REF) (local.get $tag1))
                (i32.eq (global.get $TAG_REF) (local.get $tag2))
            )
            (then
                (call $bind (local.get $d1) (local.get $d2)) ;; bind the two input store addresses
                (return (global.get $FALSE))
            )
            (else
                (local.set $indicator1 (call $loadFromStore (local.get $val1)) )
                (local.set $indicator2 (call $loadFromStore (local.get $val2)) )

                (if (result i32)
                    (i32.eq (local.get $indicator1) (local.get $indicator2))
                    (then
                        (local.set $arity (call $getIndicatorArity (local.get $indicator1)))
                        (local.set $argIdx (i32.const 1))
                        (block
                            (loop
                                (call $pushPDL (i32.add (local.get $val1) (local.get $argIdx)))
                                (call $pushPDL (i32.add (local.get $val2) (local.get $argIdx)))
                                (br_if 1 (i32.eq (local.get $argIdx) (local.get $arity)))
                                (local.set $argIdx (i32.add (local.get $argIdx) (i32.const 1)))
                                (br 0)
                            )
                        )
                        (return (global.get $FALSE))
                    )
                    (else (return (global.get $TRUE)))
                )
            )
        )
    )

    (func $initialize_environment
        (call $storeToStack (global.get $minStack) (i32.const -1))
        (call $storeToStack (i32.add (global.get $minStack) (global.get $ECP)) (i32.const -1))
        (call $storeToStack (i32.add (global.get $minStack) (global.get $ECPPred)) (i32.const -1))
        (call $storeToStack (i32.add (global.get $minStack) (global.get $En)) (i32.const 0))
    )

    ;; A sequence of instructions for a query are in the host 'code' var.
    ;; $run initializes the environment to 'empty' and sets $P = 0 (for the 0'th
    ;; instruction in 'code') and $PPred = -1 (to indicate that there is no
    ;; current predicate).
    ;; The initial environment has $CP == -1. This continuation program instruction
    ;; is used by 'proceed'. The query instructions should end with 'call'.
    ;; The call instruction ends with either 'proceed' or 'deallocate', either will pick up
    ;; this $CP==-1 value and copy it to $P, which halts the evalLoop.

    (func $run
        (call $initialize_environment)
        (global.set $PPred (i32.const -1))
        (global.set $P (i32.const 0))
        (call $evalLoop)
    )
    ;; WAM instruction loop. This halts when $P == -1.

    (func $evalLoop
        (block
            (loop
                (call $evalOp (call $getCode (global.get $P)))
                (br_if 1 (i32.eq (global.get $P) (i32.const -1)))
                (br 0)
            )
        )
    )

    ;; $evalOp dispatches on code($P) to the instruction indicated by the operator code (opCode).
    ;; Each instruction extracts the relevant arguments from code at code($P +1), ..., code($P + n),
    ;; finally resetting $P to $P+n+1 (skipping over the n arguments).

    ;; nop - 'no operator' ;; 0
    ;; (func $putStructure (param $indicator i32) (param $reg i32)) ;; 1
    ;; (func $setVariable (param $reg i32)) ;; 2
    ;; (func $putVariable (param $Xreg i32) (param $Areg i32)) ;; 3
    ;; (func $getVariable (param $Xreg i32) (param $Areg i32)) ;; 4
    ;; (func $setValue (param $reg i32)) ;; 5
    ;; (func $putValue (param $Xreg i32) (param $Areg i32)) ;; 6
    ;; (func $getValue (param $Xreg i32) (param $Areg i32)) ;; 7
    ;; (func $getStructure (param $indicator i32) (param $reg i32)) ;; 8
    ;; (func $unifyVariable (param $reg i32)) ;; 9
    ;; (func $unifyValue (param $reg i32)) ;; 10
    ;; (func $call (param $pred i32)) ;; 11
    ;; (func $proceed) ;; 12
    ;; (func $allocate (param $N i32)) ;; 13
    ;; (func $deallocate) ;; 14
    (func $nop_opcode (result i32) (i32.const 0))
    (func $put_structure_opcode (result i32) (i32.const 1))
    (func $set_variable_opcode (result i32) (i32.const 2))
    (func $put_variable_opcode (result i32) (i32.const 3))
    (func $get_variable_opcode (result i32) (i32.const 4))
    (func $set_value_opcode (result i32) (i32.const 5))
    (func $put_value_opcode (result i32) (i32.const 6))
    (func $get_value_opcode (result i32) (i32.const 7))
    (func $get_structure_opcode (result i32) (i32.const 8))
    (func $unify_variable_opcode (result i32) (i32.const 9))
    (func $unify_value_opcode (result i32) (i32.const 10))
    (func $call_opcode (result i32) (i32.const 11))
    (func $proceed_opcode (result i32) (i32.const 12))
    (func $allocate_opcode (result i32) (i32.const 13))
    (func $deallocate_opcode (result i32) (i32.const 14))


    (func $evalOp (param $op i32)
        (block
        (block
        (block
        (block
        (block
        (block
        (block
        (block
        (block
        (block
        (block
        (block
        (block
        (block
        (block
            (br_table 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 (local.get $op))
            ) ;; 0
            (call $op0) ;; nop
            return
        ) ;; 1
            (call $op1) ;; $putStructure
            return
        ) ;; 2
            (call $op2) ;; $setVariable
             return
       ) ;; 3
            (call $op3) ;; $putVariable
            return
        ) ;; 4
            (call $op4) ;; $getVariable
            return
        ) ;; 5
            (call $op5) ;; $setValue
            return
        ) ;; 6
            (call $op6) ;; $putValue
            return
        ) ;; 7
            (call $op7) ;; $getValue
            return
        ) ;; 8
            (call $op8) ;; $getStructure
            return
        ) ;; 9
            (call $op9) ;; $unifyVariable
            return
        ) ;; 10
            (call $op10) ;; $unifyValue
            return
        ) ;; 11
            (call $op11) ;; $call
            return
        ) ;; 12
            (call $op12) ;; $proceed
            return
        ) ;; 13
            (call $op13) ;; $allocate
            return
        ) ;; 14
            (call $op14) ;; $deallocate
            return
   )

    (func $op0 ;; No operation
        (call $addToP (i32.const 1))
    )

    (func $op1 ;; $putStructure
        (call $putStructure (call $getCodeArg (i32.const 1)) (call $getCodeArg (i32.const 2)))
        (call $addToP (i32.const 3))
    )

    (func $op2 ;; setVariable
        (call $setVariable (call $getCodeArg (i32.const 1)))
        (call $addToP (i32.const 2))
    )

    (func $op3 ;; putVariable
        (call $putVariable (call $getCodeArg (i32.const 1)) (call $getCodeArg (i32.const 2)) (call $getCodeArg (i32.const 3)))
        (call $addToP (i32.const 4))
    )

    (func $op4 ;; getVariable X/Ytype, permanent or temporary target register ID, source (argument temporary) register ID
        (call $getVariable (call $getCodeArg (i32.const 1)) (call $getCodeArg (i32.const 2)) (call $getCodeArg (i32.const 3)))
        (call $addToP (i32.const 4))
    )

    (func $op5 ;; setValue
        (call $setValue (call $getCodeArg (i32.const 1)))
        (call $addToP (i32.const 2))
    )

    (func $op6 ;; putValue
        (call $putValue (call $getCodeArg (i32.const 1)) (call $getCodeArg (i32.const 2)) (call $getCodeArg (i32.const 3)))
        (call $addToP (i32.const 4))
    )

    (func $op7 ;; getValue
        (call $getValue (call $getCodeArg (i32.const 1)) (call $getCodeArg (i32.const 2)))
        (call $addToP (i32.const 3))
    )

    (func $op8 ;; getStructure
        (call $getStructure (call $getCodeArg (i32.const 1)) (call $getCodeArg (i32.const 2)))
        (call $addToP (i32.const 3))
    )

    (func $op9 ;; unifyVariable
        (call $unifyVariable (call $getCodeArg (i32.const 1)))
        (call $addToP (i32.const 2))
    )

    (func $op10 ;; unifyValue
        (call $unifyValue (call $getCodeArg (i32.const 1)))
        (call $addToP (i32.const 2))
    )

    (func $op11 ;; call
        (call $call (call $getCodeArg (i32.const 1)))
        ;; call manages $P directly, resetting it to 0 for the new 'code' for the new predicate.
    )

    (func $op12 ;; proceed
        (call $proceed)
        ;; proceed manages $P directly, resetting it to $CP.
    )

    (func $op13 ;; allocate
        (call $allocate (call $getCodeArg (i32.const 1)))
        (call $addToP (i32.const 2))
    )

    (func $op14 ;; deallocate
        (call $deallocate)
        ;; deallocate manages $P directly, resetting it to STACK[$E + $ECP].
    )

    (func $putStructure (param $indicator i32) (param $reg i32)
        call $storeStructureAtHeapTop

        (call $storeIndicatorAtHeapTopPlus (local.get $indicator)) ;; at $H+1

        (call $storeHeapTopToRegister (local.get $reg))

        (call $addToH (i32.const 2)) ;; $H <- $H+2.
    )

    (func $setVariable (param $reg i32)
        call $storeReferenceAtHeapTop

        local.get $reg
        call $storeHeapTopToRegister

        i32.const 1
        call $addToH
    )

    (func $putVariable (param $variableType i32) (param $regID i32) (param $Areg i32)
        (local $targetAddr i32)
        (local.set $targetAddr (call $resolveRegisterID (local.get $variableType) (local.get $regID)) )
        call $storeReferenceAtHeapTop
        (call $storeHeapTopToRegister (local.get $targetAddr))
        (call $storeHeapTopToRegister (local.get $Areg))
        (call $addToH (i32.const 1))
    )

    ;; variableType == 0 -> temporary variable, stored in X registers where register ID -> address == register ID as memory word offset
    ;; variableType == 1 (!= 0) -> permanent variable, stored in Y registers where register ID -> address
    ;; == environment slot/register == memory word offset of $E + $EPermVarBase + register ID

    (func $resolveRegisterID (param $variableType i32) (param $reg i32) (result i32)
        (if (result i32)
            (i32.eq (local.get $variableType) (i32.const 0))
            (then (local.get $reg))
            (else (call $stackRegisterAddress (local.get $reg)))
        )
    )

    (func $getVariable (param $variableType i32) (param $regID i32) (param $Areg i32)
        (local $targetAddr i32)
        (local.set $targetAddr (call $resolveRegisterID (local.get $variableType) (local.get $regID)) )
        (call $storeAddressToAddress (local.get $Areg) (local.get $targetAddr))
    )

    (func $setValue (param $reg i32)
        local.get $reg
        call $storeRegisterToHeapTop

        i32.const 1
        call $addToH
    )
    (func $putValue (param $variableType i32) (param $XYreg i32) (param $Areg i32)
        (local $targetAddr i32)
        (local.set $targetAddr (call $resolveRegisterID (local.get $variableType) (local.get $XYreg)) )
        (call $storeAddressToAddress (local.get $targetAddr) (local.get $Areg))
    )
    (func $getValue (param $Xreg i32) (param $Areg i32)
        (call $unify (local.get $Xreg) (local.get $Areg))
    )

    (;
    From https://github.com/a-yiorgos/wambook/blob/master/wamerratum.txt:
    "In the definition of get_structure (fig 2.6, page 13) S should be
    initialized to 1 before exiting get_structure in either READ or WRITE
    modes."
    This appears to mean not that "S should be initialized to 1" but
    rather that "S should be initialized to (location of first argument)"
    where (location of first argument) is the location of the 'indicator'
    (i.e. 'f/n') plus one.
    In the WRITE case (implemented by $getStructureSTR) this is
    (call $termVal (local.get $addr)) + 1.
    In the READ case (implemented by $getStructureREF) this is ($H+2).
    ;)

    (func $getStructure (param $indicator i32) (param $reg i32)
        (local $addr i32) (local $term i32) (local $tag i32)
        (local.set $addr (call $deref (local.get $reg)))
        (local.set $term (call $loadFromStore (local.get $addr)))
        (local.set $tag (call $termTag (local.get $term)))

        (block
            (block
                (block
                    (br_table 0 1 2 (local.get $tag) )
                )
                ;; (local.get $tag) = 0 = (global.get $TAG_REF)
                (global.set $fail (call $getStructureREF (local.get $indicator) (local.get $addr)))
                return
            )
            ;; (local.get $tag) = 1 = (global.get $TAG_STR)
            (global.set $fail (call $getStructureSTR (local.get $indicator) (call $termVal (local.get $term))))
            return
        )
        ;; (local.get $tag) > 1 = failure
        (global.set $fail (global.get $TRUE))
    )

    (func $getStructureREF (param $indicator i32) (param $addr i32) (result i32)
        (local $nextH i32)
        (local.set $nextH (call $increment (global.get $H)))                        ;; nextH <- H + 1
        (call $storeToHeap (global.get $H) (call $tagStructure (local.get $nextH))) ;; HEAP[H]􏰃 <- <STR, nextH>
        (call $storeToHeap (local.get $nextH) (local.get $indicator))               ;; HEAP[nextH􏰌]􏰃 <- f/n
        (call $bind (local.get $addr) (global.get $H))                              ;; bind(addr, H)
        (call $addToH (i32.const 2))                                                ;; H <- H+2
        (global.set $S (global.get $H))                                             ;; S <- H (H is the location where the first argument of the structure will be stored).
        (global.set $mode (global.get $WRITE_MODE))                                 ;; mode <- write

        (return (global.get $FALSE)) ;; 'fail' is false.
    )

    (func $getStructureSTR (param $indicator i32) (param $a i32) (result i32)

        (if (result i32)
            (i32.eq (call $loadFromHeap (local.get $a)) (local.get $indicator)) ;; if(HEAP[a] = f/n) then
            (then
                (global.set $S (call $increment (local.get $a)))                ;; S􏰃 <- a+1
                (global.set $mode (global.get $READ_MODE))                      ;; mode <- READ
                (return (global.get $FALSE))                                    ;; return fail is false.
            )
            (else (return (global.get $TRUE)) )                                 ;; else return fail is true.
        )
    )

    (func $unifyVariable (param $reg i32)
        (if (i32.eq (global.get $mode) (global.get $READ_MODE))
            (then (call $storeAddressToAddress (global.get $S) (local.get $reg)) )
        (else (if (i32.eq (global.get $mode) (global.get $WRITE_MODE))
            (then (call $setVariable (local.get $reg)) )
            )
        )
        )
        (global.set $S (call $increment (global.get $S)))
    )

    (func $unifyValue (param $reg i32)
        (if (i32.eq (global.get $mode) (global.get $READ_MODE))
            (then (call $unify  (local.get $reg) (global.get $S)) )
        (else (if (i32.eq (global.get $mode) (global.get $WRITE_MODE))
            (then (call $setValue (local.get $reg)) )
            )
        )
        )
        (global.set $S (call $increment (global.get $S)))
    )

    (func $call (param $pred i32)
        (global.set $CP (i32.add (global.get $P) (i32.const 2)))
        (global.set $CPPred (global.get $PPred))
        (call $setCode (local.get $pred))
        (global.set $PPred (local.get $pred))
        (global.set $P (i32.const 0))
    )

    (func $proceed
        (global.set $P (global.get $CP))
        (global.set $PPred (global.get $CPPred))
        (call $setCode (global.get $PPred))
    )

    (func $allocate (param $N i32)
        (local $newE i32)
        (local.set $newE
            (i32.add
                (i32.add (i32.const 1) (global.get $EPermVarBase))
                (i32.add
                    (global.get $E)
                    (call $loadFromStack (i32.add (global.get $E) (global.get $En)))))
        )
        (call $storeToStack (local.get $newE) (global.get $E))
        (call $storeToStack (i32.add (local.get $newE) (global.get $ECP)) (global.get $CP))
        (call $storeToStack (i32.add (local.get $newE) (global.get $ECPPred)) (global.get $CPPred))
        (call $storeToStack (i32.add (local.get $newE) (global.get $En)) (local.get $N))
        (global.set $E (local.get $newE))
    )

    (func $deallocate
        (global.set $PPred (call $loadFromStack (i32.add (global.get $E) (global.get $ECPPred)) ))
        (if (i32.eq (global.get $PPred) (i32.const -1))
            (then (global.set $P (i32.const -1))) ;; causes evalLoop to halt.
            (else (global.set $P (call $loadFromStack (i32.add (global.get $E) (global.get $ECP)) )) )
        )

        (global.set $E (call $loadFromStack (global.get $E)))
    )

    (export "setH" (func $setH))
    (export "shiftTag" (func $shiftTag))
    (export "tagInteger" (func $tagInteger))
    (export "tagStructure" (func $tagStructure))
    (export "storeStructureMem" (func $storeStructureAtHeapTop))
    (export "storeReferenceAtHeapTop" (func $storeReferenceAtHeapTop))
    (export "getStructure" (func $getStructure))
    (export "addToH" (func $addToH))
    (export "bind" (func $bind))

    (export "nop_opcode" (func $nop_opcode))
    (export "put_structure_opcode" (func $put_structure_opcode))
    (export "set_variable_opcode" (func $set_variable_opcode))
    (export "put_variable_opcode" (func $put_variable_opcode))
    (export "get_variable_opcode" (func $get_variable_opcode))
    (export "set_value_opcode" (func $set_value_opcode))
    (export "put_value_opcode" (func $put_value_opcode))
    (export "get_value_opcode" (func $get_value_opcode))
    (export "get_structure_opcode" (func $get_structure_opcode))
    (export "unify_variable_opcode" (func $unify_variable_opcode))
    (export "unify_value_opcode" (func $unify_value_opcode))
    (export "call_opcode" (func $call_opcode))
    (export "proceed_opcode" (func $proceed_opcode))
    (export "allocate_opcode" (func $allocate_opcode))
    (export "deallocate_opcode" (func $deallocate_opcode))

    (export "run" (func $run))
)
