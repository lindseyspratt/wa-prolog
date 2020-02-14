(;
Language3D is Language3C with the Indexing as described in Chapter 5 section 10.

Terms: a term cell value is an i32 that is either a tagged value, written <TAG, VALUE>, or an 'indicator'.
The tagged value has a TAG of either 0 (for a REFerence), 1 (for a STRucture), 2 (for a LISt), or 3 (for a CONstant).
The VALUE portion of the term cell value is an address in (word-indexed) memory references, structures, and lists (0, 1, and 2).
The VALUE portion of a constant term cell value is a 'literal' value, not an address.

The tagged value has the lower WORD_BITS (= 27)
bits to the VALUE and the upper 5 bits to hold the TAG. For a REF term cell value the term cell value = VALUE since TAG = 0.

An indicator value is an untagged integer that uniquely identifies a functor (unique in its combination of
name and arity). It is an identifier that can be used with the imported getIndicatorArity function to determine the arity for
the indicated functor.

Memory layout: first $maxRegister*4 slots hold the registers, followed by 'PDL' slots, followed by the 'stack' slots,
followed by the 'trail', followed by the 'heap' slots.
Each slot is one i32 word, which is four 'byte' slots of memory.

Addresses are either register locations, stack locations, trail locations, or heap locations. Register addresses are 1 through $maxRegister.
Heap addresses are $minHeap and greater.
All addresses are word locations in memory.

Atom interning: The host environment implements $lookupAtom to associate an integer with an aribtrary string.

Code: the word code for WAM instructions is held in the host environment. The (call $getCode P) function returns the code word at 'P'.
The (call $setCode pred) function sets the host code to the code for the predicate program with identifier 'pred'.

Permanent variables: The assignment to registers is specialized to support environment trimming.
"...the later a permanent variable’s last occurrence’s goal is in the body,
the lower its offset in the current environment frame is.
Thus, the call instruction is given a second argument counting
the number of variables still needed in the environment after the point of call.
This count allows later stack allocating instructions
to compute a lower top of stack, if possible.
Namely, if the topmost frame on the stack is
the current environment (i.e., if E 􏰃 B)." [WAM Tutorial, 99, Ait-Kaci, p.59]

;)
(module
    (import "js" "mem" (memory 1))
    (import "js" "table" (table 2 funcref))
    (import "js" "minRegister" (global $minRegister i32))
    (import "js" "registerSize" (global $registerSize i32))
    (import "js" "registerStart" (global $registerStart i32))
    (import "js" "minPDL" (global $minPDL i32))
    (import "js" "pdlStart" (global $pdlStart i32))
    (import "js" "pdlSize" (global $pdlSize i32))
    (import "js" "stackStart" (global $stackStart i32))
    (import "js" "minStack" (global $minStack i32))
    (import "js" "stackSize" (global $stackSize i32))
    (import "js" "trailStart" (global $trailStart i32))
    (import "js" "minTrail" (global $minTrail i32))
    (import "js" "trailSize" (global $trailSize i32))
    (import "js" "heapStart" (global $heapStart i32))
    (import "js" "heapSize" (global $heapSize i32))
    (import "js" "minHeap" (global $minHeap i32))
    (import "js" "lookupAtom" (func $lookupAtom (param i32) (param i32) (result i32)))
    (import "js" "getIndicatorArity" (func $getIndicatorArity (param i32) (result i32)))
    (import "js" "getCodeFromProgram" (func $getCodeFromProgram (param i32) (param i32) (result i32)))
    (import "js" "getCode" (func $getCode (param i32) (result i32)))
    (import "js" "setCode" (func $setCode (param i32)))
    (import "js" "traceInstLog0" (func $traceInstLog0 (param i32)))
    (import "js" "traceInstLog1" (func $traceInstLog1 (param i32) (param i32)))
    (import "js" "traceInstLog2" (func $traceInstLog2 (param i32) (param i32) (param i32)))
    (import "js" "traceInstLog3" (func $traceInstLog3 (param i32) (param i32) (param i32) (param i32)))
    (import "js" "traceInstLog4" (func $traceInstLog4 (param i32) (param i32) (param i32) (param i32) (param i32)))
    (import "js" "traceInstSwitchLog" (func $traceInstSwitchLog (param i32) (param i32)))
    (import "js" "traceStoreZero" (func $traceStoreZero (param $addr i32)))
    (import "js" "traceDerefZero" (func $traceDerefZero))
    (import "js" "traceStoreTrailToReg" (func $traceStoreTrailToReg))
    (import "js" "traceStore" (func $traceStore (param i32) (param i32)))
    (import "js" "warnMaxStack" (func $warnMaxStack (param i32) (param i32)))
    (import "js" "warnMaxTrail" (func $warnMaxTrail (param i32) (param i32)))
    (import "js" "warnInvalidMemoryLayout"
        (func $warnInvalidMemoryLayout (param i32) (param i32) (param i32) (param i32) (param i32) (param i32)))
    (import "js" "warnInvalidSwitchTag" (func $warnInvalidSwitchTag (param i32)))

;;    (import "js" "consoleLog" (func $consoleLog (param i32)))

    (global $maxRegister (mut i32) (i32.const 0))
    (global $maxPDL (mut i32) (i32.const 0))
    (global $maxStack (mut i32) (i32.const 0))
    (global $maxTrail (mut i32) (i32.const 0))
    (global $maxHeap (mut i32) (i32.const 0))

    (global $TAG_REF i32 (i32.const 0))
    (global $TAG_STR i32 (i32.const 1))
    (global $TAG_LIS i32 (i32.const 2))
    (global $TAG_CON i32 (i32.const 4))

    ;;(global $TAG_INT i32 (i32.const 4))
    ;;(global $TAG_FLT i32 (i32.const 5))
    (global $TAG_PRE i32 (i32.const 6)) ;; for PREdicate indicator: an integer identifying Name/Arity, e.g. p/2.

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
    (global $CPPred (mut i32) (i32.const -1)) ;; $CPPred is the predicate code identifier for the continuation code. This identifier is used to set the host 'code' that is referenced be getCode func.
    (global $E (mut i32) (global.get $minStack)) ;; $E is the Environment location in the Stack.
    (global $B (mut i32) (global.get $minStack)) ;; $B is the base of the current choicepoint frame - $B can never validly be $minStack, so when the backtrack func tries to 'go to' $minStack it terminates the WAM.
    (global $HB (mut i32) (i32.const -1))
    (global $TR (mut i32) (global.get $minTrail))
    (global $num_of_args (mut i32) (i32.const -1))
    (global $inferences (mut i32) (i32.const 0))
    (global $baseExecutes (mut i32) (i32.const 0)) ;; number of executes invoked with no proceed, prior to top choicepoint frame.

    (; "...the continuation slot of the latest environment frame, STACK[E+1], always contains
        the address of the instruction immediately following the appropriate (call P,N)
        instruction where N is precisely the desired offset.
        ...the right offset is calculated by allocate as CODE[STACK[E+1􏰒]-1􏰒]." [AK99, p. 59]
        However, the WAM Erratum corrects this calculation to: CODE[CP - 1].
        [https://github.com/a-yiorgos/wambook/blob/master/wamerratum.txt]
    ;)
    (global $ECE i32 (i32.const 0))          ;; Continuation Environment.
    (global $ECP i32 (i32.const 1))          ;; Continuation Program-instruction.
    (global $ECPPred i32 (i32.const 2))      ;; Continuation Program-instruction Predicate code identifier.
    (global $EPermVarBase i32 (i32.const 2)) ;; add i to locate the i'th permanent variable.

    ;; The '$Bk*' globals are stack slots that follow the choicepoint (backtrack) frame arguments.
    ;; The initial slots of the frame are: STACK[$B] = the number of arguments in the frame, then STACK[$B + 1] to STACK[$B + n] for
    ;; frame arguments A1 through An.

    (global $BkCE i32 (i32.const 1) )           ;; Continuation Environment = STACK[$B + STACK[$B] + $BkCE]
    (global $BkCP i32 (i32.const 2) )           ;; Continuation Pointer = STACK[$B + STACK[$B] + $BkCP]
    (global $BkB i32 (i32.const 3))             ;; Previous choice point frame = STACK[$B + STACK[$B] + $BkB]
    (global $BkBP i32 (i32.const 4))            ;; Backtrack Pointer (next clause word location in code for $BkBPred program) = STACK[$B + STACK[$B] + $BkBP]
    (global $BkTR i32 (i32.const 5))            ;; Trail Pointer (address in TRAIL) = STACK[$B + STACK[$B] + $BkTR]
    (global $BkH i32 (i32.const 6))             ;; Heap pointer = STACK[$B + STACK[$B] + $BkH]
    (global $BkI i32 (i32.const 7))             ;; number of executes invoked with no 'proceed' (indicating incomplete inferences) since this frame was created and prior to subsequent frame.
    (global $BkBPred i32 (i32.const 8))         ;; Backtrack program (code contains next clause word location from $BkBP) = STACK[$B + STACK[$B] + $BkBPred]
    (global $BkSuffixSize i32 (i32.const 8))    ;; Number of slots in the choicepoint frame  following the argument slots.

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
;;    (func $tagInteger (param $val i32) (result i32)
;;        global.get $TAG_INT
;;        local.get $val
;;        call $applyTag
;;    )
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
    (func $tagConstant (param $val i32) (result i32)
        global.get $TAG_CON
        local.get $val
        call $applyTag
    )
    (func $tagList (param $val i32) (result i32)
        global.get $TAG_LIS
        local.get $val
        call $applyTag
    )
    (func $tagPredicate (param $val i32) (result i32)
        global.get $TAG_PRE
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
    (func $tempRegisterAddress (param $Xreg i32) (result i32)
        (i32.add (local.get $Xreg) (global.get $minRegister))
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
    (func $addToTR (param $val i32)
        (global.set $TR (i32.add (global.get $TR) (local.get $val)))
        (if (i32.gt_u (global.get $TR) (global.get $maxTrail))
            (then (call $warnMaxTrail (global.get $TR) (global.get $maxTrail)))
        )
    )

    (func $getInferences (result i32)
        (global.get $inferences)
    )
    (func $getCodeArg (param $val i32) (result i32)
        (call $getCode (i32.add (global.get $P) (local.get $val)))
    )
    (func $storeStructureAtHeapTopToRegister (param $reg i32)
        (call $storeToRegister (local.get $reg) (call $tagStructure (global.get $H)))
    )
    (func $storeIndicatorAtHeapTop (param $fn i32)
        (call $storeToHeap (global.get $H) (call $tagPredicate (local.get $fn)))
    )
    (func $storeConstantAtHeapTop (param $c i32)
        (call $storeToHeap (global.get $H) (call $tagConstant (local.get $c)))
    )
    (func $storeReferenceAtHeapTop
        (call $storeToHeap (global.get $H) (call $tagReference (global.get $H)))
    )
    (func $loadFromRegister (param $reg i32) (result i32)
        (call $loadFromAddress (call $tempRegisterAddress (local.get $reg)))
    )

    (func $storeToRegister (param $reg i32) (param $val i32)
        (call $storeToAddress (call $tempRegisterAddress (local.get $reg)) (local.get $val))
    )

    (func $storeToHeap (param $addr i32) (param $val i32)
        (call $storeToAddress (local.get $addr) (local.get $val))
    )

    (func $storeToStack (param $addr i32) (param $val i32)
        (if (i32.gt_u (local.get $addr) (global.get $maxStack))
            (then (call $warnMaxStack (local.get $addr) (global.get $maxStack)))
        )
        (call $storeToAddress (local.get $addr) (local.get $val))
    )

    (func $storeToTrail (param $addr i32) (param $val i32)
         (call $storeToAddress (local.get $addr) (local.get $val))
    )

    (func $storeToAddress (param $addr i32) (param $val i32)
;;        (if
;;            (i32.eq (local.get $val) (i32.const 0))
;;            (then (call $traceStoreZero (local.get $addr)))
;;        (else (if (i32.and
;;                        (i32.eq (local.get $addr) (i32.const 1))
;;                        (i32.eq (local.get $val) (global.get $minTrail)))
;;                    (then (call $traceStoreTrailToReg) )
;;               )
;;        ) )
        (call $traceStore (local.get $addr) (local.get $val))
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

    (func $storeHeapTopToAddress (param $addr i32)
        (call $storeToAddress (local.get $addr) (call $loadFromHeap (global.get $H)))
    )
    (func $loadFromHeap (param $addr i32) (result i32)
        (call $loadFromAddress (local.get $addr))
    )
    (func $loadFromStack (param $addr i32) (result i32)
        (call $loadFromAddress (local.get $addr))
    )
    (func $loadFromTrail (param $addr i32) (result i32)
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

    ;; variableType == 0 -> temporary variable, stored in X registers where register ID -> address == register ID as memory word offset
    ;; variableType == 1 (!= 0) -> permanent variable, stored in Y registers where register ID -> address
    ;; == environment slot/register == memory word offset of $E + $EPermVarBase + register ID

    (func $resolveRegisterID (param $variableType i32) (param $reg i32) (result i32)
        (if (result i32)
            (i32.eq (local.get $variableType) (i32.const 0))
            (then (call $tempRegisterAddress (local.get $reg)))
            (else (call $stackRegisterAddress (local.get $reg)))
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
;;        (if (i32.eq (local.get $addr) (i32.const 0))
;;            (then (call $traceDerefZero))
;;        )
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
                (call $trail (local.get $a1))
            )
            (else
                (call $storeAddressToAddress (local.get $a1) (local.get $a2)) ;; value at source $a1 to store (memory) word at destination $a2
                (call $trail (local.get $a2))
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
        ;; PDL has 2 elements due to two $pushPDL calls above, so the
        ;; loop must succeed at least once, so no initial loop condition
        ;; check is needed.
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
        (local $indicatorTerm1 i32) (local $indicatorTerm2 i32)
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
                (local.set $indicatorTerm1 (call $loadFromStore (local.get $val1)) )
                (local.set $indicatorTerm2 (call $loadFromStore (local.get $val2)) )

                (if (result i32)
                    (i32.eq (local.get $indicatorTerm1) (local.get $indicatorTerm2))
                    (then
                        (local.set $arity (call $getIndicatorArity (call $termVal (local.get $indicatorTerm1))))
                        (local.set $argIdx (i32.const 1))
                        ;; The structure may have arity == 0, in which case argIdx == 1 > arity
                        ;; and the loop must be skipped.
                        (if (i32.le_u (local.get $argIdx) (local.get $arity))
                            (then
                                (block
                                    (loop
                                        (call $pushPDL (i32.add (local.get $val1) (local.get $argIdx)))
                                        (call $pushPDL (i32.add (local.get $val2) (local.get $argIdx)))
                                        (br_if 1 (i32.eq (local.get $argIdx) (local.get $arity)))
                                        (local.set $argIdx (i32.add (local.get $argIdx) (i32.const 1)))
                                        (br 0)
                                    )
                                )
                            )
                        )
                        (return (global.get $FALSE))
                    )
                    (else (return (global.get $TRUE)))
                )
            )
        )
    )

    (func $backtrack
        (local $old i32)
        (if (i32.or
                (i32.eq (global.get $B) (global.get $maxStack))
                (i32.lt_s (global.get $B) (global.get $minStack)))
            (then (global.set $P (i32.const -1))) ;; terminate run of WAM
            (else
                (global.set $P
                     (call $loadFromStack
                        (i32.add
                            (global.get $B)
                            (i32.add
                                (global.get $BkBP)
                                (call $loadFromStack (global.get $B)))
                        )
                    )
                )
                (global.set $PPred
                     (call $loadFromStack
                        (i32.add
                            (global.get $B)
                            (i32.add
                                (global.get $BkBPred)
                                (call $loadFromStack (global.get $B)))
                        )
                    )
                )
                (call $setCode (global.get $PPred))
            )
        )
        (local.set $old (call $resetIncompleteInferences)) ;; discard the incomplete inference count for this choicepoint frame or for the 'base'.
    )

    (func $create_environment_frame (param $newE i32)
        (call $storeToStack (local.get $newE) (global.get $E))
        (call $storeToStack (i32.add (local.get $newE) (global.get $ECP)) (global.get $CP))
        (call $storeToStack (i32.add (local.get $newE) (global.get $ECPPred)) (global.get $CPPred))
    )

    (func $create_choicepoint_frame (param $newB i32) (param $label i32)
        (local $i i32)
        (local $lastFrameArg i32)
        (call $storeToStack (local.get $newB) (global.get $num_of_args))
        (local.set $i (i32.const 1))
        ;; If the $num_of_args == 0 then there are no args to store in
        ;; the choicepoint frame and the arg processing loop must
        ;; be skipped.
        (if (i32.le_u (local.get $i) (global.get $num_of_args))
            (then
                (block
                    (loop
                        (call $storeToStack
                            (i32.add (local.get $newB) (local.get $i))
                            (call $loadFromRegister (local.get $i)))
                        (br_if 1 (i32.eq (local.get $i) (global.get $num_of_args)))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br 0)
                    )
                )
            )
        )
        (local.set $lastFrameArg (i32.add (local.get $newB) (global.get $num_of_args)))
        (call $storeToStack
            (i32.add (local.get $lastFrameArg) (global.get $BkCE))
            (global.get $E)
        )
        (call $storeToStack
            (i32.add (local.get $lastFrameArg) (global.get $BkCP))
            (global.get $CP)
        )
        (call $storeToStack
            (i32.add (local.get $lastFrameArg) (global.get $BkB))
            (global.get $B)
        )
        (call $storeToStack
            (i32.add (local.get $lastFrameArg) (global.get $BkBP))
            (local.get $label)
        )
        (call $storeToStack
            (i32.add (local.get $lastFrameArg) (global.get $BkBPred))
            (global.get $PPred)
        )
        (call $storeToStack
            (i32.add (local.get $lastFrameArg) (global.get $BkTR))
            (global.get $TR)
        )
        (call $storeToStack
            (i32.add (local.get $lastFrameArg) (global.get $BkH))
            (global.get $H)
        )
        (call $storeToStack
            (i32.add (local.get $lastFrameArg) (global.get $BkI))
            (i32.const 0)
        )
    )

    (func $incrementInferences
        (local $incompleteInferences i32)
        (local.set $incompleteInferences (call $resetIncompleteInferences))
        (global.set $inferences (i32.add (global.get $inferences) (i32.add (local.get $incompleteInferences (i32.const 1)))))
    )

    (func $incrementIncompleteInferences
        (local $arity i32) (local $lastFrameArg i32) (local $counterAddr i32) (local $old i32)
        (if (i32.ge_s (global.get $B) (global.get $minStack))
            (then
                (local.set $arity (call $loadFromStack (global.get $B)))
                (local.set $lastFrameArg (i32.add (global.get $B) (local.get $arity)))
                (local.set $counterAddr (i32.add (local.get $lastFrameArg) (global.get $BkI)))
                (local.set $old (call $loadFromStack (local.get $counterAddr)))
                (call $storeToStack
                    (local.get $counterAddr)
                    (i32.add (local.get $old) (i32.const 1))
                )
            )
            (else
                (global.set $baseExecutes (i32.add (global.get $baseExecutes) (i32.const 1)))
            )
        )
    )

    (func $resetIncompleteInferences (result i32)
        (local $arity i32) (local $lastFrameArg i32) (local $counterAddr i32) (local $old i32)
        (if (result i32)
            (i32.ge_s (global.get $B) (global.get $minStack))
            (then
                (local.set $arity (call $loadFromStack (global.get $B)))
                (local.set $lastFrameArg (i32.add (global.get $B) (local.get $arity)))
                (local.set $counterAddr (i32.add (local.get $lastFrameArg) (global.get $BkI)))
                (local.set $old (call $loadFromStack (local.get $counterAddr)))
                (call $storeToStack
                    (local.get $counterAddr)
                    (i32.const 0)
                )
                (return (local.get $old))
            )
            (else
                (local.set $old (global.get $baseExecutes))
                (global.set $baseExecutes (i32.const 0))
                (return (local.get $old))
            )
        )
    )


    (func $storeRegistersFromChoicepoint
        (local $i i32) (local $n i32)
        (local.set $i (i32.const 1))
        (local.set $n (call $loadFromStack (global.get $B)))
        ;; If the number of registers in the choicepoint ($n) == 0
        ;; then the register processing must be skipped.
        (if (i32.le_u (local.get $i) (local.get $n))
            (then
                (block
                    (loop
                        (call $storeToRegister
                            (local.get $i)
                            (call $loadFromStack (i32.add (global.get $B) (local.get $i))))
                        (br_if 1 (i32.eq (local.get $i) (local.get $n)))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br 0)
                    )
                )
            )
        )
    )

    (func $callInstructionPermanentVariableCount (result i32)
        (if (result i32)
            (i32.eq (global.get $CPPred) (i32.const -1))
            (then (i32.const 0))
            (else (call $getCodeFromProgram (i32.sub (global.get $CP) (i32.const 1)) (global.get $CPPred)))
        )
    )

    (func $nextStackFrameSlot (result i32)
        (if (result i32)
            (i32.and
                (i32.eq (global.get $B) (i32.const -1))
                (i32.eq (global.get $E) (i32.const -1))
            )
            (then (return (global.get $minStack)))
            (else
                (if (result i32)
                    (i32.gt_s (global.get $E) (global.get $B) )
                    (then
                        (return
                            (i32.add
                                (i32.add (i32.const 1) (global.get $EPermVarBase))
                                (i32.add
                                    (global.get $E)
                                    (call $callInstructionPermanentVariableCount)))
                        )
                    )
                    (else
                       (return
                            (i32.add
                                (global.get $B)
                                (i32.add
                                    (call $loadFromStack (global.get $B)) ;; choicepoint frame arity  (number of argument registers)
                                    (i32.add (global.get $BkSuffixSize) (i32.const 1)) ;; choicepoint frame number of slots after argument registers, plus 1.
                                )
                            )
                       )
                    )
                )
            )
        )
    )

    (func $trail (param $a i32)
        (if
            (i32.or
                (i32.lt_u (local.get $a) (global.get $HB))
                (i32.and
                    (i32.lt_u (global.get $H) (local.get $a))
                    (i32.lt_u (local.get $a) (global.get $B))
                )
            )
            (then
                (call $storeToTrail (global.get $TR) (local.get $a))
                (call $addToTR (i32.const 1))
            )
        )
    )

    (func $unwindTrail (param $a1 i32) (param $a2 i32)
        (local $a2m1 i32) (local $trail_i i32) (local $i i32)
        (local.set $a2m1 (i32.sub (local.get $a2) (i32.const 1)))
        (local.set $i (local.get $a1))
        ;; If TRAIL address $a1 is greater than ($a2 - 1)
        ;; then skip processing the TRAIL slots.
        (if (i32.le_u (local.get $i) (local.get $a2m1))
            (then
                (block
                    (loop
                        (local.set $trail_i (call $loadFromTrail (local.get $i)))
                        (call $storeToStack (local.get $trail_i) (local.get $trail_i))
                        (br_if 1 (i32.eq (local.get $i) (local.get $a2m1)))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br 0)
                    )
                )
            )
        )
    )

(;
    // The table is in two layers.
    // The top layer is a sequence of tableSize 'buckets' (tableSize is a power of 2) - each
    // bucket is an address of a second layer table.
    // A second layer table has a word at code[address] with tableSize followed
    // by a sequence of tableSize pairs of key-value words
    // starting at code[address+1].
    // The pairs are in Prolog term order by keys.
    // For a large table this function can use a binary search.
    // For small tables it is sufficient to do a linear search.

    // tableSize bucket hash. Key is 0 to tableSize-1.
    // hash is low order log2(tableSize) bits.

table = [B1,...,Bn], n words, where 'n' is the tableSize. code[Bi] is address in code of subtable.
subsize = code[code[Bi]]
code[code[Bi]+1]...code[code[Bi]+subsize] = [S1, ..., Ssubsize]

mask = tableSize - 1;
bucketID = (val & mask) + 1;
bucketOfst = bucketID - 1;

bucketAddr = $T+buckefOfst;
subtableAddress = getCode(bucketAddr);
subsize = getCode(subtableAddress);
subtableStart = subtableAddress+1;
return search_bucket(val, subtableStart, subsize)
;)
    (func $get_hash (param $val i32) (param $T i32) (param $N i32) (result i32)
        (local $mask i32)
        (local $bucketID i32)
        (local $bucketOfst i32)
        (local $bucketAddr i32)
        (local $subtableAddr i32)
        (local $subtableStart i32)
        (local $subsize i32)

        (local.set $mask (i32.sub (local.get $N) (i32.const 1)))
        (local.set $bucketID (i32.add (i32.and (local.get $val) (local.get $mask)) (i32.const 1)))
        (local.set $bucketOfst (i32.sub (local.get $bucketID) (i32.const 1)))

        (local.set $bucketAddr (i32.add (local.get $T) (local.get $bucketOfst)))
        (local.set $subtableAddr (call $getCode (local.get $bucketAddr)))
        (if (result i32)
            (i32.eqz (local.get $subtableAddr))
            (then (return (i32.const 0))) ;; 0 indicates failure.
            (else
                (local.set $subsize (call $getCode (local.get $subtableAddr)))
                (local.set $subtableStart (i32.add (local.get $subtableAddr) (i32.const 1)))
                (return (call $search_bucket (local.get $val) (local.get $subtableStart) (local.get $subsize)))
            )
        )
    )

(;
    // The table is a sequence of tableSize pairs of key-value words
    // starting at code[$start].
    // The pairs are in any order.
    // If $val matches a 'key' the $search_bucket returns the key's value.
    // Otherwise it returns 0.
;)

    (func $search_bucket (param $val i32) (param $start i32) (param $size i32) (result i32)
        (local $limit i32) (local $searchAddr i32) (local $key i32)
        (local.set $limit (i32.add (local.get $start) (i32.mul (local.get $size) (i32.const 2))))
        (local.set $searchAddr (local.get $start))
        (if (i32.ge_u (local.get $searchAddr) (local.get $limit))
            (then (return (i32.const 0)))
        )
        (block
            (loop
                (local.set $key (call $getCode (local.get $searchAddr))) ;; $key is in code[$searchAddr]
                (if (i32.eq (local.get $key) (local.get $val))
                    (then (return (call $getCode (i32.add (local.get $searchAddr) (i32.const 1)))))) ;; $keyValue is in code[$searchAddr+1].
                (br_if 1 (i32.eq (local.get $searchAddr) (local.get $limit)))
                (local.set $searchAddr (i32.add (local.get $searchAddr) (i32.const 2)))
                (br 0)
            )
        )
        (return (i32.const 0))
    )

    (func $initialize_globals
        (global.set $H (global.get $minHeap)) ;; $H is the current top of the heap.
        (global.set $P (i32.const -1)) ;; $P is the 'program' instruction counter. The instruction is at (call $getCode (global.get $P)).
        (global.set $PPred (i32.const -1)) ;; $PPred is the predicate code identifier for the current code. This identifier is used to set the host 'code' that is referenced be getCode func.
        (global.set $PDL (i32.const -1)) ;; $PDL is offset of the top of a push down list for use in unification.
        (global.set $S (i32.const 0)) ;; $S is current structure argument address (on heap). Used when $mode = $READ_MODE.
        (global.set $CP (i32.const 0)) ;; $CP is the Continuation Program instruction counter - to continue after returning from a call.
        (global.set $CPPred (i32.const -1)) ;; $CPPred is the predicate code identifier for the continuation code. This identifier is used to set the host 'code' that is referenced be getCode func.
        (global.set $E (i32.const -1)) ;; $E is the Environment location in the Stack.
        (global.set $B (i32.const -1)) ;; $B is the base of the current choicepoint frame - $B can never validly be less than or equal to $minStack, so when the backtrack func tries to 'go to' an address less than or equal to $minStack it terminates the WAM.
        (global.set $HB (i32.const -1))
        (global.set $TR (global.get $minTrail))
        (global.set $num_of_args (i32.const -1))
        (global.set $inferences (i32.const 0))
    )
    (func $initialize_limits
        (global.set $maxRegister (i32.add (global.get $minRegister) (global.get $registerSize)))
        (global.set $maxStack (i32.add (global.get $minStack) (global.get $stackSize)))
        (global.set $maxPDL (i32.add (global.get $minPDL) (global.get $pdlSize)))
        (global.set $maxTrail (i32.add (global.get $minTrail) (global.get $trailSize)))
        (global.set $maxHeap (i32.add (global.get $minHeap) (global.get $heapSize)))
        ;; layout should order: Heap < Stack < Registers (PDL and Trail are not constrained)
        (if (i32.or
                (i32.or
                (i32.or
                (i32.ge_u (global.get $maxHeap) (global.get $minStack))
                (i32.ge_u (global.get $maxStack) (global.get $minRegister))
                )
                (i32.or
                (i32.ge_u (global.get $minHeap) (global.get $maxHeap))
                (i32.ge_u (global.get $minStack) (global.get $maxStack))
                )
                )
                (i32.or
                (i32.or
                (i32.ge_u (global.get $minRegister) (global.get $maxRegister))
                (i32.ge_u (global.get $minStack) (global.get $maxStack))
                )
                (i32.or
                (i32.ge_u (global.get $minHeap) (global.get $maxHeap))
                (i32.ge_u (global.get $minRegister) (global.get $maxRegister))
                )
                )
            )
            (then (call $warnInvalidMemoryLayout
                (global.get $minHeap) (global.get $maxHeap)
                (global.get $minStack) (global.get $maxStack)
                (global.get $minRegister) (global.get $maxRegister)))
        )

    )
    (func $initialize_environment
        (call $storeToStack (global.get $minStack) (i32.const -1))
        (call $storeToStack (i32.add (global.get $minStack) (global.get $ECP)) (i32.const -1))
        (call $storeToStack (i32.add (global.get $minStack) (global.get $ECPPred)) (i32.const -1))
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
        (call $initialize_globals)
        (call $initialize_limits)
        (call $initialize_environment)
        (global.set $PPred (i32.const -1))
        (global.set $P (i32.const 0))
        (call $evalLoop)
    )
    ;; WAM instruction loop. This halts when $P == -1.

    (func $evalLoop
        (if (i32.gt_s (global.get $P) (i32.const -1))
            (then
                (block
                    (loop
                        (call $evalOp (call $getCode (global.get $P)))
                        (br_if 1 (i32.eq (global.get $P) (i32.const -1)))
                        (br 0)
                    )
                )
            )
        )
    )

    ;; $evalOp dispatches on code($P) to the instruction indicated by the operator code (opCode).
    ;; Each instruction extracts the relevant arguments from code at code($P +1), ..., code($P + n),
    ;; finally resetting $P to $P+n+1 (skipping over the n arguments).

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
    (func $try_me_else_opcode (result i32) (i32.const 15))
    (func $retry_me_else_opcode (result i32) (i32.const 16))
    (func $trust_me_opcode (result i32) (i32.const 17))
    (func $put_constant_opcode (result i32) (i32.const 18))
    (func $get_constant_opcode (result i32) (i32.const 19))
    (func $set_constant_opcode (result i32) (i32.const 20))
    (func $unify_constant_opcode (result i32) (i32.const 21))
    (func $put_list_opcode (result i32) (i32.const 22))
    (func $get_list_opcode (result i32) (i32.const 23))
    (func $set_void_opcode (result i32) (i32.const 24))
    (func $unify_void_opcode (result i32) (i32.const 25))
    (func $execute_opcode (result i32) (i32.const 26))
    (func $put_unsafe_value_opcode (result i32) (i32.const 27))
    (func $set_local_value_opcode (result i32) (i32.const 28))
    (func $unify_local_value_opcode (result i32) (i32.const 29))
    (func $try_opcode (result i32) (i32.const 30))
    (func $retry_opcode (result i32) (i32.const 31))
    (func $trust_opcode (result i32) (i32.const 32))
    (func $switch_on_term_opcode (result i32) (i32.const 33))
    (func $switch_on_constant_opcode (result i32) (i32.const 34))
    (func $switch_on_structure_opcode (result i32) (i32.const 35))

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
        (block
        (block
        (block
        (block
        (block
        (block
            (br_table 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 (local.get $op))
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
        ) ;; 15
            (call $op15) ;; $try_me_else
            return
        ) ;; 16
            (call $op16) ;; $retry_me_else
            return
        ) ;; 17
            (call $op17) ;; $trust_me
            return
         ) ;; 18
             (call $op18) ;; $put_constant
             return
        ) ;; 19
            (call $op19) ;; $get_constant
            return
        ) ;; 20
            (call $op20) ;; $set_constant
            return
        ) ;; 21
            (call $op21) ;; $unify_constant
            return
        ) ;; 22
            (call $op22) ;; $put_list
            return
        ) ;; 23
            (call $op23) ;; $get_list
            return
        ) ;; 24
            (call $op24) ;; $set_void
            return
        ) ;; 25
            (call $op25) ;; $unify_void
            return
        ) ;; 26
            (call $op26) ;; $execute
            return
        ) ;; 27
            (call $op27) ;; $put_unsafe_value
            return
         ) ;; 28
             (call $op28) ;; $set_local_value
             return
         ) ;; 29
             (call $op29) ;; $unify_local_value
             return
         ) ;; 30
             (call $op30) ;; $try
             return
         ) ;; 31
             (call $op31) ;; $retry
             return
         ) ;; 32
             (call $op32) ;; $trust
             return
         ) ;; 33
             (call $op33) ;; $switch_on_term
             return
         ) ;; 34
             (call $op34) ;; $switch_on_constant
             return
         ) ;; 35
             (call $op35) ;; $switch_on_structure
             return
 )

    (func $traceInst0 (param $inst i32)
        (call $traceInstLog0 (local.get $inst) )
    )
    (func $traceInst1 (param $inst i32)
        (call $traceInstLog1 (local.get $inst) (call $getCodeArg (i32.const 1)))
    )
    (func $traceInst2 (param $inst i32)
        (call $traceInstLog2 (local.get $inst) (call $getCodeArg (i32.const 1)) (call $getCodeArg (i32.const 2)))
    )
    (func $traceInst3 (param $inst i32)
        (call $traceInstLog3 (local.get $inst) (call $getCodeArg (i32.const 1)) (call $getCodeArg (i32.const 2)) (call $getCodeArg (i32.const 3)))
    )
    (func $traceInst4 (param $inst i32)
        (call $traceInstLog4 (local.get $inst) (call $getCodeArg (i32.const 1))
         (call $getCodeArg (i32.const 2)) (call $getCodeArg (i32.const 3)) (call $getCodeArg (i32.const 4)))
    )

    (func $traceInstSwitch
        (call $traceInstSwitchLog (global.get $P) (global.get $PPred))
    )
    ;; trace: nop
    (func $op0 ;; No operation
        (call $addToP (i32.const 1))
    )

    ;; trace: put_structure, A1, A2
    (func $op1 ;; $putStructure
        (call $traceInst2 (i32.const 1))
        (call $putStructure (call $getCodeArg (i32.const 1)) (call $getCodeArg (i32.const 2)))
        (call $addToP (i32.const 3))
    )

    ;; trace: setVariable, A1
    (func $op2 ;; setVariable
        (call $traceInst1 (i32.const 2))
        (call $setVariable (call $getCodeArg (i32.const 1)))
        (call $addToP (i32.const 2))
    )

    (func $op3 ;; putVariable
        (call $traceInst3 (i32.const 3))
        (call $putVariable (call $getCodeArg (i32.const 1)) (call $getCodeArg (i32.const 2)) (call $getCodeArg (i32.const 3)))
        (call $addToP (i32.const 4))
    )

    (func $op4 ;; getVariable X/Ytype, permanent or temporary target register ID, source (argument temporary) register ID
        (call $traceInst3 (i32.const 4))
        (call $getVariable (call $getCodeArg (i32.const 1)) (call $getCodeArg (i32.const 2)) (call $getCodeArg (i32.const 3)))
        (call $addToP (i32.const 4))
    )

    (func $op5 ;; setValue
        (call $traceInst1 (i32.const 5))
        (call $setValue (call $getCodeArg (i32.const 1)))
        (call $addToP (i32.const 2))
    )

    (func $op6 ;; putValue
        (call $traceInst3 (i32.const 6))
        (call $putValue (call $getCodeArg (i32.const 1)) (call $getCodeArg (i32.const 2)) (call $getCodeArg (i32.const 3)))
        (call $addToP (i32.const 4))
    )

    (func $op7 ;; getValue
        (call $traceInst2 (i32.const 7))
        (call $getValue (call $getCodeArg (i32.const 1)) (call $getCodeArg (i32.const 2)))
        (if (global.get $fail)
            (then (call $backtrack))
            (else (call $addToP (i32.const 3)))
        )
    )

    (func $op8 ;; getStructure
        (call $traceInst2 (i32.const 8))
        (call $getStructure (call $getCodeArg (i32.const 1)) (call $getCodeArg (i32.const 2)))
        (if (global.get $fail)
            (then (call $backtrack))
            (else (call $addToP (i32.const 3)))
        )
    )

    (func $op9 ;; unifyVariable
        (call $traceInst1 (i32.const 9))
        (call $unifyVariable (call $getCodeArg (i32.const 1)))
        (call $addToP (i32.const 2))
    )

    (func $op10 ;; unifyValue
        (call $traceInst1 (i32.const 10))
        (call $unifyValue (call $getCodeArg (i32.const 1)))
        (if (global.get $fail)
            (then (call $backtrack))
            (else (call $addToP (i32.const 2)))
        )
    )

    (func $op11 ;; call
        (call $traceInst2 (i32.const 11))
        (call $call (call $getCodeArg (i32.const 1)) (call $getCodeArg (i32.const 2)))
        ;; call manages $P directly, resetting it to 0 for the new 'code' for the new predicate.
    )

    (func $op12 ;; proceed
        (call $traceInst0 (i32.const 12))
        (call $proceed)
        ;; proceed manages $P directly, resetting it to $CP.
    )

    (func $op13 ;; allocate
        (call $traceInst0 (i32.const 13))
        (call $allocate)
        (call $addToP (i32.const 1))
    )

    (func $op14 ;; deallocate
        (call $traceInst0 (i32.const 14))
        (call $deallocate)
        ;; deallocate manages $P directly, resetting it to STACK[$E + $ECP].
    )

    (func $op15 ;; try_me_else
        (call $traceInst1 (i32.const 15))
        (call $try_me_else (call $getCodeArg (i32.const 1)))
        (call $addToP (i32.const 2))
    )

    (func $op16 ;; retry_me_else
        (call $traceInst1 (i32.const 16))
        (call $retry_me_else (call $getCodeArg (i32.const 1)))
        (call $addToP (i32.const 2))
    )

    (func $op17 ;; trust_me
        (call $traceInst0 (i32.const 17))
        (call $trust_me)
        (call $addToP (i32.const 1))
    )

    (func $op18 ;; put_constant
        (call $traceInst2 (i32.const 18))
        (call $put_constant (call $getCodeArg (i32.const 1)) (call $getCodeArg (i32.const 2)))
        (call $addToP (i32.const 3))
    )

    (func $op19 ;; get_constant
        (call $traceInst2 (i32.const 19))
        (call $get_constant (call $getCodeArg (i32.const 1)) (call $getCodeArg (i32.const 2)))
        (if (global.get $fail)
            (then (call $backtrack))
            (else (call $addToP (i32.const 3)))
        )
    )

    (func $op20 ;; set_constant
        (call $traceInst1 (i32.const 20))
        (call $set_constant (call $getCodeArg (i32.const 1)))
        (call $addToP (i32.const 2))
    )

    (func $op21 ;; unify_constant
        (call $traceInst1 (i32.const 21))
        (call $unify_constant (call $getCodeArg (i32.const 1)))
        (if (global.get $fail)
            (then (call $backtrack))
            (else (call $addToP (i32.const 2)))
        )
    )

    (func $op22 ;; put_list
        (call $traceInst1 (i32.const 22))
        (call $put_list (call $getCodeArg (i32.const 1)))
        (call $addToP (i32.const 2))
    )

    (func $op23 ;; get_list
        (call $traceInst1 (i32.const 23))
        (call $get_list (call $getCodeArg (i32.const 1)))
        (if (global.get $fail)
            (then (call $backtrack))
            (else (call $addToP (i32.const 2)))
        )
    )

    (func $op24 ;; set_void
        (call $traceInst1 (i32.const 24))
        (call $set_void (call $getCodeArg (i32.const 1)))
        (call $addToP (i32.const 2))
    )

    (func $op25 ;; unify_void
        (call $traceInst1 (i32.const 25))
        (call $unify_void (call $getCodeArg (i32.const 1)))
        (call $addToP (i32.const 2))
    )

    (func $op26 ;; execute
        (call $traceInst1 (i32.const 26))
        (call $execute (call $getCodeArg (i32.const 1)))
        ;; $execute updates $P
    )

    (func $op27 ;; put_unsafe_value
        (call $traceInst2 (i32.const 27))
        (call $put_unsafe_value (call $getCodeArg (i32.const 1)) (call $getCodeArg (i32.const 2)))
        (call $addToP (i32.const 3))
    )

    (func $op28 ;; set_local_value
        (call $traceInst2 (i32.const 28))
        (call $set_local_value (call $getCodeArg (i32.const 1)) (call $getCodeArg (i32.const 2)))
        (call $addToP (i32.const 3))
    )

    (func $op29 ;; unify_local_value
        (call $traceInst2 (i32.const 29))
        (call $unify_local_value (call $getCodeArg (i32.const 1)) (call $getCodeArg (i32.const 2)))
        (if (global.get $fail)
            (then (call $backtrack))
            (else (call $addToP (i32.const 3)))
        )
    )

    (func $op30 ;; try
        (call $traceInst1 (i32.const 30))
        (call $try (call $getCodeArg (i32.const 1)))
        ;; $try sets $P
    )

    (func $op31 ;; retry
        (call $traceInst1 (i32.const 31))
        (call $retry (call $getCodeArg (i32.const 1)))
        ;; $retry sets $P
    )

    (func $op32 ;; trust
        (call $traceInst1 (i32.const 32))
        (call $trust (call $getCodeArg (i32.const 1)))
        ;; $trust sets $P
    )

    (func $op33 ;; switch_on_term
        (call $traceInst4 (i32.const 33))
        (call $switch_on_term (call $getCodeArg (i32.const 1)) (call $getCodeArg (i32.const 2)) (call $getCodeArg (i32.const 3)) (call $getCodeArg (i32.const 4)))
        ;; $switch_on_term sets $P
    )

    (func $op34 ;; switch_on_constant
        (call $traceInstSwitch)
        (call $switch_on_constant (call $getCodeArg (i32.const 1)) (i32.add (global.get $P) (i32.const 2)) )
        ;; $switch_on_constant sets $P
    )

    (func $op35 ;; switch_on_structure
        (call $traceInstSwitch)
        (call $switch_on_structure (call $getCodeArg (i32.const 1)) (i32.add (global.get $P) (i32.const 2)) )
        ;; $switch_on_structure sets $P
    )

    (func $putStructure (param $indicator i32) (param $reg i32)
        ;;call $storeStructureAtHeapTop ;; simplified out according to section 5.1.

        (call $storeIndicatorAtHeapTop (local.get $indicator)) ;; at $H

        (call $storeStructureAtHeapTopToRegister (local.get $reg))

        (call $addToH (i32.const 1)) ;; $H <- $H+1.
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
        (call $storeHeapTopToAddress (local.get $targetAddr))
        (call $storeHeapTopToRegister (local.get $Areg))
        (call $addToH (i32.const 1))
    )

    (func $getVariable (param $variableType i32) (param $regID i32) (param $Areg i32)
        (local $targetAddr i32)
        (local.set $targetAddr (call $resolveRegisterID (local.get $variableType) (local.get $regID)) )
        (call $storeAddressToAddress (call $tempRegisterAddress (local.get $Areg)) (local.get $targetAddr))
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
        (call $storeAddressToAddress (local.get $targetAddr) (call $tempRegisterAddress (local.get $Areg)))
    )
    (func $getValue (param $Xreg i32) (param $Areg i32)
        (call $unify (call $tempRegisterAddress (local.get $Xreg)) (call $tempRegisterAddress (local.get $Areg)))
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
        (local.set $addr (call $deref (call $tempRegisterAddress (local.get $reg))))
        (local.set $term (call $loadFromStore (local.get $addr)))
        (local.set $tag (call $termTag (local.get $term)))

        (if
            (i32.eq (global.get $TAG_REF) (local.get $tag))
            (then (global.set $fail (call $getStructureREF (local.get $indicator) (local.get $addr))))
        (else (if
            (i32.eq (global.get $TAG_STR) (local.get $tag))
            (then (global.set $fail (call $getStructureSTR (local.get $indicator) (call $termVal (local.get $term)))))
        (else (global.set $fail (global.get $TRUE)))
        )))
    )

    (func $getStructureREF (param $indicator i32) (param $addr i32) (result i32)
        (local $nextH i32)
        (local.set $nextH (call $increment (global.get $H)))                        ;; nextH <- H + 1
        (call $storeToHeap (global.get $H) (call $tagStructure (local.get $nextH))) ;; HEAP[H]􏰃 <- <STR, nextH>
        (call $storeToHeap (local.get $nextH) (call $tagPredicate (local.get $indicator)) )         ;; HEAP[nextH􏰌]􏰃 <- <PRE, f/n>
        (call $bind (local.get $addr) (global.get $H))                              ;; bind(addr, H)
        (call $addToH (i32.const 2))                                                ;; H <- H+2
        (global.set $S (local.get $nextH))                                         ;; S <- H+1 (H+1 is the location where the first argument of the structure will be stored).
        (global.set $mode (global.get $WRITE_MODE))                                 ;; mode <- write

        (return (global.get $FALSE)) ;; 'fail' is false.
    )

    (func $getStructureSTR (param $indicator i32) (param $a i32) (result i32)

        (if (result i32)
            (i32.eq (call $termVal (call $loadFromHeap (local.get $a))) (local.get $indicator)) ;; if(VAL(HEAP[a]) = f/n) then
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
            (then (call $storeAddressToAddress (global.get $S) (call $tempRegisterAddress (local.get $reg))) )
        (else (if (i32.eq (global.get $mode) (global.get $WRITE_MODE))
            (then (call $setVariable (local.get $reg)) )
            )
        )
        )
        (global.set $S (call $increment (global.get $S)))
    )

    (func $unifyValue (param $reg i32)
        (if (i32.eq (global.get $mode) (global.get $READ_MODE))
            (then (call $unify  (call $tempRegisterAddress (local.get $reg)) (global.get $S)) )
        (else (if (i32.eq (global.get $mode) (global.get $WRITE_MODE))
            (then (call $setValue (local.get $reg)) )
            )
        )
        )
        (global.set $S (call $increment (global.get $S)))
    )

    ;; $N is the number of environment slots needed for permanent variables when the $call is made.
    ;; $N is used for 'environment trimming'. It is accessed by the $allocate instruction that
    ;; implements any clause for $pred.
    (func $call (param $pred i32) (param $N i32)
        (local $arity i32)
        (local.set $arity (call $getIndicatorArity (local.get $pred)))
        (if (i32.eq (local.get $arity) (i32.const -1))
            (then
                (call $backtrack)
                return
            )
        )
        (global.set $CP (i32.add (global.get $P) (i32.const 3)))
        (global.set $CPPred (global.get $PPred))
        (call $setCode (local.get $pred))
        (global.set $PPred (local.get $pred))
        (global.set $P (i32.const 0))
        (global.set $num_of_args (local.get $arity))
    )

    (func $proceed
        (call $incrementInferences)
        (global.set $PPred (global.get $CPPred))
        (if (i32.gt_s (global.get $PPred) (i32.const -1))
            (then
                (call $setCode (global.get $PPred))
                (global.set $P (global.get $CP))
            )
            (else
                (global.set $P (i32.const -1)) ;; halt WAM
            )
        )

    )

    (func $allocate
        (local $newE i32)
        (local.set $newE (call $nextStackFrameSlot))
        (call $create_environment_frame (local.get $newE))
        (global.set $E (local.get $newE))
    )

;;    (func $deallocate
;;        (call $incrementInferences)
;;        (global.set $PPred (call $loadFromStack (i32.add (global.get $E) (global.get $ECPPred)) ))
;;        (if (i32.eq (global.get $PPred) (i32.const -1))
;;            (then (global.set $P (i32.const -1))) ;; causes evalLoop to halt.
;;            (else
;;                (call $setCode (global.get $PPred))
;;                (global.set $P (call $loadFromStack (i32.add (global.get $E) (global.get $ECP)) ))
;;            )
;;        )
;;
;;        (global.set $E (call $loadFromStack (global.get $E)))
;;    )
    (func $deallocate
        (global.set $CPPred (call $loadFromStack (i32.add (global.get $E) (global.get $ECPPred)) ))
        (global.set $CP (call $loadFromStack (i32.add (global.get $E) (global.get $ECP)) ))

        (global.set $E (call $loadFromStack (global.get $E)))
        (call $addToP (i32.const 1))
    )

    (func $try_me_else (param $label i32)
        (local $newB i32)
        (local.set $newB (call $nextStackFrameSlot))
        (call $create_choicepoint_frame (local.get $newB) (local.get $label))
        (global.set $B (local.get $newB))
        (global.set $HB (global.get $H))
    )

    (func $retry_me_else (param $label i32)
        (local $arity i32) (local $lastFrameArg i32) (local $backTR i32)
        (local.set $arity (call $loadFromStack (global.get $B)))
        (call $storeRegistersFromChoicepoint)
        (local.set $lastFrameArg (i32.add (global.get $B) (local.get $arity)))
        (global.set $E (call $loadFromStack (i32.add (local.get $lastFrameArg) (global.get $BkCE))))
        (global.set $CP (call $loadFromStack (i32.add (local.get $lastFrameArg) (global.get $BkCP))))
        (call $storeToStack
            (i32.add (local.get $lastFrameArg) (global.get $BkBP))
            (local.get $label))
        (call $storeToStack
            (i32.add (local.get $lastFrameArg) (global.get $BkBPred))
            (global.get $PPred))
        (local.set $backTR (call $loadFromStack (i32.add (local.get $lastFrameArg) (global.get $BkTR))))
        (call $unwindTrail (local.get $backTR) (global.get $TR) )
        (global.set $TR (local.get $backTR))
        (global.set $H (call $loadFromStack (i32.add (local.get $lastFrameArg) (global.get $BkH))))
        (global.set $HB (global.get $H))
    )

    (func $trust_me
        (local $arity i32) (local $lastFrameArg i32) (local $newArity i32) (local $newLastFrameArg i32) (local $backTR i32)
        (local.set $arity (call $loadFromStack (global.get $B)))
        (call $storeRegistersFromChoicepoint)
        (local.set $lastFrameArg (i32.add (global.get $B) (local.get $arity)))
        (global.set $E (call $loadFromStack (i32.add (local.get $lastFrameArg) (global.get $BkCE))))
        (global.set $CP (call $loadFromStack (i32.add (local.get $lastFrameArg) (global.get $BkCP))))
        (local.set $backTR (call $loadFromStack (i32.add (local.get $lastFrameArg) (global.get $BkTR))))
        (call $unwindTrail (local.get $backTR) (global.get $TR) )
        (global.set $TR (local.get $backTR))
        (global.set $H (call $loadFromStack (i32.add (local.get $lastFrameArg) (global.get $BkH))))
        (global.set $B (call $loadFromStack (i32.add (local.get $lastFrameArg) (global.get $BkB))))
        ;; $newArity and $newLastFrameArg are based on correction in wamerratum.
        (local.set $newArity (call $loadFromStack (global.get $B))) ;; $B was changed by preceding global.set.
        (local.set $newLastFrameArg (i32.add (global.get $B) (local.get $newArity))) ;; $B was changed by preceding global.set.
        (global.set $HB (call $loadFromStack (i32.add (local.get $newLastFrameArg) (global.get $BkH))))
    )

    (func $put_constant (param $c i32) (param $reg i32)
        (call $storeToRegister (local.get $reg) (call $tagConstant (local.get $c)))
    )

    (func $get_constant_address (param $c i32) (param $addr i32)
        (local $derefedAddr i32)
        (local.set $derefedAddr (call $deref (local.get $addr)))
        (call $get_constant_direct (local.get $c) (local.get $derefedAddr))
    )
    (func $get_constant_direct (param $c i32) (param $addr i32)
        (local $term i32) (local $tag i32)
        (local.set $term (call $loadFromStore (local.get $addr)))
        (local.set $tag (call $termTag (local.get $term)))
        (if (i32.eq (global.get $TAG_REF) (local.get $tag))
            (then
                (call $storeToAddress (local.get $addr) (call $tagConstant (local.get $c)))
                (call $trail (local.get $addr))
            )
            (else
                (if (i32.eq (global.get $TAG_CON) (local.get $tag))
                    (then
                        (if (i32.ne (local.get $c) (call $termVal (local.get $term)))
                            (then (global.set $fail (global.get $TRUE)))
                            (else (global.set $fail (global.get $FALSE)))
                        )
                    )
                    (else
                        (global.set $fail (global.get $TRUE))
                    )
                )
            )
        )
    )
    (func $get_constant (param $c i32) (param $reg i32)
        (local $addr i32)
        (local.set $addr (call $deref (call $tempRegisterAddress (local.get $reg))))
        (call $get_constant_direct (local.get $c) (local.get $addr))
    )

    (func $set_constant (param $c i32)
        (call $storeConstantAtHeapTop (local.get $c))
        (call $addToH (i32.const 1))
    )

    (func $unify_constant (param $c i32)
        (if (i32.eq (global.get $mode) (global.get $READ_MODE))
            (then
                (call $get_constant_address (local.get $c) (global.get $S))
            )
            (else ;; WRITE_MODE
                (call $set_constant (local.get $c))
            )
        )
    )

    (func $put_list (param $reg i32)
        (call $storeToRegister (local.get $reg) (call $tagList (global.get $H)))
    )

    (func $get_list (param $reg i32)
        (local $addr i32) (local $term i32) (local $tag i32)
        (local.set $addr (call $deref (call $tempRegisterAddress (local.get $reg))))
        (local.set $term (call $loadFromStore (local.get $addr)))
        (local.set $tag (call $termTag (local.get $term)))

        (if
            (i32.eq (global.get $TAG_REF) (local.get $tag))
            (then (global.set $fail (call $getListREF (local.get $addr))))
        (else (if
            (i32.eq (global.get $TAG_LIS) (local.get $tag))
            (then (global.set $fail (call $getListLIS (call $termVal (local.get $term)))))
        (else (global.set $fail (global.get $TRUE)))
        )))
    )
    (func $getListREF (param $addr i32) (result i32)
        (local $nextH i32)
        (local.set $nextH (call $increment (global.get $H)))                    ;; nextH <- H + 1
        (call $storeToHeap (global.get $H) (call $tagList (local.get $nextH)))  ;; HEAP[H]􏰃 <- <STR, nextH>
        (call $bind (local.get $addr) (global.get $H))                          ;; bind(addr, H)
        (call $addToH (i32.const 1))                                            ;; H <- H+2
        (global.set $S (local.get $nextH))                                     ;; S <- H+1 (H+1 is the location where the first argument of the list will be stored).
        (global.set $mode (global.get $WRITE_MODE))                             ;; mode <- write

        (return (global.get $FALSE)) ;; 'fail' is false.
    )

    (func $getListLIS (param $a i32) (result i32)
        (global.set $S (local.get $a))                  ;; S􏰃 <- a
        (global.set $mode (global.get $READ_MODE))      ;; mode <- READ
        (return (global.get $FALSE))                    ;; return fail is false.
    )

    (func $set_void (param $n i32)
        (local $i i32)
        (local $limit i32)
        (local.set $i (global.get $H))
        (local.set $limit (i32.add (global.get $H) (i32.sub (local.get $n) (i32.const 1)))) ;; $limit = $H + $n - 1.
        (if (i32.le_u (local.get $i) (local.get $limit))
            (then
                (block
                    (loop
                        (call $storeToHeap
                            (local.get $i)
                            (call $tagReference (local.get $i)))
                        (br_if 1 (i32.eq (local.get $i) (local.get $limit)))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br 0)
                    )
                )
            )
        )
        (call $addToH (local.get $n))
    )

    (func $unify_void (param $n i32)
        (if (i32.eq (global.get $mode) (global.get $READ_MODE))
            (then (global.set $S (i32.add (global.get $S) (local.get $n))))
        (else ;; (if (i32.eq (global.get $mode) (global.get $WRITE_MODE))
            ;;(then
            (call $set_void (local.get $n))
            ;; ))
        ))
    )

    (func $execute (param $pred i32)
        (local $arity i32)
        (call $incrementIncompleteInferences)
        (local.set $arity (call $getIndicatorArity (local.get $pred)))
        (if (i32.eq (local.get $arity) (i32.const -1))
            (then
                (call $backtrack)
                return
            )
        )
        (call $setCode (local.get $pred))
        (global.set $PPred (local.get $pred))
        (global.set $P (i32.const 0))
        (global.set $num_of_args (local.get $arity))
    )

    (;
    "...a permanent variable which is initialized by a put_variable (i.e., which first
    occurs as the argument of a body goal) is called an unsafe variable." [AK99, p.64]

    put_unsafe_value is used for Yk (instead of put_value) on the first reference to Yk
    in preparing for the call/execute of the last goal that references Yk.
    ;)
    (func $put_unsafe_value (param $permanentReg i32) (param $Areg i32)
        (local $sourceAddr i32)
        (local.set $sourceAddr (call $deref (call $resolveRegisterID (i32.const 1) (local.get $permanentReg)) ))
        (if (i32.lt_u (local.get $sourceAddr) (global.get $E))
            (then (call $storeToRegister (local.get $Areg) (call $loadFromAddress (local.get $sourceAddr))))
            (else
                (call $storeReferenceAtHeapTop)
                (call $bind (local.get $sourceAddr) (global.get $H))
                (call $storeHeapTopToRegister (local.get $Areg))
                (call $addToH (i32.const 1))
            )
        )
    )

    (;
    "Specifically, set_value Vn (resp., unify_value Vn) is unsafe whenever
    the variable Vn has not been initialized in this clause with
    set_variable or unify_variable, nor, if Vn is temporary, with put_variable." [AK99, p.68]
    AK never explicitly says what is meant be 'local' It appears that a value is
    local to a clause if it meets the conditions of the quoted statement: a variable is local
    to a clause if it has been initialized in that clause with set_variable, unify_variable,
    (for Vn temporary) put_variable.
    The set_value Vn and unify_value Vn instructions are only safe in a clause if Vn is local or if
    set_local_value Vn or unify_local_value Vn has been used previously in that clause (at which point
    Vn becomes local).
    ;)
    (func $set_local_value (param $variableType i32) (param $reg i32)
        (local $sourceAddr i32)
        (local.set $sourceAddr (call $deref (call $resolveRegisterID (local.get $variableType) (local.get $reg)) ))
        (if (i32.lt_u (local.get $sourceAddr) (global.get $H))
            (then (call $storeHeapToHeap (local.get $sourceAddr) (global.get $H))) ;;HEAP[H] <- HEAP[addr]
            (else
                (call $storeReferenceAtHeapTop)
                (call $bind (local.get $sourceAddr) (global.get $H))
                (call $addToH (i32.const 1))
            )
        )
    )

    (func $unify_local_value (param $variableType i32) (param $reg i32)
        (if (i32.eq (global.get $mode) (global.get $READ_MODE))
            (then (call $unify  (call $resolveRegisterID (local.get $variableType) (local.get $reg)) (global.get $S)) )
        (else (if (i32.eq (global.get $mode) (global.get $WRITE_MODE))
            (then (call $set_local_value (local.get $variableType) (local.get $reg)) )
            )
        )
        )
        (global.set $S (call $increment (global.get $S)))
    )

;; try L, retry L, trust L

    (func $try (param $label i32)
        (local $newB i32)
        (local.set $newB (call $nextStackFrameSlot))
        (call $create_choicepoint_frame (local.get $newB) (i32.add (global.get $P) (i32.const 2)))
        (global.set $B (local.get $newB))
        (global.set $HB (global.get $H))
        (global.set $P (local.get $label))
    )

    (func $retry (param $label i32)
        (local $arity i32) (local $lastFrameArg i32) (local $backTR i32)
        (local.set $arity (call $loadFromStack (global.get $B)))
        (call $storeRegistersFromChoicepoint)
        (local.set $lastFrameArg (i32.add (global.get $B) (local.get $arity)))
        (global.set $E (call $loadFromStack (i32.add (local.get $lastFrameArg) (global.get $BkCE))))
        (global.set $CP (call $loadFromStack (i32.add (local.get $lastFrameArg) (global.get $BkCP))))
        (call $storeToStack
            (i32.add (local.get $lastFrameArg) (global.get $BkBP))
            (i32.add (global.get $P) (i32.const 2)))
        (call $storeToStack
            (i32.add (local.get $lastFrameArg) (global.get $BkBPred))
            (global.get $PPred))
        (local.set $backTR (call $loadFromStack (i32.add (local.get $lastFrameArg) (global.get $BkTR))))
        (call $unwindTrail (local.get $backTR) (global.get $TR) )
        (global.set $TR (local.get $backTR))
        (global.set $H (call $loadFromStack (i32.add (local.get $lastFrameArg) (global.get $BkH))))
        (global.set $HB (global.get $H))
        (global.set $P (local.get $label))
    )

    (func $trust (param $label i32)
        (call $trust_me)
        (global.set $P (local.get $label))
    )

;; switch_on_term V,C,L,S
;; switch_on_constant N,T
;; switch_on_structure N,T

    (func $switch_on_term (param $V i32) (param $C i32) (param $L i32) (param $S i32)
        (local $a1Addr i32) (local $term i32) (local $tag i32) (local $next i32)
        (local.set $a1Addr (call $deref (call $tempRegisterAddress (i32.const 1))) ) ;; deref(A1)
        (local.set $term (call $loadFromAddress (local.get $a1Addr)))
        (local.set $tag (call $termTag (local.get $term)))
        (local.set $next
            (if (result i32)
                (i32.eq (local.get $tag) (global.get $TAG_REF))
                (then (local.get $V))
            (else (if (result i32) (i32.eq (local.get $tag) (global.get $TAG_CON))
                (then (local.get $C))
            (else (if (result i32) (i32.eq (local.get $tag) (global.get $TAG_LIS))
                (then (local.get $L))
            (else (if (result i32) (i32.eq (local.get $tag) (global.get $TAG_STR))
                (then (local.get $S))
                (else (call $warnInvalidSwitchTag (local.get $tag)) (return (i32.const 0)))
            )) )) ))
            )
        )
        (if (i32.eqz (local.get $next))
            (then (call $backtrack))
            (else (global.set $P (local.get $next)))
        )
    )

    (func $switch_on_constant (param $N i32) (param $T i32)
        (local $a1Addr i32) (local $term i32) (local $val i32) (local $inst i32)
        (local.set $a1Addr (call $deref (call $tempRegisterAddress (i32.const 1))) ) ;; deref(A1)
        (local.set $term (call $loadFromAddress (local.get $a1Addr)))
        (local.set $val (call $termVal (local.get $term)))
        (local.set $inst (call $get_hash (local.get $val) (local.get $T) (local.get $N))) ;; $inst === 0 -> not found
        (if (i32.eqz (local.get $inst))
            (then (call $backtrack))
            (else (global.set $P (local.get $inst)))
        )
    )

    (func $switch_on_structure (param $N i32) (param $T i32)
        (local $a1Addr i32) (local $term i32) (local $val i32) (local $inst i32)
        (local.set $a1Addr (call $deref (call $tempRegisterAddress (i32.const 1))) ) ;; deref(A1)
        (local.set $term (call $loadFromAddress (local.get $a1Addr)))
        (local.set $val (call $termVal (local.get $term)))
        (local.set $inst (call $get_hash (local.get $val) (local.get $T) (local.get $N))) ;; $inst === 0 -> not found
        (if (i32.eqz (local.get $inst))
            (then (call $backtrack))
            (else (global.set $P (local.get $inst)))
        )
    )

    (export "setH" (func $setH))
    (export "shiftTag" (func $shiftTag))
    (export "tagStructure" (func $tagStructure))
    (export "getStructure" (func $getStructure))
    (export "addToH" (func $addToH))
    (export "bind" (func $bind))
    (export "getInferences" (func $getInferences))

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
    (export "try_me_else_opcode" (func $try_me_else_opcode))
    (export "retry_me_else_opcode" (func $retry_me_else_opcode))
    (export "trust_me_opcode" (func $trust_me_opcode))
    (export "put_constant_opcode" (func $put_constant_opcode))
    (export "get_constant_opcode" (func $get_constant_opcode))
    (export "set_constant_opcode" (func $set_constant_opcode))
    (export "unify_constant_opcode" (func $unify_constant_opcode))
    (export "put_list_opcode" (func $put_list_opcode))
    (export "get_list_opcode" (func $get_list_opcode))
    (export "set_void_opcode" (func $set_void_opcode))
    (export "unify_void_opcode" (func $unify_void_opcode))
    (export "execute_opcode" (func $execute_opcode))
    (export "put_unsafe_value_opcode" (func $put_unsafe_value_opcode))
    (export "set_local_value_opcode" (func $set_local_value_opcode))
    (export "unify_local_value_opcode" (func $unify_local_value_opcode))
    (export "try_opcode" (func $try_opcode))
    (export "retry_opcode" (func $retry_opcode))
    (export "trust_opcode" (func $trust_opcode))
    (export "switch_on_term_opcode" (func $switch_on_term_opcode))
    (export "switch_on_constant_opcode" (func $switch_on_constant_opcode))
    (export "switch_on_structure_opcode" (func $switch_on_structure_opcode))

    (export "run" (func $run))
)
