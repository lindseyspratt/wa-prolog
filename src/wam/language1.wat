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

;)
(module
    (import "js" "mem" (memory 1))
    (import "js" "table" (table 2 funcref))
    (import "js" "maxRegister" (global $maxRegister i32))
    (import "js" "pdlStart" (global $pdlStart i32))
    (import "js" "maxPDL" (global $maxPDL i32))
    (import "js" "heapStart" (global $heapStart i32))
    (import "js" "minHeap" (global $minHeap i32))
    (import "js" "lookupAtom" (func $lookupAtom (param i32) (param i32) (result i32)))
    (import "js" "getIndicatorArity" (func $getIndicatorArity (param i32) (result i32)))
    (import "js" "getCode" (func $getCode (param i32) (result i32)))
    (import "js" "indicator_a/0" (global $predA_0 i32))
    (import "js" "indicator_f/1" (global $predF_1 i32))
    (import "js" "indicator_h/2" (global $predH_2 i32))
    (import "js" "indicator_p/3" (global $predP_3 i32))

    (func $predP_3
        (call $program2_10)
    )

  (elem (i32.const 0) $predP_3)
  (global $predP_3Program i32 (i32.const 0))

    (type $void (func))

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
    (global $P (mut i32) (i32.const 0)) ;; $P is the 'program' instruction counter. The instruction is at (call $getCode (global.get $P)).
    (global $PDL (mut i32) (i32.const -1)) ;; $PDL is offset of the top of a push down list for use in unification.
    (global $S (mut i32) (i32.const 0)) ;; $S is current structure argument address (on heap). Used when $mode = $READ_MODE.

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
;;        global.get $heapStart
;;        i32.add
    )
    (func $wordToPDLByteOffset (param $val i32) (result i32)
        local.get $val
        call $wordToByteOffset
        global.get $pdlStart
        i32.add
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
    (func $loadFromAddress (param $addr i32) (result i32)
        (i32.load (call $wordToByteOffset (local.get $addr)))
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

    (func $putVariable (param $Xreg i32) (param $Areg i32)
        call $storeReferenceAtHeapTop
        (call $storeHeapTopToRegister (local.get $Xreg))
        (call $storeHeapTopToRegister (local.get $Areg))
        (call $addToH (i32.const 1))
    )
    (func $getVariable (param $Xreg i32) (param $Areg i32)
        (call $storeAddressToAddress (local.get $Areg) (local.get $Xreg))
    )

    (func $setValue (param $reg i32)
        local.get $reg
        call $storeRegisterToHeapTop

        i32.const 1
        call $addToH
    )
    (func $putValue (param $Xreg i32) (param $Areg i32)
        (call $storeAddressToAddress (local.get $Xreg) (local.get $Areg))
    )
    (func $getValue (param $Xreg i32) (param $Areg i32)
        (call $unify (local.get $Xreg) (local.get $Areg))
    )
    (func $getStructure (param $indicator i32) (param $reg i32)
        (local $addr i32) (local $tag i32)
        (local.set $addr (call $deref (local.get $reg)))
        (local.set $tag (call $termTag (local.get $addr)))

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
            (global.set $fail (call $getStructureSTR (local.get $indicator) (call $termVal (local.get $addr))))
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
        (call_indirect (type $void) (local.get $pred))
    )

    (func $proceed)

    (func $query2_9
        (call $putVariable (i32.const 4) (i32.const 1))
        (call $putStructure (global.get $predH_2) (i32.const 2))
        (call $setValue (i32.const 4))
        (call $setVariable (i32.const 5))
        (call $putStructure (global.get $predF_1) (i32.const 3))
        (call $setValue (i32.const 5))
        (call $call (global.get $predP_3Program))
    )

    (func $test
        (call $setVariable (i32.const 1))
        (call $setVariable (i32.const 2))
        (call $setVariable (i32.const 3))
        (call $program2_10)
    )

    (func $program2_10 ;; indirect target of $predP_3Program in table.
        (call $getStructure (global.get $predF_1) (i32.const 1))
        (call $unifyVariable (i32.const 4))
        (call $getStructure (global.get $predH_2) (i32.const 2))
        (call $unifyVariable (i32.const 5))
        (call $unifyVariable (i32.const 6))
        (call $getValue (i32.const 5) (i32.const 3))
        (call $getStructure (global.get $predF_1) (i32.const 6))
        (call $unifyVariable (i32.const 7))
        (call $getStructure (global.get $predA_0) (i32.const 7))
        (call $proceed)
    )
(;
procedure unify(a1, a2:address) :
    push(a1, PDL); push(a2, PDL);
    fail <- false;
    while(! (empty(PDL) \/ fail) ) {
        d1 <- deref(pop(PDL)); d2 <- deref(pop(PDL));
        if d1 != d2 then
          begin
            <t1,v1> <- STORE[d1]; <t2,v2> <- STORE[d2];
            if t1 = REF \/ t2 = REF
              then bind(d1, d2)
              else
                begin
                  f1/n1 <- STORE[v1]; f2/n2 <- STORE[v2];
                  if f1 = f2 /\ n1 = n2
                    then
                      for i = 1 to n1 do
                        begin
                          push(v1 + i, PDL)
                          push(v2 + i, PDL)
                        end
                    else fail <- true
                end
          end
    end
end unify

;)
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
    (;
    WAM instruction loop:
    function inst(P) {
        let code = getCode(P);
        switch(code) {
            ...
            case K: nextP = instK(P); continue;
            ...
        }
        inst(nextP);
    }

    function instSetValue(P) {
        let arg = getCode(P+1);
        storeRegisterToHeap(arg);
        H ++;
        return P+2;
    ;)
    (export "setH" (func $setH))
    (export "shiftTag" (func $shiftTag))
    (export "tagInteger" (func $tagInteger))
    (export "tagStructure" (func $tagStructure))
    (export "storeStructureMem" (func $storeStructureAtHeapTop))
    (export "storeReferenceAtHeapTop" (func $storeReferenceAtHeapTop))
    (export "getStructure" (func $getStructure))
    (export "addToH" (func $addToH))
    (export "bind" (func $bind))
    (export "test" (func $test))
    (export "program2_10" (func $program2_10))
)
