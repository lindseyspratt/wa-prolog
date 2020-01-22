(module
    (import "js" "mem" (memory 1))
    (import "js" "table" (table 1 funcref))
    (elem (i32.const 1)
     $loadToRegister1 $loadToRegister2 $loadToRegister3 $loadToRegister4 $loadToRegister5
     $loadFromRegister1 $loadFromRegister3 $loadFromRegister3 $loadFromRegister5 $loadFromRegister5
    )
    (type $i32 (func (param i32)))
    (type $void_to_i32 (func (result i32)))

    (global $LOAD_FROM_REGISTER_OFFSET i32 (i32.const 5)) ;; 5 registers, X1-X5

    (global $TAG_REF i32 (i32.const 0))
    (global $TAG_STR i32 (i32.const 1))
    (global $TAG_LST i32 (i32.const 2))
    (global $TAG_INT i32 (i32.const 3))
    (global $TAG_ATM i32 (i32.const 4))
    (global $TAG_FLT i32 (i32.const 5))

    (global $H (mut i32) (i32.const 0))
    (global $X1 (mut i32) (i32.const 0))
    (global $X2 (mut i32) (i32.const 0))
    (global $X3 (mut i32) (i32.const 0))
    (global $X4 (mut i32) (i32.const 0))
    (global $X5 (mut i32) (i32.const 0))

    (func $getH (result i32)
        global.get $H
    )

    (func $setH (param $val i32)
        local.get $val
        global.set $H
    )

    (func $shiftTag (param $tag i32) (result i32)
        local.get $tag
        i32.const 27 ;; WORD_BITS
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
    (func $wordToByteOffset (param $val i32) (result i32)
        local.get $val
        i32.const 4
        i32.mul
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
    (func $storeStructureMem
        global.get $H
        call $wordToByteOffset

        global.get $H
        call $increment

        call $tagStructure

        i32.store ;; store <STR, H+1> at byte H*4
    )
    (func $storeIndicatorMem (param $fn i32)
        global.get $H
        call $increment
        call $wordToByteOffset

        local.get $fn

        i32.store ;; store f/n at byte (H+1)*4
    )
    (func $storeReferenceMem
        global.get $H
        call $wordToByteOffset

        global.get $H
        call $tagReference

        i32.store ;; store <REF, H> at byte H*4
    )
    (func $storeRegisterToHeap (param $val i32)
        global.get $H
        local.get $val
        call $loadFromRegister
        i32.store
    )
    (func $loadFromRegister1 (result i32)
        global.get $X1
    )
    (func $loadFromRegister2 (result i32)
        global.get $X2
    )
    (func $loadFromRegister3 (result i32)
        global.get $X4
    )
    (func $loadFromRegister4 (result i32)
        global.get $X4
    )
    (func $loadFromRegister5 (result i32)
        global.get $X5
    )
    (func $loadFromRegister (param $reg i32) (result i32)
        local.get $reg
        global.get $LOAD_FROM_REGISTER_OFFSET ;; skip the $loadToRegisterI functions in the table.
        i32.add

        call_indirect (type $void_to_i32)
    )
    (func $loadHeapToRegister (param $val i32)
        global.get $H
        i32.load
        local.get $val
        call $loadToRegister
    )
    (func $loadToRegister1 (param $val i32)
        local.get $val
        global.set $X1
    )
    (func $loadToRegister2 (param $val i32)
        local.get $val
        global.set $X2
    )
    (func $loadToRegister3 (param $val i32)
        local.get $val
        global.set $X3
    )
    (func $loadToRegister4 (param $val i32)
        local.get $val
        global.set $X4
    )
    (func $loadToRegister5 (param $val i32)
        local.get $val
        global.set $X5
    )
    (func $loadToRegister (param $reg i32) (param $val i32)
        local.get $reg
        local.get $val
        call_indirect (type $i32)
    )
    (func $putStructure (param $indicator i32) (param $reg i32)
        call $storeStructureMem

        local.get $indicator
        call $storeIndicatorMem

        local.get $reg
        call $loadHeapToRegister

        i32.const 2
        call $addToH
    )

    (func $setVariable (param $reg i32)
        call $storeReferenceMem

        local.get $reg
        call $loadHeapToRegister

        i32.const 1
        call $addToH
    )

    (func $setValue (param $reg i32)
        local.get $reg
        call $storeRegisterToHeap

        i32.const 1
        call $addToH
    )

(;
put structure h􏰐􏰈􏰒/2, X􏰍3       % ?-X3=􏰍h
set variable X􏰈2             %      (􏰚Z,
set variable X􏰏5             %        W),
put structure f/1, 􏰐􏰌􏰒 X􏰎4      %   X4=f
set value X􏰏5                %      (W),
put structure p􏰐􏰍􏰒/3, X􏰌1       %   X1=p
set value X􏰈2                %      (Z,
set value X􏰍3                %        X3,
set value X􏰎4                %         X4).
          Figure 2.3: Compiled code for L0 􏰋query ?-p(􏰚Z, h(􏰚Z,W), f(W))
;)
    (global $predF_1 i32 (i32.const 1))
    (global $predH_2 i32 (i32.const 2))
    (global $predP_3 i32 (i32.const 3))

    (func $query2_3
        global.get $predH_2
        i32.const 3
        call $putStructure

        i32.const 2
        call $setVariable

        i32.const 5
        call $setVariable

        global.get $predF_1
        i32.const 4
        call $putStructure

        i32.const 5
        call $setValue

        global.get $predP_3
        i32.const 1
        call $putStructure

        i32.const 2
        call $setValue

        i32.const 3
        call $setValue

        i32.const 4
        call $setValue
    )
    (export "setH" (func $setH))
    (export "shiftTag" (func $shiftTag))
    (export "tagInteger" (func $tagInteger))
    (export "tagStructure" (func $tagStructure))
    (export "storeStructureMem" (func $storeStructureMem))
    (export "query2_3" (func $query2_3))
)
(;
const TAG_REF = 0; // 0x00000000
const TAG_STR = 1; // 0x08000000
const TAG_LST = 2; // 0x10000000
const TAG_INT = 3; // 0x18000000
const TAG_ATM = 4; // 0x20000000
const TAG_FLT = 5; // 0x28000000
const WORD_BITS = 27;
;)