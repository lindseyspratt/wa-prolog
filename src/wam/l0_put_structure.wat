(module
    (import "js" "mem" (memory 1))

    (func $shiftTag (param $tag i32) (result i32)
        local.get $tag
        i32.const 27 ;; WORD_BITS
        i32.shl)
    (func $tagInteger (param $val i32) (result i32)
        i32.const 3
        call $shiftTag
        local.get $val
        i32.xor
    )
    (func $tagStructure (param $val i32) (result i32)
        i32.const 1
        call $shiftTag
        local.get $val
        i32.xor
    )
    (func $storeStructureMem (param $H i32)
        local.get $H
        i32.const 4
        i32.mul

        i32.const 1
        local.get $H
        i32.add

        call $tagStructure

        i32.store ;; store <STR, H+1> at byte H*4
    )
    (export "shiftTag" (func $shiftTag))
    (export "tagInteger" (func $tagInteger))
    (export "tagStructure" (func $tagStructure))
    (export "storeStructureMem" (func $storeStructureMem))
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