(module
  (import "js" "increment" (func $increment (param i32) (result i32)))
  (import "console" "log" (func $log (param i32)))
  (func (export "increment")
    i32.const 3
    call $increment
    call $log))
