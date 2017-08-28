
open Merkle
open Values
open Source

type vm = {
  code : inst array;
  stack : value array;
  memory : value array;
  break_stack : (int*int) array;
(*  call_stack : (int*int*int) array; *)
  call_stack : int array;
  globals : value array;
  calltable : int array;
  mutable pc : int;
  mutable stack_ptr : int;
  mutable memsize : int;
  mutable break_ptr : int;
  mutable call_ptr : int;
}

exception VmError

let value_bool v = not (v = I32 0l)

let value_to_int = function
 | I32 i -> Int32.to_int i
 | I64 i -> Int64.to_int i
 | _ -> 0

let inc_pc vm = vm.pc <- vm.pc+1

let vm_step vm = match vm.code.(vm.pc) with
 | NOP -> inc_pc vm
 | UNREACHABLE -> raise VmError
 | JUMP x -> vm.pc <- x
 | JUMPI x -> vm.pc <- (if value_bool (vm.stack.(vm.stack_ptr-1)) then x else vm.pc + 1)
 | CALL x ->
   (* vm.call_stack.(vm.call_ptr) <- (vm.pc, vm.stack_ptr, vm.break_ptr);  I now guess that it won't need these *)
   vm.call_stack.(vm.call_ptr) <- vm.pc;
   vm.call_ptr <- vm.call_ptr + 1;
   vm.pc <- x
 | LABEL _ -> raise VmError (* these should have been processed away *)
 | PUSHBRK x ->
   inc_pc vm;
   vm.break_stack.(vm.break_ptr) <- (x, vm.stack_ptr);
   vm.break_ptr <- vm.break_ptr + 1
 | POPBRK ->
   inc_pc vm;
   vm.break_ptr <- vm.break_ptr - 1
 | BREAK ->
   let loc, sptr = vm.break_stack.(vm.break_ptr-1) in
   vm.break_ptr <- vm.break_ptr - 1;
   vm.stack_ptr <- sptr;
   vm.pc <- loc
 | RETURN ->
   vm.pc <- vm.call_stack.(vm.call_ptr-1);
   vm.call_ptr <- vm.call_ptr - 1
 | LOAD x ->
   inc_pc vm;
   vm.stack.(vm.stack_ptr-1) <- vm.memory.(value_to_int vm.stack.(vm.stack_ptr-1) + x)
 | STORE x ->
   inc_pc vm;
   vm.memory.(value_to_int vm.stack.(vm.stack_ptr-1) + x) <- vm.stack.(vm.stack_ptr-2);
   vm.stack_ptr <- vm.stack_ptr - 1
 | DROP ->
   inc_pc vm;
   vm.stack_ptr <- vm.stack_ptr - 1
 | DUP x ->
   inc_pc vm;
   vm.stack.(vm.stack_ptr) <- vm.stack.(vm.stack_ptr-x);
   vm.stack_ptr <- vm.stack_ptr + 1
 | SWAP x ->
   inc_pc vm;
   vm.stack.(vm.stack_ptr-1) <- vm.stack.(vm.stack_ptr-x)
 | LOADGLOBAL x ->
   inc_pc vm;
   vm.stack.(vm.stack_ptr) <- vm.globals.(x);
   vm.stack_ptr <- vm.stack_ptr + 1
 | STOREGLOBAL x ->
   inc_pc vm;
   vm.globals.(x) <- vm.stack.(vm.stack_ptr)
 | CURMEM ->
   inc_pc vm;
   vm.stack.(vm.stack_ptr) <- I32 (Int32.of_int vm.memsize);
   vm.stack_ptr <- vm.stack_ptr + 1
 | GROW ->
   inc_pc vm;
   vm.memsize <- vm.memsize + value_to_int vm.stack.(vm.stack_ptr-1);
   vm.stack_ptr <- vm.stack_ptr - 1
 | POPI x ->
   (* this might be a bit too complex instruction *)
   inc_pc vm;
   let idx = min x (value_to_int vm.stack.(vm.stack_ptr-1)) in
   vm.stack.(vm.stack_ptr-x-1) <- vm.stack.(vm.stack_ptr-idx-1);
   vm.stack_ptr <- vm.stack_ptr - x
 | BREAKTABLE ->
   let pos = value_to_int (vm.stack.(vm.stack_ptr-1)) in
   let loc, sptr = vm.break_stack.(vm.break_ptr-pos) in
   vm.break_ptr <- vm.break_ptr - 1;
   vm.stack_ptr <- sptr;
   vm.pc <- loc
 | PUSH lit ->
   inc_pc vm;
   vm.stack.(vm.stack_ptr) <- lit.it;
   vm.stack_ptr <- vm.stack_ptr + 1
 | CONV op ->
   inc_pc vm;
   vm.stack.(vm.stack_ptr-1) <- Eval_numeric.eval_cvtop op vm.stack.(vm.stack_ptr-1)
 | UNA op ->
   inc_pc vm;
   vm.stack.(vm.stack_ptr-1) <- Eval_numeric.eval_unop op vm.stack.(vm.stack_ptr-1)
 | TEST op ->
   inc_pc vm;
   vm.stack.(vm.stack_ptr-1) <- value_of_bool (Eval_numeric.eval_testop op vm.stack.(vm.stack_ptr-1))
 | BIN op ->
   inc_pc vm;
   vm.stack.(vm.stack_ptr-2) <- Eval_numeric.eval_binop op vm.stack.(vm.stack_ptr-2) vm.stack.(vm.stack_ptr-1)
 | CMP op ->
   inc_pc vm;
   vm.stack.(vm.stack_ptr-2) <- value_of_bool (Eval_numeric.eval_relop op vm.stack.(vm.stack_ptr-2) vm.stack.(vm.stack_ptr-1))
 | CALLI x ->
   let addr = value_to_int vm.stack.(vm.stack_ptr-1) in
   vm.stack_ptr <- vm.stack_ptr - 1;
   vm.pc <- vm.calltable.(addr+x)


