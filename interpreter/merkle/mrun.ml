
open Merkle
open Values

type vm = {
  code : inst array;
  stack : value array;
  memory : value array;
  break_stack : (int*int) array;
(*  call_stack : (int*int*int) array; *)
  call_stack : int array;
  globals : value array;
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
 | JUMPI x -> vm.pc <- (if value_bool (vm.stack.(vm.stack_ptr)) then x else vm.pc + 1)
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
   let loc, sptr = vm.break_stack.(vm.break_ptr) in
   vm.break_ptr <- vm.break_ptr - 1;
   vm.stack_ptr <- sptr;
   vm.pc <- loc
 | RETURN ->
   vm.pc <- vm.call_stack.(vm.call_ptr);
   vm.call_ptr <- vm.call_ptr - 1
 | LOAD x ->
   inc_pc vm;
   vm.stack.(vm.stack_ptr) <- vm.memory.(value_to_int vm.stack.(vm.stack_ptr) + x)
 | STORE x ->
   inc_pc vm;
   vm.memory.(value_to_int vm.stack.(vm.stack_ptr) + x) <- vm.stack.(vm.stack_ptr-1);
   vm.stack_ptr <- vm.stack_ptr - 1
 | DROP ->
   inc_pc vm;
   vm.stack_ptr <- vm.stack_ptr - 1

