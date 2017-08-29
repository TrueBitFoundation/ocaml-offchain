
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

(* microcode *)

type in_code =
 | NoIn
 | Immed
 | GlobalIn
 | InReg1
 | StackIn0
 | StackIn1
 | StackInReg
 | StackInReg2
 | ReadPc
 | ReadStackPtr
 | BreakLocIn
 | BreakStackIn
 | BreakLocInReg
 | BreakStackInReg
 | CallIn
 | MemoryIn
 | MemsizeIn
 | TableIn

type reg =
 | Reg1
 | Reg2
 | Reg3

type out_code =
 | BreakLocOut
 | BreakStackOut
 | StackOutReg1
 | StackOut0
 | StackOut1
 | MemoryOut
 | CallOut
 | NoOut
 | GlobalOut

type alu_code =
 | Nop
 | UnaOp of Ast.unop
 | ConvOp of Ast.cvtop
 | BinOp of Ast.binop
 | RelOp of Ast.relop
 | TestOp of Ast.testop
 | Trap | Min
 | CheckJump

type stack_ch =
 | StackRegSub
 | StackReg
 | StackReg2
 | StackReg3
 | StackInc
 | StackDec
 | StackNop

type microp = {
  read_reg1 : in_code;
  read_reg2 : in_code;
  read_reg3 : in_code;
  write1 : reg * out_code;
  write2 : reg * out_code;
  alu_code : alu_code;
  call_ch : stack_ch;
  stack_ch : stack_ch;
  break_ch : stack_ch;
  pc_ch : stack_ch;
  mem_ch : bool;
  immed : value;
}

let noop = {
  read_reg1 = NoIn;
  read_reg2 = NoIn;
  read_reg3 = NoIn;
  write1 = (Reg1, NoOut);
  write2 = (Reg1, NoOut);
  alu_code = Nop;
  call_ch = StackNop;
  stack_ch = StackNop;
  break_ch = StackNop;
  pc_ch = StackInc;
  mem_ch = false;
  immed = I32 Int32.zero;
}

type registers = {
  mutable reg1: value;
  mutable reg2: value;
  mutable reg3: value;
  mutable ireg: value;
(*  mutable op: microp; *)
}

let i x = I32 (Int32.of_int x)

let micro_step vm =
  (* fetch code *)
  let op = get_code vm.code.(vm.pc) in
  (* init registers *)
  let regs = {reg1=i 0; reg2=i 0; reg3=i 0; ireg=op.immed} in
  (* read registers *)
  reg1 <- read_register vm regs op.read_reg1;
  reg2 <- read_register vm regs op.read_reg2;
  reg3 <- read_register vm regs op.read_reg3;
  (* ALU *)
  reg1 <- handle_alu regs op.alu_code;
  write_register vm (get_register (fst op.write1) regs) (snd op.write1);
  write_register vm (get_register (fst op.write2) regs) (snd op.write2);

let get_code = function
 | NOP -> noop
 | UNREACHABLE -> {noop with alu_code=Trap}
 | JUMP x -> {noop with immed=i x; read_reg1 = Immed; pc_ch=StackReg}
 | JUMPI x -> {noop with immed=i x; read_reg1 = Immed; read_reg2 = StackIn0; alu_code = CheckJump; pc_ch=StackReg; stack_ch=StackDec}
 | CALL x -> {noop with immed=i x; read_reg1 = Immed; read_reg2 = ReadPc; write1 = (Reg2, CallOut); call_ch = StackInc; pc_ch=StackReg}
 | LABEL _ -> raise VmError (* these should have been processed away *)
 | PUSHBRK x -> {noop with immed=i x; read_reg1 = Immed; read_reg2 = ReadStackPtr; write1 = (Reg1, BreakLocOut); write2 = (Reg2, BreakStackOut); break_ch=StackInc}
 | POPBRK -> {noop with break_ch=StackDec}
 | BREAK -> {noop with read_reg1 = BreakLocIn; read_reg2 = BreakStackIn; break_ch=StackDec; stack_ch=StackReg2; pc_ch=StackReg}
 | RETURN -> {noop with read_reg1=CallIn; call_ch=StackDec; pc_ch=StackReg}
 (* Reg12: memory address *)
 | LOAD x -> {noop with immed=i x; read_reg1=Immed; read_reg2=StackIn0; read_reg3=MemoryIn; write1=(Reg3, StackOut1)}
 | STORE x -> {noop with immed=i x; read_reg1=Immed; read_reg2=StackIn0; read_reg3=StackIn1; write1=(Reg3, MemoryOut); stack_ch=StackDec}
 | DROP -> {noop with stack_ch=StackDec}
 | DUP x -> {noop with immed=i x; read_reg1=Immed; read_reg2=StackInReg; write1=(Reg2, StackOut1); stack_ch=StackInc}
 | SWAP x -> {noop with immed=i x; read_reg1=Immed; read_reg2=StackIn0; write1=(Reg2, StackOutReg1)}
 | LOADGLOBAL x -> {noop with immed=i x; read_reg1=Immed; read_reg2=GlobalIn; write1=(Reg2, StackOut0); stack_ch=StackInc}
 | STOREGLOBAL x -> {noop with immed=i x; read_reg1=Immed; read_reg2=StackIn0; write1=(Reg2, GlobalOut)}
 | CURMEM -> {noop with stack_ch=StackInc; read_reg2 = MemsizeIn; write1=(Reg2, StackOut0)}
 | GROW -> {noop with read_reg2=MemsizeIn; read_reg3 = StackIn0; mem_ch=true; stack_ch=StackDec}
 | PUSH lit -> {noop with immed=lit.it; read_reg1=Immed; stack_ch=StackInc; write1=(Reg1, StackOut0)}
 | CONV op -> {noop with read_reg1=StackIn0; write1=(Reg1, StackOut1); alu_code=ConvOp op}
 | UNA op -> {noop with read_reg1=StackIn0; write1=(Reg1, StackOut1); alu_code=UnaOp op}
 | TEST op -> {noop with read_reg1=StackIn0; write1=(Reg1, StackOut1); alu_code=TestOp op}
 | BIN op -> {noop with read_reg1=StackIn1; read_reg2=StackIn0; write1=(Reg1, StackOut1); alu_code=BinOp op; stack_ch=StackDec}
 | CMP op -> {noop with read_reg1=StackIn1; read_reg2=StackIn0; write1=(Reg1, StackOut1); alu_code=RelOp op; stack_ch=StackDec}
 | CALLI x -> {noop with immed=i x; read_reg1=Immed; read_reg2=StackIn0; read_reg3=TableIn; pc_ch=StackReg3}
 | POPI1 x -> {noop with immed=i x; read_reg1=Immed; read_reg2=StackIn0; alu_code=Min; write1=(Reg1, StackOut1)}
 | POPI2 x -> {noop with immed=i x; read_reg1=Immed; read_reg2=StackIn0; read_reg3=StackInReg2; write1=(Reg3, StackOutReg1); stack_ch=StackRegSub}
 | BREAKTABLE -> {noop with read_reg1=StackIn0; read_reg2=BreakLocInReg; read_reg3=BreakStackInReg; break_ch=StackDec; stack_ch=StackReg3; pc_ch=StackReg2}

let vm_step vm = match vm.code.(vm.pc) with
 | NOP -> inc_pc vm
 | UNREACHABLE -> raise VmError
 | JUMP x -> vm.pc <- x
 | JUMPI x ->
   vm.pc <- (if value_bool (vm.stack.(vm.stack_ptr-1)) then x else vm.pc + 1);
   vm.stack_ptr <- vm.stack_ptr - 1
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
 | POPI1 x ->
   inc_pc vm;
   vm.stack.(vm.stack_ptr-1) <- i (min x (value_to_int vm.stack.(vm.stack_ptr-1)))
 | POPI2 x ->
   (* this might be a bit too complex instruction *)
   inc_pc vm;
   let idx = value_to_int vm.stack.(vm.stack_ptr-1) in
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
   vm.stack.(vm.stack_ptr-2) <- Eval_numeric.eval_binop op vm.stack.(vm.stack_ptr-2) vm.stack.(vm.stack_ptr-1);
   vm.stack_ptr <- vm.stack_ptr - 1
 | CMP op ->
   inc_pc vm;
   vm.stack.(vm.stack_ptr-2) <- value_of_bool (Eval_numeric.eval_relop op vm.stack.(vm.stack_ptr-2) vm.stack.(vm.stack_ptr-1));
   vm.stack_ptr <- vm.stack_ptr - 1
 | CALLI x ->
   let addr = value_to_int vm.stack.(vm.stack_ptr-1) in
   vm.stack_ptr <- vm.stack_ptr - 1;
   vm.pc <- vm.calltable.(addr+x)


