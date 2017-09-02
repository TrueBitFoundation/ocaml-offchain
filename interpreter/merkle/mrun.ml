
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
  calltable : int array;
  mutable pc : int;
  mutable stack_ptr : int;
  mutable break_ptr : int;
  mutable call_ptr : int;
  mutable memsize : int;
}

exception VmError

let value_bool v = not (v = I32 0l)

let value_to_int = function
 | I32 i -> Int32.to_int i
 | I64 i -> Int64.to_int i
 | _ -> 0

let i x = I32 (Int32.of_int x)

let inc_pc vm = vm.pc <- vm.pc+1

let create_vm code =
  { code = Array.of_list code;
    stack = Array.make 10000 (i 0);
    memory = Array.make 10000 (i 0);
    call_stack = Array.make 10000 0;
    break_stack = Array.make 10000 (0,0);
    globals = Array.make 1000 (i 0);
    calltable = Array.make 10000 0;
    pc = 0;
    stack_ptr = 0;
    memsize = 10000;
    break_ptr = 0;
    call_ptr = 0; }

(* microcode *)

type in_code =
 | NoIn
 | Immed
 | GlobalIn
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
 | Unary of Ast.unop
 | Convert of Ast.cvtop
 | Binary of Ast.binop
 | Compare of Ast.relop
 | Test of Ast.testop
 | Trap
 | Min
 | CheckJump
 | Nop

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

let read_register vm reg = function
 | NoIn -> i 0
 | Immed -> reg.ireg
 | GlobalIn -> vm.globals.(value_to_int reg.reg1)
 | StackIn0 -> vm.stack.(vm.stack_ptr-1)
 | StackIn1 -> vm.stack.(vm.stack_ptr-2)
 | StackInReg -> vm.stack.(vm.stack_ptr-1-value_to_int reg.reg1)
 | StackInReg2 -> vm.stack.(vm.stack_ptr-1-value_to_int reg.reg2)
 | ReadPc -> i (vm.pc+1)
 | ReadStackPtr -> i vm.stack_ptr
 | BreakLocIn -> i (fst (vm.break_stack.(vm.break_ptr-1)))
 | BreakStackIn -> i (snd (vm.break_stack.(vm.break_ptr-1)))
 | BreakLocInReg -> i (fst (vm.break_stack.(vm.break_ptr-1-value_to_int reg.reg1)))
 | BreakStackInReg -> i (snd (vm.break_stack.(vm.break_ptr-1-value_to_int reg.reg1)))
 | CallIn -> i vm.call_stack.(vm.call_ptr-1)
 | MemoryIn -> vm.memory.(value_to_int reg.reg1+value_to_int reg.reg2)
 | MemsizeIn -> i vm.memsize
 | TableIn -> i vm.calltable.(value_to_int reg.reg1)

let get_register regs = function
 | Reg1 -> regs.reg1
 | Reg2 -> regs.reg2
 | Reg3 -> regs.reg3

let write_register vm regs v = function
 | NoOut -> ()
 | GlobalOut -> vm.globals.(value_to_int regs.reg1) <- v
 | CallOut -> vm.call_stack.(vm.call_ptr) <- value_to_int v
 | MemoryOut -> vm.memory.(value_to_int regs.reg1+value_to_int regs.reg2) <- v
 | StackOut0 -> vm.stack.(vm.stack_ptr) <- v
 | StackOut1 -> vm.stack.(vm.stack_ptr-1) <- v
 | StackOutReg1 -> vm.stack.(vm.stack_ptr-value_to_int regs.reg1) <- v
 | BreakLocOut ->
   let (a,b) = vm.break_stack.(vm.break_ptr) in
   vm.break_stack.(vm.break_ptr) <- (value_to_int v, b)
 | BreakStackOut ->
   let (a,b) = vm.break_stack.(vm.break_ptr) in
   vm.break_stack.(vm.break_ptr) <- (a, value_to_int v)

let handle_ptr regs ptr = function
 | StackRegSub -> ptr - value_to_int regs.reg1
 | StackReg -> value_to_int regs.reg1
 | StackReg2 -> value_to_int regs.reg2
 | StackReg3 -> value_to_int regs.reg3
 | StackInc -> ptr + 1
 | StackDec -> ptr - 1
 | StackNop -> ptr

let handle_alu r1 r2 r3 = function
 | Min -> i (min (value_to_int r1) (value_to_int r2))
 | Convert op -> Eval_numeric.eval_cvtop op r1
 | Unary op -> Eval_numeric.eval_unop op r1
 | Test op -> value_of_bool (Eval_numeric.eval_testop op r1)
 | Binary op -> Eval_numeric.eval_binop op r1 r2
 | Compare op -> value_of_bool (Eval_numeric.eval_relop op r1 r2)
 | Trap -> raise VmError
 | Nop -> r1
 | CheckJump -> if value_bool r2 then r1 else i (value_to_int r3 + 1)

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
 | PUSH lit -> {noop with immed=lit; read_reg1=Immed; stack_ch=StackInc; write1=(Reg1, StackOut0)}
 | CONV op -> {noop with read_reg1=StackIn0; write1=(Reg1, StackOut1); alu_code=Convert op}
 | UNA op -> {noop with read_reg1=StackIn0; write1=(Reg1, StackOut1); alu_code=Unary op}
 | TEST op -> {noop with read_reg1=StackIn0; write1=(Reg1, StackOut1); alu_code=Test op}
 | BIN op -> {noop with read_reg1=StackIn1; read_reg2=StackIn0; write1=(Reg1, StackOut1); alu_code=Binary op; stack_ch=StackDec}
 | CMP op -> {noop with read_reg1=StackIn1; read_reg2=StackIn0; write1=(Reg1, StackOut1); alu_code=Compare op; stack_ch=StackDec}
 | CALLI x -> {noop with immed=i x; read_reg1=Immed; read_reg2=StackIn0; read_reg3=TableIn; pc_ch=StackReg3}
 | POPI1 x -> {noop with immed=i x; read_reg1=Immed; read_reg2=StackIn0; alu_code=Min; write1=(Reg1, StackOut1)}
 | POPI2 x -> {noop with immed=i x; read_reg1=Immed; read_reg2=StackIn0; read_reg3=StackInReg2; write1=(Reg3, StackOutReg1); stack_ch=StackRegSub}
 | BREAKTABLE -> {noop with read_reg1=StackIn0; read_reg2=BreakLocInReg; read_reg3=BreakStackInReg; break_ch=StackDec; stack_ch=StackReg3; pc_ch=StackReg2}

let micro_step vm =
  (* fetch code *)
  let op = get_code vm.code.(vm.pc) in
  (* init registers *)
  let regs = {reg1=i 0; reg2=i 0; reg3=i 0; ireg=op.immed} in
  (* read registers *)
  regs.reg1 <- read_register vm regs op.read_reg1;
  regs.reg2 <- read_register vm regs op.read_reg2;
  regs.reg3 <- read_register vm regs op.read_reg3;
  (* ALU *)
  regs.reg1 <- handle_alu regs.reg1 regs.reg2 regs.reg3 op.alu_code;
  (* Write registers *)
  write_register vm regs (get_register regs (fst op.write1)) (snd op.write1);
  write_register vm regs (get_register regs (fst op.write2)) (snd op.write2);
  (* update pointers *)
  vm.pc <- handle_ptr regs vm.pc op.pc_ch;
  vm.break_ptr <- handle_ptr regs vm.break_ptr op.break_ch;
  vm.stack_ptr <- handle_ptr regs vm.stack_ptr op.stack_ch;
  vm.call_ptr <- handle_ptr regs vm.call_ptr op.call_ch;
  if op.mem_ch then vm.memsize <- vm.memsize + value_to_int regs.reg1

let vm_step vm = match vm.code.(vm.pc) with
 | NOP -> inc_pc vm
 | UNREACHABLE -> raise VmError
 | JUMP x ->
   vm.pc <- x
 | JUMPI x ->
   vm.pc <- (if value_bool (vm.stack.(vm.stack_ptr-1)) then x else vm.pc + 1);
   vm.stack_ptr <- vm.stack_ptr - 1
 | CALL x ->
   (* vm.call_stack.(vm.call_ptr) <- (vm.pc, vm.stack_ptr, vm.break_ptr);  I now guess that it won't need these *)
   vm.call_stack.(vm.call_ptr) <- vm.pc+1;
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
   vm.stack.(vm.stack_ptr-x) <- vm.stack.(vm.stack_ptr-1)
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
   vm.stack.(vm.stack_ptr) <- lit;
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

let trace_step vm = match vm.code.(vm.pc) with
 | NOP -> "NOP"
 | UNREACHABLE -> "UNREACHABLE"
 | JUMP x -> "JUMP"
 | JUMPI x -> "JUMPI " ^ if value_bool (vm.stack.(vm.stack_ptr-1)) then " jump" else " no jump"
 | CALL x -> "CALL " ^ string_of_int x
 | LABEL _ -> "LABEL ???"
 | PUSHBRK x -> "PUSHBRK"
 | POPBRK -> "POPBRK"
 | BREAK -> "BREAK"
 | RETURN -> "RETURN"
 | LOAD x -> "LOAD (broken)"
 | STORE x -> "STORE (broken)"
 | DROP -> "DROP"
 | DUP x -> "DUP" ^ string_of_int x ^ ": " ^ string_of_value vm.stack.(vm.stack_ptr-x)
 | SWAP x -> "SWAP " ^ string_of_int x
 | LOADGLOBAL x -> "LOADGLOBAL"
 | STOREGLOBAL x -> "STOREGLOBAL"
 | CURMEM -> "CURMEM"
 | GROW -> "GROW"
 | POPI1 x -> "POPI1"
 | POPI2 x -> "POPI2"
 | BREAKTABLE -> "BREAKTABLE"
 | PUSH lit -> "PUSH " ^ string_of_value lit
 | CONV op -> "CONV"
 | UNA op -> "UNA"
 | TEST op -> "TEST"
 | BIN op -> "BIN " ^ string_of_value vm.stack.(vm.stack_ptr-2) ^ " " ^ string_of_value vm.stack.(vm.stack_ptr-1)
 | CMP op -> "CMP " ^ string_of_value vm.stack.(vm.stack_ptr-2) ^ " " ^ string_of_value vm.stack.(vm.stack_ptr-1)
 | CALLI x -> "CALLI"


