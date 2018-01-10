
open Merkle
open Values

exception VmTrap
exception VmError

type input = {
  file_name : string array;
  file_data : string array;
  file_size : int array;
}

type vm = {
  code : inst array;
  input : input;

  mutable stack : value array;
  mutable memory : Int64.t array;
  mutable call_stack : int array;
  mutable globals : value array;
  mutable calltable : int array;
  mutable calltable_types : Int64.t array;

  mutable pc : int;
  mutable stack_ptr : int;
  mutable call_ptr : int;
  mutable memsize : int;
}

let inc_pc vm = vm.pc <- vm.pc+1

let empty_input sz = {
  file_name = Array.make sz "";
  file_data = Array.make sz "";
  file_size = Array.make sz 0;
}

let create_vm code = {
  code = Array.of_list code;
(*  stack = Array.make 1024 (i 0); memory = Array.make 1024 0L; *)
  stack = Array.make !Flags.stack_size (i 0);
(*  memory = Array.make (1024*64) 0L; *)
  memory = Array.make (!Flags.memory_size*1024*8) 0L;
  input = empty_input 16;
  call_stack = Array.make (!Flags.call_size) 0;
  globals = Array.make (!Flags.globals_size) (i 0);
  calltable = Array.make (!Flags.table_size) (-1);
  calltable_types = Array.make (!Flags.table_size) 0L;
  pc = 0;
  stack_ptr = 0;
  memsize = 0;
  call_ptr = 0;
}

let rec pow2 n = if n = 0 then 1 else 2 * pow2 (n-1)

(* microcode *)

type in_code =
 | NoIn
 | Immed
 | GlobalIn
 | StackIn0
 | StackIn1
 | StackIn2
 | StackInReg
 | StackInReg2
 | ReadPc
 | ReadStackPtr
 | CallIn
 | MemoryIn1
 | MemsizeIn
 | TableIn
 | MemoryIn2
 | TableTypeIn
 | InputSizeIn
 | InputNameIn
 | InputDataIn

type alu_code =
 | Unary of Ast.unop
 | Convert of Ast.cvtop
 | Binary of Ast.binop
 | Compare of Ast.relop
 | Test of Ast.testop
 | Trap
 | Exit
 | Min
 | CheckJump
 | Nop
 | FixMemory of Types.value_type * (Memory.mem_size * Memory.extension) option
 | CheckJumpForward
 | CheckDynamicCall

type reg =
 | Reg1
 | Reg2
 | Reg3

type out_code =
 | StackOutReg1
 | StackOut0
 | StackOut1
 | StackOut2
 | CallOut
 | NoOut
 | GlobalOut
 | MemoryOut1 of Types.value_type * Memory.mem_size option
 | MemoryOut2 of Types.value_type * Memory.mem_size option
 | InputSizeOut
 | InputNameOut
 | InputCreateOut
 | InputDataOut
 | CallTableOut
 | CallTypeOut
 | SetStack
 | SetCallStack
 | SetTable
 | SetTableTypes
 | SetMemory
 | SetGlobals

type stack_ch =
 | StackRegSub
 | StackReg
 | StackReg2
 | StackReg3
 | StackInc
 | StackDec
 | StackNop
 | StackDec2
 | StackDecImmed

type microp = {
  read_reg1 : in_code;
  read_reg2 : in_code;
  read_reg3 : in_code;
  write1 : reg * out_code;
  write2 : reg * out_code;
  alu_code : alu_code;
  call_ch : stack_ch;
  stack_ch : stack_ch;
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
 | StackIn2 -> vm.stack.(vm.stack_ptr-3)
 | StackInReg -> vm.stack.(vm.stack_ptr-value_to_int reg.reg1)
 | StackInReg2 -> vm.stack.(vm.stack_ptr-value_to_int reg.reg2)
 | ReadPc -> i (vm.pc+1)
 | ReadStackPtr -> i vm.stack_ptr
 | CallIn -> i vm.call_stack.(vm.call_ptr-1)
 | MemoryIn1 -> I64 vm.memory.((value_to_int reg.reg1+value_to_int reg.ireg) / 8)
 | MemoryIn2 -> I64 vm.memory.((value_to_int reg.reg1+value_to_int reg.ireg) / 8 + 1)
 | MemsizeIn -> i vm.memsize
 | TableIn -> i vm.calltable.(value_to_int reg.reg1)
 | TableTypeIn -> I64 vm.calltable_types.(value_to_int reg.reg1)
 | InputSizeIn -> i vm.input.file_size.(value_to_int reg.reg1)
 | InputNameIn ->
   let str = vm.input.file_name.(value_to_int reg.reg2) in
   let s1 = value_to_int reg.reg1 in
   let chr = if s1 < String.length str then Char.code str.[s1] else 0 in
   i chr
 | InputDataIn -> i (Char.code vm.input.file_data.(value_to_int reg.reg2).[value_to_int reg.reg1])

let get_register regs = function
 | Reg1 -> regs.reg1
 | Reg2 -> regs.reg2
 | Reg3 -> regs.reg3

let get_memory mem loc = Byteutil.mini_memory mem.(loc/8) mem.(loc/8+1)

let memop mem v addr = function
 | None -> Memory.store mem addr 0l v
 | Some sz -> Memory.store_packed sz mem addr 0l v

let set_input_name vm s2 s1 v =
   let str = vm.input.file_name.(s2) in
   let str = if String.length str < 256 then String.make 256 (Char.chr 0) else str in
   Bytes.set str s1 (Char.chr (value_to_int v));
   vm.input.file_name.(s2) <- str

let write_register vm regs v = function
 | NoOut -> ()
 | GlobalOut -> vm.globals.(value_to_int regs.reg1) <- v
 | CallOut -> vm.call_stack.(vm.call_ptr) <- value_to_int v
 | CallTableOut -> vm.calltable.(value_to_int regs.ireg) <- value_to_int v
 | CallTypeOut -> vm.calltable_types.(value_to_int regs.ireg) <- value_to_int64 v
 | MemoryOut1 (_,sz) ->
    let loc = value_to_int regs.reg1+value_to_int regs.ireg in
    let mem = get_memory vm.memory loc in
    memop mem v (Int64.of_int (loc-(loc/8)*8)) sz;
    vm.memory.(loc/8) <- fst (Byteutil.Decode.mini_memory mem);
    trace ("Stored " ^ Int64.to_string vm.memory.(loc/8))
 | MemoryOut2 (_,sz) ->
    let loc = value_to_int regs.reg1+value_to_int regs.ireg in
    let mem = get_memory vm.memory loc in
    memop mem v (Int64.of_int (loc-(loc/8)*8)) sz;
    vm.memory.(loc/8+1) <- snd (Byteutil.Decode.mini_memory mem)
 | StackOut0 ->
    trace ("push to stack: " ^ string_of_value v);
    vm.stack.(vm.stack_ptr) <- v
 | StackOut1 ->
    trace ("replace top of stack: " ^ string_of_value v);
    vm.stack.(vm.stack_ptr-1) <- v
 | StackOut2 ->
    trace ("pop to stack: " ^ string_of_value v);
    vm.stack.(vm.stack_ptr-2) <- v
 | StackOutReg1 -> vm.stack.(vm.stack_ptr-value_to_int regs.reg1) <- v
 | InputSizeOut ->
   let s1 = value_to_int regs.reg1 in
   vm.input.file_size.(s1) <- value_to_int v
 | InputCreateOut ->
   let s1 = value_to_int regs.reg1 in
   vm.input.file_data.(s1) <- Bytes.create (value_to_int v)
 | InputNameOut ->
   let s2 = value_to_int regs.reg1 in
   let s1 = value_to_int regs.reg2 in
   set_input_name vm s2 s1 v
 | InputDataOut ->
   let s2 = value_to_int regs.reg1 in
   let s1 = value_to_int regs.reg2 in
   Bytes.set vm.input.file_data.(s2) s1 (Char.chr (value_to_int v))
 | SetStack ->
   let sz = pow2 (value_to_int v) in
   vm.stack <- Array.make sz (i 0)
 | SetCallStack ->
   let sz = pow2 (value_to_int v) in
   vm.call_stack <- Array.make sz 0
 | SetTable ->
   let sz = pow2 (value_to_int v) in
   vm.calltable <- Array.make sz (-1)
 | SetTableTypes ->
   let sz = pow2 (value_to_int v) in
   vm.calltable_types <- Array.make sz 0L
 | SetMemory ->
   let sz = pow2 (value_to_int v) in
   vm.memory <- Array.make sz 0L
 | SetGlobals ->
   let sz = pow2 (value_to_int v) in
   vm.globals <- Array.make sz (i 0)

let setup_memory vm m instance =
  let open Ast in
  let open Types in
  let open Source in
  List.iter (function MemoryType {min; _} ->
    trace ("Memory size " ^ Int32.to_string min);
    vm.memsize <- Int32.to_int min) (List.map (fun a -> a.it.mtype) m.memories);
  if !Flags.run_wasm then vm.memsize <- 1000000

let init_memory m instance =
  let open Ast in
  let open Types in
  let open Source in
  trace ("Segments: " ^ string_of_int (List.length m.data));
  let res = ref [] in
  let init (dta:bytes Ast.segment) =
    let offset = value_to_int (Eval.eval_const instance dta.it.offset) in
    let sz = Bytes.length dta.it.init in
    for i = 0 to sz-1 do
      let v = I32 (Int32.of_int (Char.code (Bytes.get dta.it.init i))) in
      res :=
        [STORE {ty=I32Type; align=0; offset=0l; sz=Some Memory.Mem8};
          PUSH v; PUSH (I32 (Int32.of_int (offset+i)))] @ !res
    done in
  List.iter init m.data;
  List.rev !res

let init_calltable m instance =
  let open Ast in
  let open Source in
  let init (dta:var list Ast.segment) = List.flatten (List.map (fun _ -> [NOP;NOP;NOP;NOP]) dta.it.init) in
  List.flatten (List.map init m.elems)

(* cannot compile function before the size of this segment is known *)
let setup_calltable vm m instance f_resolve code_offset =
  let open Ast in
  let open Source in
  let ftab, ttab = make_tables m in
  let pos = ref code_offset in
  let init (dta:var list Ast.segment) =
    let offset = value_to_int (Eval.eval_const instance dta.it.offset) in
    List.iteri (fun idx el ->
      let f_num = Int32.to_int el.it in
      vm.code.(!pos) <- PUSH (i (Hashtbl.find f_resolve f_num)); incr pos;
      vm.code.(!pos) <- INITCALLTABLE (offset+idx); incr pos;
      trace ("Table element " ^ Int32.to_string el.it);
      let func = Byteutil.ftype_hash (Hashtbl.find ftab el.it) in
      trace ("Call table at " ^ string_of_int (offset+idx) ^ ": function " ^ string_of_int f_num ^ " type " ^ Int64.to_string func);
      vm.code.(!pos) <- PUSH (I64 func); incr pos;
      vm.code.(!pos) <- INITCALLTYPE (offset+idx); incr pos;
      ()) dta.it.init in
  List.iter init m.elems

let find_global a b t =
 let open Types in
 match Utf8.encode a with
 | "env" -> Env.lookup b (ExternalGlobalType t)
 | "global" -> Global.lookup b (ExternalGlobalType t)
 | _ -> assert false

let setup_globals (m:Ast.module_') instance =
  trace "Initializing globals";
  let open Source in
  let open Ast in
  let res = ref [] in
  let rec get_imports i = function
   | [] -> []
   | {it=im; _} :: tl ->
     match im.idesc.it with
     | GlobalImport t ->
       ( match find_global im.module_name im.item_name t with
       | Instance.ExternalGlobal x -> res := [STOREGLOBAL i; PUSH x] @ !res
       | _ -> () );
       im :: get_imports (i+1) tl
     | _ -> get_imports i tl in
  let num_imports = List.length (get_imports 0 m.imports) in
  let init i (dta:Ast.global) =
    let v = Eval.eval_const instance dta.it.value in
    res := [STOREGLOBAL (i+num_imports); PUSH v] @ !res in
  List.iteri init m.globals;
  List.rev !res

let handle_ptr regs ptr = function
 | StackRegSub -> ptr - value_to_int regs.reg1
 | StackReg -> value_to_int regs.reg1
 | StackReg2 -> value_to_int regs.reg2
 | StackReg3 -> value_to_int regs.reg3
 | StackInc -> ptr + 1
 | StackDec2 -> ptr - 2
 | StackDec -> ptr - 1
 | StackNop -> ptr
 | StackDecImmed -> ptr - 1 - value_to_int regs.ireg

let mem_load r2 r3 ty sz loc =
  let open Byteutil in
  let mem = mini_memory_v r2 r3 in
(*  trace ("LOADING " ^ w256_to_string (get_value r2) ^ " & " ^ Byteutil.w256_to_string (get_value r3));
  trace ("Get memory: " ^ w256_to_string (Memory.to_bytes mem)); *)
  let addr = Int64.of_int (loc-(loc/8)*8) in
  ( match sz with
  | None -> Memory.load mem addr 0l ty
  | Some (sz, ext) -> Memory.load_packed sz ext mem addr 0l ty )

let handle_alu r1 r2 r3 ireg = function
 | FixMemory (ty, sz) -> mem_load r2 r3 ty sz (value_to_int r1+value_to_int ireg)
 | Min ->
   let v = min (value_to_int r1) (value_to_int r2) in
   trace ("min " ^ string_of_int v);
   i v
 | Convert op -> Eval_numeric.eval_cvtop op r1
 | Unary op -> Eval_numeric.eval_unop op r1
 | Test op -> value_of_bool (Eval_numeric.eval_testop op r1)
 | Binary op -> Eval_numeric.eval_binop op r1 r2
 | Compare op -> value_of_bool (Eval_numeric.eval_relop op r1 r2)
 | Trap -> raise (Eval.Trap (Source.no_region, "unreachable executed"))
 | Exit -> raise VmTrap
 | Nop -> r1
 | CheckJump ->
   trace ("check jump " ^ string_of_value r2 ^ " jump to " ^ string_of_value r1 ^ " or " ^ string_of_value r3);
   if value_bool r2 then r1 else i (value_to_int r3)
 | CheckJumpForward ->
   let idx = value_to_int r1 in
   let x = value_to_int ireg in
   let idx = if idx < 0 || idx >= x then x else idx in
   i (value_to_int r2 + idx)
 | CheckDynamicCall -> (* expected type is in the immediate, reg2 has the type of called function *)
   if r2 <> ireg then raise (Eval.Trap (Source.no_region, "indirect call signature mismatch"))
   else i 0

open Ast

let get_code = function
 | NOP -> noop
 | STUB _ -> noop
 | UNREACHABLE -> {noop with alu_code=Trap}
 | EXIT -> {noop with alu_code=Exit}
 | JUMP x -> {noop with immed=i x; read_reg1 = Immed; pc_ch=StackReg}
 | JUMPI x -> {noop with immed=i x; read_reg1 = Immed; read_reg2 = StackIn0; read_reg3 = ReadPc; alu_code = CheckJump; pc_ch=StackReg; stack_ch=StackDec}
 | JUMPFORWARD x -> {noop with immed=i x; read_reg1 = StackIn0; read_reg2 = ReadPc; alu_code = CheckJumpForward; pc_ch=StackReg; stack_ch=StackDec}
 | CALL x -> {noop with immed=i x; read_reg1=Immed; read_reg2 = ReadPc; write1 = (Reg2, CallOut); call_ch = StackInc; pc_ch=StackReg}
 | CHECKCALLI x -> {noop with immed=I64 x; read_reg1=StackIn0; read_reg2=TableTypeIn; alu_code=CheckDynamicCall; pc_ch=StackInc}
 | CALLI -> {noop with read_reg2=ReadPc; read_reg1=StackIn0; read_reg3=TableIn; pc_ch=StackReg3; write1 = (Reg2, CallOut); call_ch = StackInc; stack_ch=StackDec}
 | INPUTSIZE -> {noop with read_reg1=StackIn0; read_reg2=InputSizeIn; write1 = (Reg2, StackOut1)}
 | INPUTNAME -> {noop with read_reg1=StackIn0; read_reg2=StackIn1; read_reg3=InputNameIn; write1 = (Reg3, StackOut2); stack_ch=StackDec}
 | INPUTDATA -> {noop with read_reg1=StackIn0; read_reg2=StackIn1; read_reg3=InputDataIn; write1 = (Reg3, StackOut2); stack_ch=StackDec}
 | OUTPUTSIZE -> {noop with read_reg1=StackIn0; read_reg2=StackIn1; write1 = (Reg2, InputSizeOut); write2 = (Reg2, InputCreateOut); stack_ch=StackDec2}
 | OUTPUTNAME -> {noop with immed=i 2; read_reg1=StackIn2; read_reg2=StackIn1; read_reg3=StackIn0; write1 = (Reg3, InputNameOut); stack_ch=StackDecImmed}
 | OUTPUTDATA -> {noop with immed=i 2; read_reg1=StackIn2; read_reg2=StackIn1; read_reg3=StackIn0; write1 = (Reg3, InputDataOut); stack_ch=StackDecImmed}
 | LABEL _ -> raise VmError (* these should have been processed away *)
 | RETURN -> {noop with read_reg1=CallIn; call_ch=StackDec; pc_ch=StackReg}
 (* IReg + Reg1: memory address *)
 | LOAD x -> {noop with immed=I32 x.offset; read_reg1=StackIn0; read_reg2=MemoryIn1; read_reg3=MemoryIn2; alu_code=FixMemory (x.ty, x.sz); write1=(Reg1, StackOut1)}
 | STORE x -> {noop with immed=I32 x.offset; read_reg1=StackIn1; read_reg2=StackIn0; write1=(Reg2, MemoryOut1 (x.ty,x.sz)); write2=(Reg2, MemoryOut2 (x.ty,x.sz)); stack_ch=StackDec2}
 | DROP x -> {noop with immed=i x; read_reg1 = Immed; stack_ch=StackRegSub}
 | DROP_N -> {noop with read_reg1 = StackIn0; stack_ch=StackRegSub}
 | DUP x -> {noop with immed=i x; read_reg1=Immed; read_reg2=StackInReg; write1=(Reg2, StackOut0); stack_ch=StackInc}
 | SWAP x -> {noop with immed=i x; read_reg1=Immed; read_reg2=StackIn0; write1=(Reg2, StackOutReg1)}
 | LOADGLOBAL x -> {noop with immed=i x; read_reg1=Immed; read_reg2=GlobalIn; write1=(Reg2, StackOut0); stack_ch=StackInc}
 | STOREGLOBAL x -> {noop with immed=i x; read_reg1=Immed; read_reg2=StackIn0; write1=(Reg2, GlobalOut); stack_ch=StackDec}
 | INITCALLTABLE x -> {noop with immed=i x; read_reg2=StackIn0; write1=(Reg2, CallTableOut); stack_ch=StackDec}
 | INITCALLTYPE x -> {noop with immed=i x; read_reg2=StackIn0; write1=(Reg2, CallTypeOut); stack_ch=StackDec}
 | CURMEM -> {noop with stack_ch=StackInc; read_reg2 = MemsizeIn; write1=(Reg2, StackOut0)}
 | GROW -> {noop with read_reg2=MemsizeIn; read_reg3 = StackIn0; mem_ch=true; stack_ch=StackDec}
 | PUSH lit -> {noop with immed=lit; read_reg1=Immed; stack_ch=StackInc; write1=(Reg1, StackOut0)}
 | CONV op -> {noop with read_reg1=StackIn0; write1=(Reg1, StackOut1); alu_code=Convert op}
 | UNA op -> {noop with read_reg1=StackIn0; write1=(Reg1, StackOut1); alu_code=Unary op}
 | TEST op -> {noop with read_reg1=StackIn0; write1=(Reg1, StackOut1); alu_code=Test op}
 | BIN op -> {noop with read_reg1=StackIn1; read_reg2=StackIn0; write1=(Reg1, StackOut2); alu_code=Binary op; stack_ch=StackDec}
 | CMP op -> {noop with read_reg1=StackIn1; read_reg2=StackIn0; write1=(Reg1, StackOut2); alu_code=Compare op; stack_ch=StackDec}
 | SETSTACK x -> {noop with immed=i x; read_reg1=Immed; write1=(Reg1,SetStack)}
 | SETCALLSTACK x -> {noop with immed=i x; read_reg1=Immed; write1=(Reg1,SetCallStack)}
 | SETGLOBALS x -> {noop with immed=i x; read_reg1=Immed; write1=(Reg1,SetGlobals)}
 | SETMEMORY x -> {noop with immed=i x; read_reg1=Immed; write1=(Reg1,SetMemory)}
 | SETTABLE x -> {noop with immed=i x; read_reg1=Immed; write1=(Reg1,SetTableTypes); write2=(Reg1,SetTable)}

let micro_step vm =
  let open Values in
  (* fetch code *)
  let op = get_code vm.code.(vm.pc) in
  (* init registers *)
  let regs = {reg1=i 0; reg2=i 0; reg3=i 0; ireg=op.immed} in
  (* read registers *)
  regs.reg1 <- read_register vm regs op.read_reg1;
  trace ("read R1 " ^ string_of_value regs.reg1);
  regs.reg2 <- read_register vm regs op.read_reg2;
  trace ("read R2 " ^ string_of_value regs.reg2);
  regs.reg3 <- read_register vm regs op.read_reg3;
  trace ("read R3 " ^ string_of_value regs.reg3);
  (* ALU *)
  regs.reg1 <- handle_alu regs.reg1 regs.reg2 regs.reg3 regs.ireg op.alu_code;
  (* Write registers *)
  let w1 = get_register regs (fst op.write1) in
  trace ("write 1: " ^ string_of_value w1);
  write_register vm regs w1 (snd op.write1);
  let w2 = get_register regs (fst op.write2) in
  trace ("write 2: " ^ string_of_value w2);
  write_register vm regs w2 (snd op.write2);
  (* update pointers *)
  trace "update pointers";
  vm.pc <- handle_ptr regs vm.pc op.pc_ch;
  vm.stack_ptr <- handle_ptr regs vm.stack_ptr op.stack_ch;
  vm.call_ptr <- handle_ptr regs vm.call_ptr op.call_ch;
  if op.mem_ch then vm.memsize <- vm.memsize + value_to_int regs.reg1

let get_memory_int vm loc =
   let a = vm.memory.(loc/8) in
   let b = vm.memory.(loc/8+1) in
   let res = value_to_int (mem_load (I64 a) (I64 b) Types.I32Type None loc) in
(*   trace ("load int " ^ string_of_int res ^ " from " ^ string_of_int loc); *)
   res

let get_memory_char vm loc =
   let a = vm.memory.(loc/8) in
   let b = vm.memory.(loc/8+1) in
   let res = value_to_int (mem_load (I64 a) (I64 b) Types.I32Type (Some (Memory.Mem8, Memory.ZX)) loc) in
(*   trace ("load byte " ^ string_of_int res ^ " from " ^ string_of_int loc); *)
   Char.chr res

let get_vm_string vm loc =
  let res = ref "" in
(*  let loc = ref (get_memory_int vm loc) in *)
  let loc = ref loc in
  while Char.code (get_memory_char vm !loc) <> 0 do
    res := !res ^ String.make 1 (get_memory_char vm !loc);
    incr loc
  done;
  !res

let get_vm_buffer vm loc len =
  let res = ref "" in
(*  let loc = ref (get_memory_int vm loc) in *)
  for i = 0 to len - 1 do
    res := !res ^ String.make 1 (get_memory_char vm (loc+i));
  done;
  !res

let string_of_char byte = String.make 1 (Char.chr byte)

let get_vm_bytes vm loc len =
  let res = ref [] in
  for i = 0 to len-1 do
    res := Char.code (get_memory_char vm (loc+i)) :: !res
  done;
  List.rev !res

let rec get_datas vm ptr count =
  if count = 0 then [] else
  let len = get_memory_int vm (ptr+4) in
  (get_vm_bytes vm (get_memory_int vm ptr) len) :: get_datas vm (ptr+8) (count-1)

let vm_step vm = match vm.code.(vm.pc) with
 | BIN op ->
   inc_pc vm;
   vm.stack.(vm.stack_ptr-2) <- Eval_numeric.eval_binop op vm.stack.(vm.stack_ptr-2) vm.stack.(vm.stack_ptr-1);
   vm.stack_ptr <- vm.stack_ptr - 1
 | CMP op ->
   inc_pc vm;
   vm.stack.(vm.stack_ptr-2) <- value_of_bool (Eval_numeric.eval_relop op vm.stack.(vm.stack_ptr-2) vm.stack.(vm.stack_ptr-1));
   vm.stack_ptr <- vm.stack_ptr - 1
 | NOP -> inc_pc vm
 | JUMP x ->
   vm.pc <- x
 | JUMPI x ->
   vm.pc <- (if value_bool (vm.stack.(vm.stack_ptr-1)) then x else vm.pc + 1);
   vm.stack_ptr <- vm.stack_ptr - 1
 | DROP x ->
   inc_pc vm;
   vm.stack_ptr <- vm.stack_ptr - x
 | DUP x ->
   inc_pc vm;
   vm.stack.(vm.stack_ptr) <- vm.stack.(vm.stack_ptr-x);
   vm.stack_ptr <- vm.stack_ptr + 1
 | SWAP x ->
   inc_pc vm;
   vm.stack.(vm.stack_ptr-x) <- vm.stack.(vm.stack_ptr-1)
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
 | STUB "env . _debugString" ->
   let ptr = value_to_int (vm.stack.(vm.stack_ptr - 1)) in
   let ptr = !Flags.memory_offset + ptr in
   prerr_endline ("DEBUG: " ^ get_vm_string vm ptr);
   inc_pc vm
 | STUB "env . _debugBuffer" ->
   let ptr = value_to_int (vm.stack.(vm.stack_ptr - 2)) in
   let ptr = !Flags.memory_offset + ptr in
   let len = value_to_int (vm.stack.(vm.stack_ptr - 1)) in
   prerr_endline ("DEBUG: " ^ get_vm_buffer vm ptr len);
   inc_pc vm
 | STUB "env . _debugInt" ->
   let ptr = value_to_int (vm.stack.(vm.stack_ptr - 1)) in
   prerr_endline ("DEBUG: " ^ string_of_int ptr);
   inc_pc vm
 | STUB "env . _debugRead" ->
   let chr = value_to_int (vm.stack.(vm.stack_ptr - 1)) in
   trace ("reading " ^ string_of_char chr);
   inc_pc vm
 | STUB "env . _debugReadCount" ->
   let chr = value_to_int (vm.stack.(vm.stack_ptr - 1)) in
   trace ("reading " ^ string_of_int chr ^ " bytes");
   inc_pc vm
 | STUB "env . _debugSeek" ->
   let chr = value_to_int (vm.stack.(vm.stack_ptr - 1)) in
   trace ("seeking at " ^ string_of_int chr);
   inc_pc vm
 | STUB str ->
   prerr_endline ("STUB " ^ str);
   inc_pc vm
 | INITCALLTABLE x ->
   inc_pc vm;
   vm.calltable.(x) <- value_to_int vm.stack.(vm.stack_ptr-1);
   vm.stack_ptr <- vm.stack_ptr - 1
 | INITCALLTYPE x ->
   inc_pc vm;
   vm.calltable_types.(x) <- value_to_int64 vm.stack.(vm.stack_ptr-1);
   vm.stack_ptr <- vm.stack_ptr - 1
 | EXIT -> raise VmTrap
 | UNREACHABLE -> raise (Eval.Trap (Source.no_region, "unreachable executed"))
 | JUMPFORWARD x ->
   let idx = value_to_int vm.stack.(vm.stack_ptr-1) in
   let idx = if idx < 0 || idx >= x then x else idx in
   vm.pc <- vm.pc + 1 + idx;
   vm.stack_ptr <- vm.stack_ptr - 1
 | CALL x ->
   (* vm.call_stack.(vm.call_ptr) <- (vm.pc, vm.stack_ptr, vm.break_ptr);  I now guess that it won't need these *)
   vm.call_stack.(vm.call_ptr) <- vm.pc+1;
   vm.call_ptr <- vm.call_ptr + 1;
   vm.pc <- x
 | CALLI ->
   let addr = value_to_int vm.stack.(vm.stack_ptr-1) in
   vm.stack_ptr <- vm.stack_ptr - 1;
   vm.call_stack.(vm.call_ptr) <- vm.pc+1;
   vm.call_ptr <- vm.call_ptr + 1;
   (* prerr_endline ("call table from " ^ string_of_int addr ^ " got " ^ string_of_int vm.calltable.(addr)); *)
   vm.pc <- vm.calltable.(addr)
 | CHECKCALLI _ -> inc_pc vm
 | LABEL _ -> raise VmError (* these should have been processed away *)
 | RETURN ->
   vm.pc <- vm.call_stack.(vm.call_ptr-1);
   vm.call_ptr <- vm.call_ptr - 1
 | LOAD x ->
   inc_pc vm;
   let loc = value_to_int vm.stack.(vm.stack_ptr-1) + Int32.to_int x.offset in
   ( match x.ty, x.sz with
   | Types.I32Type, None ->
     let idx = loc land 0x07 in
     ( match idx with
     | 0 -> vm.stack.(vm.stack_ptr-1) <- I32 (Int64.to_int32 vm.memory.(loc lsr 3))
     | 4 -> vm.stack.(vm.stack_ptr-1) <- I32 (Int64.to_int32 (Int64.shift_right vm.memory.(loc lsr 3) 32))
     | _ ->
       let a = vm.memory.(loc/8) in
       let b = vm.memory.(loc/8+1) in
       if !Flags.trace then Printf.printf "Loading %s and %s\n" (Int64.to_string a) (Int64.to_string b);
       vm.stack.(vm.stack_ptr-1) <- mem_load (I64 a) (I64 b) x.ty x.sz loc )
   | Types.I32Type, Some (Memory.Mem8, Memory.ZX) ->
     let idx = loc mod 8 in
     vm.stack.(vm.stack_ptr-1) <- I32 (Int64.to_int32 (Int64.logand (Int64.shift_right vm.memory.(loc/8) (8*idx)) 0xffL))
   | _ ->
     let a = vm.memory.(loc/8) in
     let b = vm.memory.(loc/8+1) in
     if !Flags.trace then Printf.printf "Loading %s and %s\n" (Int64.to_string a) (Int64.to_string b);
     vm.stack.(vm.stack_ptr-1) <- mem_load (I64 a) (I64 b) x.ty x.sz loc )
 | STORE x ->
   inc_pc vm;
   let loc = value_to_int vm.stack.(vm.stack_ptr-2) + Int32.to_int x.offset in
   ( match x.ty, x.sz with
   | Types.I32Type, None ->
     let idx = loc mod 8 in
     ( match idx with
     | 0 ->
       let oldv = vm.memory.(loc lsr 3) in
       let v2 = match vm.stack.(vm.stack_ptr-1) with I32 v -> Int64.logand (Int64.of_int32 v) 0xffffffffL | _ -> 0L in
       vm.memory.(loc lsr 3) <- Int64.logor v2 (Int64.logand oldv 0xffffffff00000000L)
     | 4 ->
       let oldv = vm.memory.(loc lsr 3) in
       let v2 = match vm.stack.(vm.stack_ptr-1) with I32 v -> Int64.shift_left (Int64.of_int32 v) 32 | _ -> 0L in
       vm.memory.(loc lsr 3) <- Int64.logor v2 (Int64.logand oldv 0xffffffffL)
(*       
       let check = Int64.logor v2 (Int64.logand oldv 0xffffffff00000000L) in
       let mem = get_memory vm.memory loc in
       let v = vm.stack.(vm.stack_ptr-1) in
       memop mem v (Int64.of_int (loc-(loc/8)*8)) x.sz;
       let a, b = Byteutil.Decode.mini_memory mem in
       if check <> a then Printf.printf "What? got %Lx should be %Lx, value %Lx, old %Lx\n" check a v2 oldv;
       vm.memory.(loc/8) <- a;
       vm.memory.(loc/8+1) <- b *)
     | _ ->
      let mem = get_memory vm.memory loc in
      let v = vm.stack.(vm.stack_ptr-1) in
      memop mem v (Int64.of_int (loc-(loc/8)*8)) x.sz;
      let a, b = Byteutil.Decode.mini_memory mem in
      vm.memory.(loc/8) <- a;
      vm.memory.(loc/8+1) <- b )
   | _ ->
      let mem = get_memory vm.memory loc in
      let v = vm.stack.(vm.stack_ptr-1) in
      memop mem v (Int64.of_int (loc-(loc/8)*8)) x.sz;
      let a, b = Byteutil.Decode.mini_memory mem in
      vm.memory.(loc/8) <- a;
      vm.memory.(loc/8+1) <- b );
   vm.stack_ptr <- vm.stack_ptr - 2
 | DROP_N ->
   inc_pc vm;
   vm.stack_ptr <- vm.stack_ptr - value_to_int vm.stack.(vm.stack_ptr-1)
 | INPUTSIZE ->
   inc_pc vm;
   vm.stack.(vm.stack_ptr-1) <- i vm.input.file_size.(value_to_int vm.stack.(vm.stack_ptr-1))
 | INPUTNAME ->
   inc_pc vm;
   let s1 = value_to_int vm.stack.(vm.stack_ptr-1) in
   let s2 = value_to_int vm.stack.(vm.stack_ptr-2) in
   let str = vm.input.file_name.(s2) in
   let chr = if s1 < String.length str then Char.code str.[s1] else 0 in
   vm.stack.(vm.stack_ptr-2) <- i (chr);
   vm.stack_ptr <- vm.stack_ptr - 1
 | INPUTDATA ->
   inc_pc vm;
   let s1 = value_to_int vm.stack.(vm.stack_ptr-1) in
   let s2 = value_to_int vm.stack.(vm.stack_ptr-2) in
   vm.stack.(vm.stack_ptr-2) <- i (Char.code vm.input.file_data.(s2).[s1]);
   vm.stack_ptr <- vm.stack_ptr - 1
 | OUTPUTSIZE ->
   inc_pc vm;
   let s1 = value_to_int vm.stack.(vm.stack_ptr-1) in
   let s2 = value_to_int vm.stack.(vm.stack_ptr-2) in
   vm.input.file_size.(s2) <- s1;
   vm.input.file_data.(s2) <- Bytes.make s1 (Char.chr 0);
   vm.stack_ptr <- vm.stack_ptr - 2
 | OUTPUTNAME ->
   inc_pc vm;
   let s1 = value_to_int vm.stack.(vm.stack_ptr-1) in
   let s2 = value_to_int vm.stack.(vm.stack_ptr-2) in
   let s3 = value_to_int vm.stack.(vm.stack_ptr-3) in
   vm.stack_ptr <- vm.stack_ptr - 3;
   let str = vm.input.file_name.(s3) in
   let str = if String.length str = 1 then String.make 256 (Char.chr 0) else str in
   Bytes.set str s2 (Char.chr s1);
   vm.input.file_name.(s3) <- str
 | OUTPUTDATA ->
   inc_pc vm;
   let s1 = value_to_int vm.stack.(vm.stack_ptr-1) in
   let s2 = value_to_int vm.stack.(vm.stack_ptr-2) in
   let s3 = value_to_int vm.stack.(vm.stack_ptr-3) in
   vm.stack_ptr <- vm.stack_ptr - 3;
   Bytes.set vm.input.file_data.(s3) s2 (Char.chr s1)
 | LOADGLOBAL x ->
   inc_pc vm;
   vm.stack.(vm.stack_ptr) <- vm.globals.(x);
   vm.stack_ptr <- vm.stack_ptr + 1
 | STOREGLOBAL x ->
   inc_pc vm;
   vm.globals.(x) <- vm.stack.(vm.stack_ptr-1);
   vm.stack_ptr <- vm.stack_ptr - 1
 | CURMEM ->
   inc_pc vm;
   vm.stack.(vm.stack_ptr) <- I32 (Int32.of_int vm.memsize);
   vm.stack_ptr <- vm.stack_ptr + 1
 | GROW ->
   inc_pc vm;
   vm.memsize <- vm.memsize + value_to_int vm.stack.(vm.stack_ptr-1);
   vm.stack_ptr <- vm.stack_ptr - 1
 | SETSTACK v ->
   let sz = pow2 v in
   vm.stack <- Array.make sz (i 0);
   inc_pc vm
 | SETCALLSTACK v ->
   let sz = pow2 v in
   vm.call_stack <- Array.make sz 0;
   inc_pc vm
 | SETTABLE v ->
   let sz = pow2 v in
   vm.calltable <- Array.make sz (-1);
   vm.calltable_types <- Array.make sz 0L;
   inc_pc vm
 | SETMEMORY v ->
   let sz = pow2 v in
   vm.memory <- Array.make sz 0L;
   inc_pc vm
 | SETGLOBALS v ->
   let sz = pow2 v in
   vm.globals <- Array.make sz (i 0);
   inc_pc vm

open Types

let type_size = function
 | I32Type -> 4
 | F32Type -> 4
 | I64Type -> 8
 | F64Type -> 8

let size_size = function
 | Memory.Mem8 -> 1
 | Memory.Mem16 -> 2
 | Memory.Mem32 -> 4

let load_memory_limit addr (op:'a memop) =
  let x = value_to_int addr + Int32.to_int op.offset in
  match op.sz with
  | None -> x - 1 + type_size op.ty
  | Some (sz,_) -> x - 1 + size_size sz

let store_memory_limit addr (op:'a memop) =
  let x = value_to_int addr + Int32.to_int op.offset in
  match op.sz with
  | None -> x - 1 + type_size op.ty
  | Some sz -> x - 1 + size_size sz

let test_errors vm = match vm.code.(vm.pc) with
 | PUSH _ | DUP _ ->
   if vm.stack_ptr < 0 then raise (Eval.Exhaustion (Source.no_region, "stack underflow"))
   else if Array.length vm.stack <= vm.stack_ptr then raise (Eval.Exhaustion (Source.no_region, "call stack exhausted"))
 | CALL _ -> if Array.length vm.call_stack <= vm.call_ptr then raise (Eval.Exhaustion (Source.no_region, "call stack exhausted"))
 | CHECKCALLI x ->
   let addr = value_to_int vm.stack.(vm.stack_ptr-1) in
   let len = Array.length vm.calltable in
   if addr > len then raise (Eval.Trap (Source.no_region, "undefined element")) else
   if addr < 0 then raise (Eval.Trap (Source.no_region, "undefined element")) else
(*   if vm.calltable.(addr) = -1 then raise (Eval.Trap (Source.no_region, "uninitialized element " ^ string_of_int addr)) else *)
   if vm.calltable.(addr) = -1 then raise (Eval.Trap (Source.no_region, "undefined element")) else
   if vm.calltable_types.(addr) <> x then begin
     trace ("At address " ^ string_of_int addr);
     trace ("Expected " ^ Int64.to_string x);
     trace ("Was " ^ Int64.to_string vm.calltable_types.(addr));
     raise (Eval.Trap (Source.no_region, "indirect call signature mismatch"))
   end else
   if Array.length vm.call_stack <= vm.call_ptr then raise (Eval.Exhaustion (Source.no_region, "call stack exhausted"))
 | LOAD op ->
    let loc = vm.stack.(vm.stack_ptr-1) in
(*    let _ = prerr_endline (string_of_value loc) in *)
    if load_memory_limit loc op >= vm.memsize*64*1024 then raise (Eval.Trap (Source.no_region, "out of bounds memory access"))
    else if value_to_int loc >= vm.memsize*64*1024 then raise (Eval.Trap (Source.no_region, "out of bounds memory access"))
    else if Int32.to_int op.offset < 0 then raise (Eval.Trap (Source.no_region, "out of bounds memory access"))
 | STORE op ->
    if store_memory_limit vm.stack.(vm.stack_ptr-2) op >= vm.memsize*64*1024 then raise (Eval.Trap (Source.no_region, "out of bounds memory access"))
    else if value_to_int vm.stack.(vm.stack_ptr-2) >= vm.memsize*64*1024 then raise (Eval.Trap (Source.no_region, "out of bounds memory access"))
    else if Int32.to_int op.offset < 0 then raise (Eval.Trap (Source.no_region, "out of bounds memory access"))
 | _ -> ()

let pack_label a =
  let open Memory in
  match a with
  | SX -> "S"
  | ZX -> "U"

let size_label a =
  let open Memory in
  match a with
  | Mem8 -> "8"
  | Mem16 -> "16"
  | Mem32 -> "32"

let load_label op =
  ( match op.ty with
  | I32Type -> "32"
  | I64Type -> "64"
  | F32Type -> "F32"
  | F64Type -> "F64" ) ^
  ( match op.sz with
  | Some (sz, pack) -> "_" ^ pack_label pack ^ size_label sz
  | None -> "" )

let trace_step vm = match vm.code.(vm.pc) with
 | NOP -> "NOP"
 | STUB str -> "STUB " ^ str
 | UNREACHABLE -> "UNREACHABLE"
 | EXIT -> "EXIT"
 | INPUTSIZE -> "INPUTSIZE"
 | INPUTNAME -> "INPUTNAME"
 | INPUTDATA ->
   let s1 = value_to_int vm.stack.(vm.stack_ptr-1) in
   let s2 = value_to_int vm.stack.(vm.stack_ptr-2) in
   let str = String.make 1 vm.input.file_data.(s2).[s1] in
   "INPUTDATA " ^ str
 | OUTPUTSIZE -> "OUTPUTSIZE"
 | OUTPUTNAME -> "OUTPUTNAME"
 | OUTPUTDATA -> "OUTPUTDATA"
 | JUMP x -> "JUMP"
 | JUMPI x ->
   let x = vm.stack.(vm.stack_ptr-1) in
   "JUMPI " ^ (if value_bool x then " jump" else " no jump") ^ " " ^ string_of_value x
 | JUMPFORWARD x -> "JUMPFORWARD " ^ string_of_value vm.stack.(vm.stack_ptr-1)
 | CALL x -> "CALL " ^ string_of_int x
 | LABEL _ -> "LABEL ???"
 | RETURN -> "RETURN"
 | LOAD x ->
   let loc = value_to_int vm.stack.(vm.stack_ptr-1) + Int32.to_int x.offset in
   let a = vm.memory.(loc/8) in
   let b = vm.memory.(loc/8+1) in
   let v = mem_load (I64 a) (I64 b) x.ty x.sz loc in
   "LOAD" ^ load_label x ^ " from " ^ string_of_value vm.stack.(vm.stack_ptr-1) ^ " offset " ^ Int32.to_string x.offset ^ " got " ^ string_of_value v
 | STORE x -> "STORE " ^ string_of_value vm.stack.(vm.stack_ptr-1) ^ " to " ^ string_of_value vm.stack.(vm.stack_ptr-2) ^ " offset " ^ Int32.to_string x.offset
   (* 
   let open Byteutil in
   trace ("STORING " ^ Byteutil.w256_to_string (get_value (I64 a)) ^ " & " ^ Byteutil.w256_to_string (get_value (I64 b))); *)
 | DROP x -> "DROP" ^ string_of_int x
 | DROP_N -> "DROP_N " ^ string_of_value vm.stack.(vm.stack_ptr-1)
 | DUP x -> "DUP" ^ string_of_int x ^ ": " ^ string_of_value vm.stack.(vm.stack_ptr-x)
 | SWAP x -> "SWAP" ^ string_of_int x ^ ": " ^ string_of_value vm.stack.(vm.stack_ptr-1)
 | LOADGLOBAL x -> "LOADGLOBAL " ^ string_of_int x ^ ": " ^ string_of_value vm.globals.(x)
 | STOREGLOBAL x -> "STOREGLOBAL " ^ string_of_int x ^ ": " ^ string_of_value vm.stack.(vm.stack_ptr-1)
 | INITCALLTABLE x -> "INITCALLTABLE " ^ string_of_int x ^ ": " ^ string_of_value vm.stack.(vm.stack_ptr-1)
 | INITCALLTYPE x -> "INITCALLTYPE " ^ string_of_int x ^ ": " ^ string_of_value vm.stack.(vm.stack_ptr-1)
 | CURMEM -> "CURMEM"
 | GROW -> "GROW"
 | PUSH lit -> "PUSH " ^ string_of_value lit
 | CONV op -> "CONV"
 | UNA op -> "UNA"
 | TEST op -> "TEST"
 | BIN op -> "BIN " ^ string_of_value vm.stack.(vm.stack_ptr-2) ^ " " ^ string_of_value vm.stack.(vm.stack_ptr-1)
 | CMP op -> "CMP " ^ string_of_value vm.stack.(vm.stack_ptr-2) ^ " " ^ string_of_value vm.stack.(vm.stack_ptr-1)
 | CALLI -> "CALLI"
 | CHECKCALLI x -> "CHECKCALLI"
 | SETSTACK v -> "SETSTACK"
 | SETCALLSTACK v -> "SETCALLSTACK"
 | SETTABLE v -> "SETTABLE"
 | SETMEMORY v -> "SETMEMORY"
 | SETGLOBALS v -> "SETGLOBALS"

let stack_to_string vm n =
  let res = ref "" in
  for i = max 0 (vm.stack_ptr - n) to vm.stack_ptr - 1 do
    res := !res ^ " ; " ^ string_of_value vm.stack.(i)
  done;
  !res


