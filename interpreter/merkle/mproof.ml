
open Merkle
open Values
open Mrun
open Mbinary

let trace = Merkle.trace

(* so now we need the different proofs *)

type location_proof =
 | SimpleProof
 | LocationProof of (int * w256 list)

type pointer =
 | PcPtr
 | StackPtr
 | CallPtr
 | BreakPtr

let make_fetch_code vm =
  trace "microp word";
  let code = Array.map (fun v -> microp_word (get_code v)) vm.code in
  trace "fetch code";
  let loc_proof = location_proof code vm.pc in
  trace "fetched code";
  (vm_to_bin vm, loc_proof)

let read_position vm regs = function
 | NoIn -> 0
 | Immed -> 0
 | ReadPc -> 0
 | ReadStackPtr -> 0
 | MemsizeIn -> 0
 | GlobalIn -> value_to_int regs.reg1
 | StackIn0 ->
   trace ("Read stack from " ^ string_of_int (vm.stack_ptr-1));
   vm.stack_ptr-1
 | StackIn1 -> vm.stack_ptr-2
 | StackInReg -> vm.stack_ptr-value_to_int regs.reg1
 | StackInReg2 -> vm.stack_ptr-value_to_int regs.reg2
 | BreakLocIn -> vm.break_ptr-1
 | BreakStackIn -> vm.break_ptr-1
 | BreakLocInReg -> vm.break_ptr-1-value_to_int regs.reg1
 | BreakStackInReg -> vm.break_ptr-1-value_to_int regs.reg1
 | CallIn -> vm.call_ptr-1
 | MemoryIn1 -> (value_to_int regs.reg1+value_to_int regs.ireg) / 8
 | MemoryIn2 -> (value_to_int regs.reg1+value_to_int regs.ireg) / 8 + 1
 | TableIn -> value_to_int regs.reg1
 | TableTypeIn -> value_to_int regs.reg1

let write_position vm regs = function
 | NoOut -> 0
 | GlobalOut -> value_to_int regs.reg1
 | CallOut -> vm.call_ptr
 | MemoryOut1 (_,_) -> (value_to_int regs.reg1+value_to_int regs.ireg) / 8
 | MemoryOut2 (_,_) -> (value_to_int regs.reg1+value_to_int regs.ireg) / 8  + 1
 | StackOut0 -> vm.stack_ptr
 | StackOut1 -> vm.stack_ptr-1
 | StackOutReg1 -> vm.stack_ptr-value_to_int regs.reg1
 | BreakLocOut -> vm.break_ptr
 | BreakStackOut -> vm.break_ptr
 | StackOut2 -> vm.stack_ptr-2

let loc_proof loc arr = (loc, location_proof arr loc)

open Byteutil

let get_read_location m loc =
 let pos = read_position m.m_vm m.m_regs loc in
 let vm = m.m_vm in
 match loc with
 | NoIn -> SimpleProof
 | Immed -> SimpleProof
 | ReadPc -> SimpleProof
 | ReadStackPtr -> SimpleProof
 | MemsizeIn -> SimpleProof
 | GlobalIn -> LocationProof (loc_proof pos (Array.map get_value vm.globals))
 | StackIn0 -> LocationProof (loc_proof pos (Array.map get_value vm.stack))
 | StackIn1 -> LocationProof (loc_proof pos (Array.map get_value vm.stack))
 | StackInReg -> LocationProof (loc_proof pos (Array.map get_value vm.stack))
 | StackInReg2 -> LocationProof (loc_proof pos (Array.map get_value vm.stack))
 | MemoryIn1 -> LocationProof (loc_proof pos (Array.map (fun i -> get_value (I64 i)) vm.memory))
 | MemoryIn2 -> LocationProof (loc_proof pos (Array.map (fun i -> get_value (I64 i)) vm.memory))
 | TableIn -> LocationProof (loc_proof pos (Array.map u256 vm.calltable))
 | TableTypeIn -> LocationProof (loc_proof pos (Array.map (fun i -> get_value (I64 i)) vm.calltable_types))
 | BreakLocIn -> LocationProof (loc_proof pos (Array.map (fun a -> u256 (fst a)) vm.break_stack))
 | BreakStackIn -> LocationProof (loc_proof pos (Array.map (fun a -> u256 (snd a)) vm.break_stack))
 | BreakLocInReg -> LocationProof (loc_proof pos (Array.map (fun a -> u256 (fst a)) vm.break_stack))
 | BreakStackInReg -> LocationProof (loc_proof pos (Array.map (fun a -> u256 (snd a)) vm.break_stack))
 | CallIn -> LocationProof (loc_proof pos (Array.map u256 vm.call_stack))

let get_write_location m loc =
 let pos = write_position m.m_vm m.m_regs loc in
 let vm = m.m_vm in
 match loc with
 | NoOut -> SimpleProof
 | BreakLocOut -> LocationProof (loc_proof pos (Array.map (fun a -> u256 (fst a)) vm.break_stack))
 | BreakStackOut -> LocationProof (loc_proof pos (Array.map (fun a -> u256 (snd a)) vm.break_stack))
 | StackOutReg1 -> LocationProof (loc_proof pos (Array.map get_value vm.stack))
 | StackOut0 -> LocationProof (loc_proof pos (Array.map get_value vm.stack))
 | StackOut1 -> LocationProof (loc_proof pos (Array.map get_value vm.stack))
 | StackOut2 -> LocationProof (loc_proof pos (Array.map get_value vm.stack))
 | MemoryOut1 _ -> LocationProof (loc_proof pos (Array.map (fun i -> get_value (I64 i)) vm.memory))
 | MemoryOut2 _ -> LocationProof (loc_proof pos (Array.map (fun i -> get_value (I64 i)) vm.memory))
 | CallOut -> LocationProof (loc_proof pos (Array.map u256 vm.call_stack))
 | GlobalOut -> LocationProof (loc_proof pos (Array.map get_value vm.globals))

let make_register_proof1 m =
  (machine_to_bin m, vm_to_bin m.m_vm, get_read_location m m.m_microp.read_reg1)

let make_register_proof2 m =
  (machine_to_bin m, vm_to_bin m.m_vm, get_read_location m m.m_microp.read_reg2)

let make_register_proof3 m =
  (machine_to_bin m, vm_to_bin m.m_vm, get_read_location m m.m_microp.read_reg3)

let make_write_proof m wr =
  (machine_to_bin m, vm_to_bin m.m_vm, get_write_location m (snd wr))

type micro_proof = {
  fetch_code_proof : vm_bin * w256 list;
  init_regs_proof : vm_bin * microp;
  read_register_proof1 : machine_bin * vm_bin * location_proof;
  read_register_proof2 : machine_bin * vm_bin * location_proof;
  read_register_proof3 : machine_bin * vm_bin * location_proof;
  alu_proof : machine_bin;
  write_proof1 : machine_bin * vm_bin * location_proof;
  write_proof2 : machine_bin * vm_bin * location_proof;
  update_ptr_proof1 : machine_bin * vm_bin;
  update_ptr_proof2 : machine_bin * vm_bin;
  update_ptr_proof3 : machine_bin * vm_bin;
  update_ptr_proof4 : machine_bin * vm_bin;
  memsize_proof : machine_bin * vm_bin;
  finalize_proof : vm_bin;
}

let micro_step_proofs vm =
  (* fetch code *)
  let op = get_code vm.code.(vm.pc) in
  let fetch_code_proof = make_fetch_code vm in
  (* init registers *)
  let regs = {reg1=i 0; reg2=i 0; reg3=i 0; ireg=op.immed} in
  let init_regs_proof = (vm_to_bin vm, op) in
  (* read registers *)
  let m = {m_vm=vm; m_regs=regs; m_microp=op} in
  let read_register_proof1 = make_register_proof1 m in
  regs.reg1 <- read_register vm regs op.read_reg1;
  let read_register_proof2 = make_register_proof2 m in
  regs.reg2 <- read_register vm regs op.read_reg2;
  let read_register_proof3 = make_register_proof3 m in
  regs.reg3 <- read_register vm regs op.read_reg3;
  (* ALU *)
  let alu_proof = machine_to_bin m in
  regs.reg1 <- handle_alu regs.reg1 regs.reg2 regs.reg3 regs.ireg op.alu_code;
  (* Write registers *)
  let write_proof1 = make_write_proof m op.write1 in
  write_register vm regs (get_register regs (fst op.write1)) (snd op.write1);
  let write_proof2 = make_write_proof m op.write2 in
  write_register vm regs (get_register regs (fst op.write2)) (snd op.write2);
  (* update pointers *)
  let update_ptr_proof1 = (machine_to_bin m, vm_to_bin vm) in
  vm.pc <- handle_ptr regs vm.pc op.pc_ch;
  let update_ptr_proof2 = (machine_to_bin m, vm_to_bin vm) in
  vm.break_ptr <- handle_ptr regs vm.break_ptr op.break_ch;
  let update_ptr_proof3 = (machine_to_bin m, vm_to_bin vm) in
  vm.stack_ptr <- handle_ptr regs vm.stack_ptr op.stack_ch;
  let update_ptr_proof4 = (machine_to_bin m, vm_to_bin vm) in
  vm.call_ptr <- handle_ptr regs vm.call_ptr op.call_ch;
  let memsize_proof = (machine_to_bin m, vm_to_bin vm) in
  if op.mem_ch then vm.memsize <- vm.memsize + value_to_int regs.reg1;
  let finalize_proof = vm_to_bin vm in
  {fetch_code_proof; init_regs_proof; 
   read_register_proof1; read_register_proof2; read_register_proof3; alu_proof; write_proof1; write_proof2;
   update_ptr_proof1; update_ptr_proof2; update_ptr_proof3; update_ptr_proof4;
   memsize_proof; finalize_proof}

(* Doing checks *)

let check_fetch state1 state2 (vm_bin, proof) =
  let microp = get_leaf vm_bin.bin_pc proof in
  let c1 = (state1 = hash_vm_bin vm_bin) in
  let c2 = (vm_bin.bin_code = get_root vm_bin.bin_pc proof) in
  let c3 = (state2 = keccak (hash_vm_bin vm_bin) microp) in
  if not c1 then trace "Bad initial state";
  if not c2 then trace "Bad code";
  if not c3 then trace "Bad final state";
  c1 && c2 && c3
(*  state1 = hash_vm_bin vm_bin &&
  vm_bin.bin_code = get_root vm_bin.bin_pc proof &&
  state2 = keccak (hash_vm_bin vm_bin) microp *)

let check_init_registers state1 state2 (vm_bin, microp) =
  let regs = {reg1 = i 0; reg2 = i 0; reg3 = i 0; ireg=microp.immed} in
  let vm_bin = hash_vm_bin vm_bin in
  state1 = keccak vm_bin (microp_word microp) &&
  state2 = hash_machine_bin {bin_vm=vm_bin; bin_microp=microp; bin_regs=regs}

let value_from_proof = function
 | SimpleProof ->
    trace "Was empty";
    raise EmptyArray
 | LocationProof (loc, lst) ->
    trace "Ok here";
    get_leaf loc lst

let read_from_proof regs vm proof = function
 | NoIn -> get_value (i 0)
 | Immed -> get_value regs.ireg
 | ReadPc -> get_value (i (vm.bin_pc+1))
 | MemsizeIn -> get_value (i vm.bin_memsize)
 | ReadStackPtr -> get_value (i vm.bin_stack_ptr)
 | _ -> value_from_proof proof

let read_position_bin vm regs = function
 | NoIn -> 0
 | Immed -> 0
 | ReadPc -> 0
 | ReadStackPtr -> 0
 | MemsizeIn -> 0
 | GlobalIn -> value_to_int regs.reg1
 | StackIn0 -> vm.bin_stack_ptr-1
 | StackIn1 -> vm.bin_stack_ptr-2
 | StackInReg -> vm.bin_stack_ptr-value_to_int regs.reg1
 | StackInReg2 -> vm.bin_stack_ptr-value_to_int regs.reg2
 | BreakLocIn -> vm.bin_break_ptr-1
 | BreakStackIn -> vm.bin_break_ptr-1
 | BreakLocInReg -> vm.bin_break_ptr-1-value_to_int regs.reg1
 | BreakStackInReg -> vm.bin_break_ptr-1-value_to_int regs.reg1
 | CallIn -> vm.bin_call_ptr-1
 | MemoryIn1 -> (value_to_int regs.reg1+value_to_int regs.ireg) / 8
 | MemoryIn2 -> (value_to_int regs.reg1+value_to_int regs.ireg) / 8 + 1
 | TableIn -> value_to_int regs.reg1
 | TableTypeIn -> value_to_int regs.reg1

let write_position_bin vm regs = function
 | NoOut -> 0
 | GlobalOut -> value_to_int regs.reg1
 | CallOut -> vm.bin_call_ptr
 | MemoryOut1 _ -> (value_to_int regs.reg1+value_to_int regs.ireg) / 8
 | MemoryOut2 _ -> (value_to_int regs.reg1+value_to_int regs.ireg) / 8 + 1
 | StackOut0 -> vm.bin_stack_ptr
 | StackOut1 -> vm.bin_stack_ptr-1
 | StackOutReg1 -> vm.bin_stack_ptr-value_to_int regs.reg1
 | BreakLocOut -> vm.bin_break_ptr
 | BreakStackOut -> vm.bin_break_ptr
 | StackOut2 -> vm.bin_stack_ptr-2

let read_root_bin vm = function
 | GlobalIn -> vm.bin_globals
 | StackIn0 -> vm.bin_stack
 | StackIn1 -> vm.bin_stack
 | StackInReg -> vm.bin_stack
 | StackInReg2 -> vm.bin_stack
 | BreakLocIn -> vm.bin_break_stack1
 | BreakStackIn -> vm.bin_break_stack2
 | BreakLocInReg -> vm.bin_break_stack1
 | BreakStackInReg -> vm.bin_break_stack2
 | CallIn -> vm.bin_call_stack
 | MemoryIn1 -> vm.bin_memory
 | MemoryIn2 -> vm.bin_memory
 | TableIn -> vm.bin_calltable
 | TableTypeIn -> vm.bin_calltable_types
 | _ -> assert false

let write_root_bin vm = function
 | GlobalOut -> vm.bin_globals
 | CallOut -> vm.bin_call_stack
 | MemoryOut1 _ -> vm.bin_memory
 | MemoryOut2 _ -> vm.bin_memory
 | StackOut0 -> vm.bin_stack
 | StackOut1 -> vm.bin_stack
 | StackOutReg1 -> vm.bin_stack
 | BreakLocOut -> vm.bin_break_stack1
 | BreakStackOut -> vm.bin_break_stack2
 | StackOut2 -> vm.bin_stack
 | _ -> assert false

let check_read_proof regs vm proof = function
 | NoIn -> true
 | Immed -> true
 | ReadPc -> true
 | MemsizeIn -> true
 | ReadStackPtr -> true
 | a ->
    ( match proof with
    | LocationProof (loc, lst) ->
       read_position_bin vm regs a = loc &&
       read_root_bin vm a = get_root loc lst
    | _ -> false ) 

let check_write_proof regs vm proof = function
 | NoOut -> true
 | a ->
    ( match proof with
    | LocationProof (loc, lst) ->
       write_position_bin vm regs a = loc &&
       write_root_bin vm a = get_root loc lst
    | _ -> false ) 

let check_read1_proof state1 state2 (m, vm, proof) =
  let regs = {(regs_to_bin m.bin_regs) with b_reg1 = read_from_proof m.bin_regs vm proof m.bin_microp.read_reg1} in
  state1 = hash_machine_bin m &&
  m.bin_vm = hash_vm_bin vm &&
  check_read_proof m.bin_regs vm proof m.bin_microp.read_reg1 &&
  state2 = hash_machine_regs m regs

let check_read2_proof state1 state2 (m, vm, proof) =
  let regs = {(regs_to_bin m.bin_regs) with b_reg2 = read_from_proof m.bin_regs vm proof m.bin_microp.read_reg2} in
  state1 = hash_machine_bin m &&
  m.bin_vm = hash_vm_bin vm &&
  check_read_proof m.bin_regs vm proof m.bin_microp.read_reg2 &&
  state2 = hash_machine_regs m regs

let check_read3_proof state1 state2 (m, vm, proof) =
  let regs = {(regs_to_bin m.bin_regs) with b_reg3 = read_from_proof m.bin_regs vm proof m.bin_microp.read_reg3} in
  state1 = hash_machine_bin m &&
  m.bin_vm = hash_vm_bin vm &&
  check_read_proof m.bin_regs vm proof m.bin_microp.read_reg3 &&
  state2 = hash_machine_regs m regs

let check_alu_proof state1 state2 m =
  let regs = {(m.bin_regs) with reg1 = handle_alu m.bin_regs.reg1 m.bin_regs.reg2 m.bin_regs.reg3 m.bin_regs.ireg m.bin_microp.alu_code} in
  state1 = hash_machine_bin m &&
  state2 = hash_machine_bin {m with bin_regs=regs}

let check_finalize state1 state2 m =
  state1 = hash_machine_bin m &&
  state2 = m.bin_vm

let check_update_stack_ptr state1 state2 (m,vm) =
  let vm2 = {vm with bin_stack_ptr=handle_ptr m.bin_regs vm.bin_stack_ptr m.bin_microp.stack_ch} in
  let m2 = {m with bin_vm=hash_vm_bin vm2} in
  state1 = hash_machine_bin m &&
  m.bin_vm = hash_vm_bin vm &&
  state2 = hash_machine_bin m2

let check_update_pc state1 state2 (m,vm) =
  let vm2 = {vm with bin_pc=handle_ptr m.bin_regs vm.bin_pc m.bin_microp.pc_ch} in
  let m2 = {m with bin_vm=hash_vm_bin vm2} in
  state1 = hash_machine_bin m &&
  m.bin_vm = hash_vm_bin vm &&
  state2 = hash_machine_bin m2

let check_update_break_ptr state1 state2 (m,vm) =
  let vm2 = {vm with bin_break_ptr=handle_ptr m.bin_regs vm.bin_break_ptr m.bin_microp.break_ch} in
  let m2 = {m with bin_vm=hash_vm_bin vm2} in
  state1 = hash_machine_bin m &&
  m.bin_vm = hash_vm_bin vm &&
  state2 = hash_machine_bin m2

let check_update_call_ptr state1 state2 (m,vm) =
  let vm2 = {vm with bin_call_ptr=handle_ptr m.bin_regs vm.bin_call_ptr m.bin_microp.call_ch} in
  let m2 = {m with bin_vm=hash_vm_bin vm2} in
  state1 = hash_machine_bin m &&
  m.bin_vm = hash_vm_bin vm &&
  state2 = hash_machine_bin m2

let check_update_memsize (state1:bytes) (state2:bytes) (m,vm) =
  let vm2 = {vm with bin_memsize=(if m.bin_microp.mem_ch then value_to_int m.bin_regs.reg1 else 0) + vm.bin_memsize} in
  state1 = hash_machine_bin m &&
  m.bin_vm = hash_vm_bin vm &&
  state2 = hash_vm_bin vm2

let merkle_change nv = function
 | LocationProof (loc, lst) ->
   let lst = set_leaf loc nv lst in
   get_root loc lst
 | _ -> assert false

let merkle_change_memory1 regs nv sz = function
 | LocationProof (loc, lst) ->
   let old = get_leaf loc lst in
   trace ("Old memory 1: " ^ w256_to_string old);
   let addr = value_to_int regs.reg1+value_to_int regs.ireg in
   let mem = mini_memory (Byteutil.Decode.word old) 0L in
   memop mem (I64 (Byteutil.Decode.word nv)) (Int64.of_int (addr-(addr/8)*8)) sz;
   let res = fst (Byteutil.Decode.mini_memory mem) in
   trace ("New memory 1: " ^ w256_to_string (get_value (I64 res)));
   let lst = set_leaf loc (get_value (I64 res)) lst in
   get_root loc lst
 | _ -> assert false

let merkle_change_memory2 regs nv sz = function
 | LocationProof (loc, lst) ->
   let old = get_leaf loc lst in
   trace ("Old memory 2: " ^ w256_to_string old);
   let addr = value_to_int regs.reg1+value_to_int regs.ireg in
   let mem = mini_memory 0L (Byteutil.Decode.word old) in
   memop mem (I64 (Byteutil.Decode.word nv)) (Int64.of_int (addr-(addr/8)*8)) sz;
   let res = snd (Byteutil.Decode.mini_memory mem) in
   trace ("New memory 2: " ^ w256_to_string (get_value (I64 res)));
   let lst = set_leaf loc (get_value (I64 res)) lst in
   get_root loc lst
 | _ -> assert false

let write_register_bin proof vm regs v = function
 | NoOut -> vm
 | GlobalOut -> {vm with bin_globals=merkle_change v proof}
 | CallOut -> {vm with bin_call_stack=merkle_change v proof}
 | BreakLocOut -> {vm with bin_break_stack1=merkle_change v proof}
 | BreakStackOut -> {vm with bin_break_stack2=merkle_change v proof}
 | StackOutReg1 -> {vm with bin_stack=merkle_change v proof}
 | StackOut0 -> {vm with bin_stack=merkle_change v proof}
 | StackOut1 -> {vm with bin_stack=merkle_change v proof}
 | StackOut2 -> {vm with bin_stack=merkle_change v proof}
 (* Should we apply the type here? *)
 | MemoryOut1 (_,sz) -> {vm with bin_memory=merkle_change_memory1 regs v sz proof}
 | MemoryOut2 (_,sz) -> {vm with bin_memory=merkle_change_memory2 regs v sz proof}

let check_write1_proof state1 state2 (m, vm, proof) =
  let vm2 = write_register_bin proof vm m.bin_regs (get_value (get_register m.bin_regs (fst m.bin_microp.write1))) (snd m.bin_microp.write1) in
  state1 = hash_machine_bin m &&
  m.bin_vm = hash_vm_bin vm &&
  check_write_proof m.bin_regs vm proof (snd m.bin_microp.write1) &&
  state2 = hash_machine_bin {m with bin_vm=hash_vm_bin vm2}

let check_write2_proof state1 state2 (m, vm, proof) =
  let vm2 = write_register_bin proof vm m.bin_regs (get_value (get_register m.bin_regs (fst m.bin_microp.write2))) (snd m.bin_microp.write2) in
  state1 = hash_machine_bin m &&
  m.bin_vm = hash_vm_bin vm &&
  check_write_proof m.bin_regs vm proof (snd m.bin_microp.write2) &&
  state2 = hash_machine_bin {m with bin_vm=hash_vm_bin vm2}

let t1 (a,_,_) = a

let to_hex a = "\"0x" ^ w256_to_string a ^ "\""

let vm_to_string vm =
  "{" ^
  " \"code\": " ^ to_hex vm.bin_code ^ "," ^
  " \"stack\": " ^ to_hex vm.bin_stack ^ "," ^
  " \"memory\": " ^ to_hex vm.bin_memory ^ "," ^
  " \"break_stack1\": " ^ to_hex vm.bin_break_stack1 ^ "," ^
  " \"break_stack2\": " ^ to_hex vm.bin_break_stack2 ^ "," ^
  " \"call_stack\": " ^ to_hex vm.bin_call_stack ^ "," ^
  " \"globals\": " ^ to_hex vm.bin_globals ^ "," ^
  " \"calltable\": " ^ to_hex vm.bin_calltable ^ "," ^
  " \"calltypes\": " ^ to_hex vm.bin_calltable_types ^ "," ^
  " \"pc\": " ^ string_of_int vm.bin_pc ^ "," ^
  " \"stack_ptr\": " ^ string_of_int vm.bin_stack_ptr ^ "," ^
  " \"break_ptr\": " ^ string_of_int vm.bin_break_ptr ^ "," ^
  " \"call_ptr\": " ^ string_of_int vm.bin_call_ptr ^ "," ^
  " \"memsize\": " ^ string_of_int vm.bin_memsize ^ " " ^
  "}"

let list_to_string lst = "[" ^ String.concat ", " (List.map to_hex lst) ^ "]"

let machine_to_string m =
  "{" ^
  " \"vm\": " ^ to_hex m.bin_vm ^ "," ^
  " \"op\": " ^ to_hex (microp_word m.bin_microp) ^ "," ^
  " \"reg1\": " ^ to_hex (get_value m.bin_regs.reg1) ^ "," ^
  " \"reg2\": " ^ to_hex (get_value m.bin_regs.reg2) ^ "," ^
  " \"reg3\": " ^ to_hex (get_value m.bin_regs.reg3) ^ "," ^
  " \"ireg\": " ^ to_hex (get_value m.bin_regs.ireg) ^ " " ^
  "}"

let loc_to_string = function
 | SimpleProof -> "{ \"location\": 0, \"list\": [] }"
 | LocationProof (loc,lst) -> "{ \"location\": " ^ string_of_int loc ^ ", \"list\": " ^ list_to_string lst ^ " }"

let proof3_to_string (m, vm, loc) =
  "{ \"vm\": " ^ vm_to_string vm ^ ", \"machine\": " ^ machine_to_string m ^ ", \"merkle\": " ^ loc_to_string loc ^ " }"

let proof2_to_string (m, vm) =
  "{ \"vm\": " ^ vm_to_string vm ^ ", \"machine\": " ^ machine_to_string m ^ " }"

let check_proof proof =
  let state1 = hash_vm_bin (fst proof.fetch_code_proof) in
  let state2 = keccak (hash_vm_bin (fst proof.init_regs_proof)) (microp_word (snd proof.init_regs_proof)) in
  if check_fetch state1 state2 proof.fetch_code_proof then trace "Fetch Success"
  else trace "Fetch Failure";
  let state3 = hash_machine_bin (t1 proof.read_register_proof1) in
  if check_init_registers state2 state3 proof.init_regs_proof then trace "Init Success"
  else trace "Init Failure";
  let state4 = hash_machine_bin (t1 proof.read_register_proof2) in
  if check_read1_proof state3 state4 proof.read_register_proof1 then trace "Read R1 Success"
  else trace "Read R1 Failure";
  let state5 = hash_machine_bin (t1 proof.read_register_proof3) in
  if check_read2_proof state4 state5 proof.read_register_proof2 then trace "Read R2 Success"
  else trace "Read R2 Failure";
  let state6 = hash_machine_bin proof.alu_proof in
  if check_read3_proof state5 state6 proof.read_register_proof3 then trace "Read R3 Success"
  else trace "Read R3 Failure";
  let state7 = hash_machine_bin (t1 proof.write_proof1) in
  if check_alu_proof state6 state7 proof.alu_proof then trace "ALU Success"
  else trace "ALU Failure";
  let state8 = hash_machine_bin (t1 proof.write_proof2) in
  if check_write1_proof state7 state8 proof.write_proof1 then trace "Write 1 Success"
  else trace "Write 1 Failure";
  let state9 = hash_machine_bin (fst proof.update_ptr_proof1) in
  if check_write2_proof state8 state9 proof.write_proof2 then trace "Write 2 Success"
  else trace "Write 2 Failure";
  let state10 = hash_machine_bin (fst proof.update_ptr_proof2) in
  if check_update_pc state9 state10 proof.update_ptr_proof1 then trace "PC Success"
  else trace "PC Failure";
  let state11 = hash_machine_bin (fst proof.update_ptr_proof3) in
  if check_update_break_ptr state10 state11 proof.update_ptr_proof2 then trace "Break Ptr Success"
  else trace "Break Ptr Failure";
  let state12 = hash_machine_bin (fst proof.update_ptr_proof4) in
  if check_update_stack_ptr state11 state12 proof.update_ptr_proof3 then trace "Stack Ptr Success"
  else trace "Stack Ptr Failure";
  let state13 = hash_machine_bin (fst proof.memsize_proof) in
  if check_update_call_ptr state12 state13 proof.update_ptr_proof4 then trace "Call Ptr Success"
  else trace "Call Ptr Failure";
  let state14 = hash_vm_bin proof.finalize_proof in
  if check_update_memsize state13 state14 proof.memsize_proof then trace "Memsize Success"
  else trace "Memsize Failure";
  let states = [state1;state2;state3;state4;state5;state6;state7;state8;state9;state10;state11;state12;state13;state14] in
  Printf.printf "{\n";
  Printf.printf "  \"states\": [%s],\n" (String.concat ", " (List.map to_hex states));
  Printf.printf "  \"fetch\": { \"vm\": %s, \"location\": %s },\n"
    (vm_to_string (fst proof.fetch_code_proof)) (list_to_string (snd proof.fetch_code_proof));
  Printf.printf "  \"init\": { \"vm\": %s, \"op\": %s },\n"
    (vm_to_string (fst proof.init_regs_proof)) (to_hex (microp_word (snd proof.init_regs_proof)));
  Printf.printf "  \"reg1\": %s,\n" (proof3_to_string proof.read_register_proof1);
  Printf.printf "  \"reg2\": %s,\n" (proof3_to_string proof.read_register_proof2);
  Printf.printf "  \"reg3\": %s,\n" (proof3_to_string proof.read_register_proof3);
  Printf.printf "  \"alu\": %s,\n" (machine_to_string proof.alu_proof);
  Printf.printf "  \"write1\": %s,\n" (proof3_to_string proof.write_proof1);
  Printf.printf "  \"write2\": %s,\n" (proof3_to_string proof.write_proof2);
  Printf.printf "  \"pc\": %s,\n" (proof2_to_string proof.update_ptr_proof1);
  Printf.printf "  \"break_ptr\": %s,\n" (proof2_to_string proof.update_ptr_proof2);
  Printf.printf "  \"stack_ptr\": %s,\n" (proof2_to_string proof.update_ptr_proof3);
  Printf.printf "  \"call_ptr\": %s,\n" (proof2_to_string proof.update_ptr_proof4);
  Printf.printf "  \"memsize\": %s,\n" (proof2_to_string proof.memsize_proof);
  Printf.printf "  \"final\": %s\n" (vm_to_string proof.finalize_proof);
  Printf.printf "}\n";
  ()


