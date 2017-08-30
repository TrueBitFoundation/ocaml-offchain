
open Mrun
open Mbinary

(* so now we need the different proofs *)

type location_proof =
 | SimpleProof
 | LocationProof of (int * w256 list)

type pointer =
 | PcPtr
 | StackPtr
 | CallPtr
 | BreakPtr

type micro_proof = {
  fetch_code_proof : vm_bin * microp * w256 list;
  init_regs_proof : vm_bin * microp;
  read_register_proof1 : machine_bin * vm_bin * location_proof;
  read_register_proof2 : machine_bin * vm_bin * location_proof;
  read_register_proof3 : machine_bin * vm_bin * location_proof;
  alu_proof : machine_bin;
  write_register_proof1 : machine_bin * vm_bin * location_proof;
  write_register_proof2 : machine_bin * vm_bin * location_proof;
  update_ptr_proof1 : machine_bin * vm_bin;
  update_ptr_proof2 : machine_bin * vm_bin;
  update_ptr_proof3 : machine_bin * vm_bin;
  update_ptr_proof4 : machine_bin * vm_bin;
  update_ptr_proof5 : machine_bin * vm_bin;
  memsize_proof : machine_bin * vm_bin;
  finalize_proof : machine_bin;
}

let make_fetch_code vm =
  let op = get_code vm.code.(vm.pc) in
  (vm_to_bin vm, op, location_proof (Array.map (fun v -> microp_word (get_code v)) vm.code) vm.pc)

let read_position vm reg = function
 | NoIn -> 0
 | Immed -> 0
 | ReadPc -> 0
 | ReadStackPtr -> 0
 | MemsizeIn -> 0
 | GlobalIn -> value_to_int reg.reg1
 | StackIn0 -> vm.stack_ptr-1
 | StackIn1 -> vm.stack_ptr-2
 | StackInReg -> vm.stack_ptr-1-value_to_int reg.reg1
 | StackInReg2 -> vm.stack_ptr-1-value_to_int reg.reg2
 | BreakLocIn -> vm.break_ptr-1
 | BreakStackIn -> vm.break_ptr-1
 | BreakLocInReg -> vm.break_ptr-1-value_to_int reg.reg1
 | BreakStackInReg -> vm.break_ptr-1-value_to_int reg.reg1
 | CallIn -> vm.call_ptr-1
 | MemoryIn -> value_to_int reg.reg1+value_to_int reg.reg2
 | TableIn -> value_to_int reg.reg1

let write_position vm regs = function
 | NoOut -> 0
 | GlobalOut -> value_to_int regs.reg1
 | CallOut -> vm.call_ptr
 | MemoryOut -> value_to_int regs.reg1+value_to_int regs.reg2
 | StackOut0 -> vm.stack_ptr
 | StackOut1 -> vm.stack_ptr-1
 | StackOutReg1 -> vm.stack_ptr-value_to_int regs.reg1
 | BreakLocOut -> vm.break_ptr
 | BreakStackOut -> vm.break_ptr

let loc_proof loc arr = (loc, location_proof arr loc)

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
 | MemoryIn -> LocationProof (loc_proof pos (Array.map get_value vm.memory))
 | TableIn -> LocationProof (loc_proof pos (Array.map u256 vm.calltable))
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
 | MemoryOut -> LocationProof (loc_proof pos (Array.map get_value vm.memory))
 | CallOut -> LocationProof (loc_proof pos (Array.map u256 vm.call_stack))
 | GlobalOut -> LocationProof (loc_proof pos (Array.map get_value vm.globals))

let make_register_proof1 m =
  (machine_to_bin m, vm_to_bin m.m_vm, get_read_location m m.m_microp.read_reg1)

let micro_step_proof vm =
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
  vm.stack_ptr <- handle_ptr regs vm.stack_ptr op.stack_ch ;
  vm.pc <- handle_ptr regs vm.pc op.pc_ch;
  vm.break_ptr <- handle_ptr regs vm.break_ptr op.break_ch;
  vm.stack_ptr <- handle_ptr regs vm.stack_ptr op.stack_ch;
  vm.call_ptr <- handle_ptr regs vm.call_ptr op.call_ch;
  if op.mem_ch then vm.memsize <- vm.memsize + value_to_int regs.reg1

let test () = ()

