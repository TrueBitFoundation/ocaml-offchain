
open Mrun
open Mbinary

(* so now we need the different proofs *)

type location_proof =
 | NoinProof
 | ImmedProof
 | GlobalProof of int * w256 list
 | StackProof of int * w256 list
 | BreakLocProof of int * w256 list
 | BreakStackProof of int * w256 list
 | CallProof of int * w256 list
 | TableProof of int * w256 list
 | PcProof
 | StackPtrProof
 | MemsizeInProof

type pointer =
 | PcPtr
 | StackPtr
 | CallPtr
 | BreakPtr

type proof =
 | FetchCodeProof
 | InitRegsProof
 | ReadRegisterProof of reg * location_proof
 | AluProof of alu_code
 | WriteRegisterProof of reg * location_proof
 | UpdatePtrProof of pointer * stack_ch
 | MemsizeProof
 | FinalizeProof

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

