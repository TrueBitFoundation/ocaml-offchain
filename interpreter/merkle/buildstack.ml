
open Ast
open Source
open Types
open Values

let do_it x f = {x with it=f x.it}

let it e = {it=e; at=no_region}

type ctx = {
  tctx : Valid.context;
  possible : int32 -> bool;
  adjust_stack0 : var;
  adjust_stack_i32 : var;
  adjust_stack_i64 : var;
  adjust_stack_f32 : var;
  adjust_stack_f64 : var;
  start_block : var;
  end_block : var;
  label : int;
  pop : var;
  push : var;
  var_type : int32 -> func_type;
  lookup_type : int32 -> func_type;
}

(* perhaps should get everything as args, just be a C function: add them to env *)

let rec inner_loops inst =
  let loc = Int32.of_int inst.at.left.column in
  match inst.it with
  | Block (ty, lst) -> List.flatten (List.map inner_loops lst)
  | Loop (ty, lst) -> loc :: List.flatten (List.map inner_loops lst)
  | If (ty, l1, l2) -> List.flatten (List.map inner_loops l1) @ List.flatten (List.map inner_loops l2)
  | a -> []

(* for active blocks ... *)

let type_rets = function
 | FuncType (_, lst) -> List.length lst

let type_pops = function
 | FuncType (lst, _) -> List.length lst

let inst_rets ctx = function
  | Block (ty, _) -> List.length ty
  | Loop (ty, _) -> List.length ty
  | If (ty, _, _) -> List.length ty
  | Const _ -> 1
  | Test _ -> 1
  | Compare _ -> 1
  | Unary _ -> 1
  | Binary _ -> 1
  | Convert _ -> 1
  | BrIf _ -> 0
  | BrTable (_, _) -> 0
  | Drop -> 0
  | GrowMemory -> 0
  | CurrentMemory -> 1
  | GetGlobal _ -> 1
  | SetGlobal _ -> 0
  | Call {it=v; _} -> type_rets (ctx.var_type v)
  | CallIndirect {it=v; _} -> type_rets (ctx.lookup_type v)
  | Select -> 1
  | GetLocal _ -> 1
  | SetLocal _ -> 0
  | TeeLocal _ -> 1
  | Load _ -> 1
  | Store _ -> 0
  | _ -> 0

let inst_pops ctx = function
  | Block (ty, _) -> 0
  | Loop (ty, _) -> 0
  | If (ty, _, _) -> 1
  | Const _ -> 0
  | Test _ -> 1
  | Compare _ -> 2
  | Unary _ -> 1
  | Binary _ -> 2
  | Convert _ -> 2
  | BrIf _ -> 1
  | BrTable (_, _) -> 1
  | Drop -> 1
  | GrowMemory -> 1
  | CurrentMemory -> 0
  | GetGlobal _ -> 0
  | SetGlobal _ -> 1
  | Call {it=v; _} -> type_pops (ctx.var_type v)
  | CallIndirect {it=v; _} -> type_pops (ctx.lookup_type v)
  | Select -> 3
  | GetLocal _ -> 0
  | SetLocal _ -> 1
  | TeeLocal _ -> 1
  | Load _ -> 1
  | Store _ -> 2
  | _ -> 0

let determine_type tctx block =
  let _, lst = Valid.type_seq tctx block in
  prerr_endline (string_of_int (List.length lst));
  match List.rev lst with
  | Some x :: _ -> x
  | _ -> raise (Failure "typeing error")

(* after each instruction, modify stack *)
(* perhaps call should be different? nope *)
(* so there should be two alternatives for return... *)
let build_stack ctx (pre, inst) =
  let rets = inst_rets ctx inst.it in
  if rets > 1 then prerr_endline ("number of rets " ^ string_of_int rets);
  let pops = inst_pops ctx inst.it in
  let adjust =
    if rets = 0 then ctx.adjust_stack0 else match determine_type ctx.tctx (pre@[inst]) with
    | I32Type -> ctx.adjust_stack_i32
    | F32Type -> ctx.adjust_stack_f32
    | I64Type -> ctx.adjust_stack_i64
    | F64Type -> ctx.adjust_stack_f64
    in
  inst :: List.map it [Const (it (I32 (Int32.of_int pops))); Call adjust]

let rec remap_blocks label inst =
  let handle {it=v; _} = if Int32.of_int label > v then it v else it (Int32.add v 1l) in
  do_it inst (function
  | Block (ty, lst) -> Block (ty, List.map (remap_blocks (label+1)) lst)
  | If (ty, l1, l2) -> If (ty, List.map (remap_blocks (label+1)) l1, List.map (remap_blocks (label+1)) l2)
  | Loop (ty, lst) -> Loop (ty, List.map (remap_blocks (label+1)) lst)
  | Br v -> Br (handle v)
  | BrIf v -> BrIf (handle v)
  | BrTable (lst, v) -> BrTable (List.map handle lst, handle v)
  | a -> a)

let rec postfix = function
 | [] -> []
 | a::tl -> (tl, a) :: postfix tl

let prefix lst = List.rev (List.map (fun (l, a) -> List.rev l, a) (postfix (List.rev lst)))

let rec process_inst ctx inst =
  let loc = Int32.of_int inst.at.left.column in
  let loop_locs = List.rev (inner_loops inst) in
  let mk_block ty lst =
     if loop_locs = [] then lst
     else if ctx.possible loc then 
        (* here we have to remap all the blocks ... *)
        let lst = List.map (remap_blocks 0) lst in
        List.map it [
          Const (it (I32 loc));
          Call ctx.start_block;
          If (ty, List.flatten (List.map (build_stack ctx) (prefix lst)), lst)]
     else List.map it [
        Const (it (I32 loc));
        Call ctx.start_block;
        Drop] @ lst in
  let e_block = if loop_locs = [] then [] else List.map it [Const (it (I32 loc)); Call ctx.end_block] in
  let res = match inst.it with
  | Block (ty, lst) ->
      List.map it [Block (ty, mk_block ty (List.flatten (List.map (process_inst ctx) lst)))] @ e_block
  | If (ty, l1, l2) ->
      List.map it [If (ty, mk_block ty (List.flatten (List.map (process_inst ctx) l1)), mk_block ty (List.flatten (List.map (process_inst ctx) l2)))] @ e_block
  | Loop (ty, lst) ->
      List.map it [Loop (ty, List.map it [Const (it (I32 loc)); Call ctx.end_block] @ mk_block ty (List.flatten (List.map (process_inst ctx) lst)))] @ e_block
  | a -> List.map it [a] in
  res

let process_function ctx f =
  let loc = Int32.of_int f.at.left.column in
  let ctx = {ctx with tctx=Valid.func_context ctx.tctx f} in
  do_it f (fun f ->
    {f with body=List.map it [Const (it (I32 loc)); Call ctx.push] @ List.flatten (List.map (process_inst ctx) f.body) @ List.map it [Call ctx.pop] })

let path_table fn =
  let res = Hashtbl.create 123 in
  let open Yojson.Basic in
  let data = from_channel (open_in fn) in
  let lst = Util.to_list data in
  List.iter (fun el ->
     let loc = Util.member "loc" el in
     Hashtbl.add res (Int32.of_int (Util.to_int loc)) true;
  ) lst;
  res

let process m =
  do_it m (fun m ->
    (* add function types *)
    let i_num = List.length (Merkle.func_imports (it m)) in
    let ftypes = m.types @ [
      it (FuncType ([I32Type], []));
      it (FuncType ([I32Type], [I32Type]));
      it (FuncType ([I32Type], []));
      it (FuncType ([], []));
      it (FuncType ([I32Type; I32Type], [I32Type]));
      it (FuncType ([I64Type; I32Type], [I64Type]));
      it (FuncType ([F32Type; I32Type], [F32Type]));
      it (FuncType ([F64Type; I32Type], [F64Type]));
      ] in
    let ftypes_len = List.length m.types in
    let adjust_type0 = it (Int32.of_int ftypes_len) in
    let start_type = it (Int32.of_int (ftypes_len+1)) in
    let end_type = it (Int32.of_int (ftypes_len+2)) in
    let pop_type = it (Int32.of_int (ftypes_len+3)) in
    let adjust_type_i32 = it (Int32.of_int (ftypes_len+4)) in
    let adjust_type_i64 = it (Int32.of_int (ftypes_len+5)) in
    let adjust_type_f32 = it (Int32.of_int (ftypes_len+6)) in
    let adjust_type_f64 = it (Int32.of_int (ftypes_len+7)) in
    (* add imports *)
    let added = [
       it {module_name=Utf8.decode "env"; item_name=Utf8.decode "adjustStack0"; idesc=it (FuncImport adjust_type0)};
       it {module_name=Utf8.decode "env"; item_name=Utf8.decode "startCritical"; idesc=it (FuncImport start_type)};
       it {module_name=Utf8.decode "env"; item_name=Utf8.decode "endCritical"; idesc=it (FuncImport end_type)};
       it {module_name=Utf8.decode "env"; item_name=Utf8.decode "popCritical"; idesc=it (FuncImport pop_type)};
       it {module_name=Utf8.decode "env"; item_name=Utf8.decode "pushCritical"; idesc=it (FuncImport end_type)};
       it {module_name=Utf8.decode "env"; item_name=Utf8.decode "adjustStackI32"; idesc=it (FuncImport adjust_type_i32)}; (* for each type, need a different function *)
       it {module_name=Utf8.decode "env"; item_name=Utf8.decode "adjustStackI64"; idesc=it (FuncImport adjust_type_i64)}; (* for each type, need a different function *)
       it {module_name=Utf8.decode "env"; item_name=Utf8.decode "adjustStackF32"; idesc=it (FuncImport adjust_type_f32)}; (* for each type, need a different function *)
       it {module_name=Utf8.decode "env"; item_name=Utf8.decode "adjustStackF64"; idesc=it (FuncImport adjust_type_f64)}; (* for each type, need a different function *)
    ] in
    let imps = m.imports @ added in
    let pos_tab = path_table "critical.out" in
    (* remap calls *)
    let remap x = let x = Int32.to_int x in if x >= i_num then Int32.of_int (x + List.length added) else Int32.of_int x in
    let funcs = List.map (Merge.remap remap (fun x -> x) (fun x -> x)) m.funcs in
    let pre_m = {m with funcs=funcs;
            types=ftypes;
            imports=imps;
            exports=List.map (Merge.remap_export remap (fun x -> x) (fun x -> x) "") m.exports;
            elems=List.map (Merge.remap_elements remap) m.elems; } in
    let ftab, ttab = Merkle.make_tables pre_m in
    let ctx = {
      tctx = Valid.module_context (it pre_m);
      adjust_stack0 = it (Int32.of_int (i_num+0));
      start_block = it (Int32.of_int (i_num+1));
      end_block = it (Int32.of_int (i_num+2));
      pop = it (Int32.of_int (i_num+3));
      push = it (Int32.of_int (i_num+4));
      adjust_stack_i32 = it (Int32.of_int (i_num+5));
      adjust_stack_i64 = it (Int32.of_int (i_num+6));
      adjust_stack_f32 = it (Int32.of_int (i_num+7));
      adjust_stack_f64 = it (Int32.of_int (i_num+8));
      var_type = Hashtbl.find ftab;
      lookup_type = Hashtbl.find ttab;
      possible = (fun loc -> Hashtbl.mem pos_tab loc);
      label = 0;
    } in
    let res = {pre_m with funcs=List.map (process_function ctx) pre_m.funcs} in
    prerr_endline ("here");
    res
    )



