
open Ast
open Source
open Types
open Values

let do_it x f = {x with it=f x.it}

let it e = {it=e; at=no_region}

(*
type ctx = {
  stepper : Int32.t; (* number of steps taken *)
  pointer : Int32.t; (* stack pointer *)
  target : Int32.t; (* target step *)
}*)

type ctx = {
  enter_loop : var;
  push_func : var;
  pop : var;
  loc : Int32.t;
  f_loops : Int32.t list;
}

(* perhaps should get everything as args, just be a C function: add them to env *)

let rec inner_loops inst =
  let loc = Int32.of_int inst.at.left.column in
  match inst.it with
  | Block (ty, lst) -> List.flatten (List.map inner_loops lst)
  | Loop (ty, lst) -> loc :: List.flatten (List.map inner_loops lst)
  | If (ty, l1, l2) -> List.flatten (List.map inner_loops l1) @ List.flatten (List.map inner_loops l2)
  | a -> []

(* for each block, find all loops, after block check if that loop exited *)

let rec process_inst ctx inst =
  (*
  let loc = Int32.of_int inst.at.left.column in
  let loop_locs = List.rev (inner_loops inst) in
  let loops = List.flatten (List.map (fun loc -> List.map it [Const (it (I32 loc)); Call ctx.pop_loop]) loop_locs) in
  if loop_locs <> [] then prerr_endline ("At location " ^ Int32.to_string loc ^ " loops " ^ String.concat ", " (List.map Int32.to_string loop_locs));
  let mk_block inst =
     if loop_locs = [] then [it inst] 
     else List.map it [Const (it (I32 loc)); Call ctx.start_block; inst; Const (it (I32 loc)); Call ctx.end_block] in
  *)
  let res = match inst.it with
  | Block (ty, lst) -> [Block (ty, List.flatten (List.map (process_inst ctx) lst))]
  | If (ty, l1, l2) -> [If (ty, List.flatten (List.map (process_inst ctx) l1), List.flatten (List.map (process_inst ctx) l2))]
  | Loop (ty, lst) -> [Loop (ty, List.map it [Call ctx.enter_loop] @ List.flatten (List.map (process_inst ctx) lst))]
  | Call x -> [Call x; Const (it (I32 ctx.loc)); Call ctx.pop]
  | CallIndirect x -> [CallIndirect x; Const (it (I32 ctx.loc)); Call ctx.pop]
  | a -> [a] in
  List.map it res

let process_function ctx f =
  let loc = Int32.of_int f.at.left.column in
  let ctx = {ctx with loc=loc} in
  do_it f (fun f ->
    {f with body=List.map it [Const (it (I32 loc)); Call ctx.push_func] @ List.flatten (List.map (process_inst ctx) f.body)})

let process m =
  do_it m (fun m ->
    (* add function types *)
    let i_num = List.length (Merkle.func_imports (it m)) in
    let ftypes = m.types @ [
       it (FuncType ([], [I32Type]));
       it (FuncType ([I32Type], []));
       it (FuncType ([], []));
       it (FuncType ([I32Type; I32Type], []));
       it (FuncType ([I32Type; I32Type], [I32Type]));
       ] in
    let ftypes_len = List.length m.types in
(*    let get_type = it (Int32.of_int ftypes_len) in *)
    let set_type = it (Int32.of_int (ftypes_len+1)) in
    let pop_type = it (Int32.of_int (ftypes_len+2)) in
(*    let push_call_type = it (Int32.of_int (ftypes_len+3)) in
    let push_indirect_type = it (Int32.of_int (ftypes_len+4)) in *)
    (* add imports *)
    let added = [
       it {module_name=Utf8.decode "env"; item_name=Utf8.decode "popFuncCritical"; idesc=it (FuncImport set_type)};
       it {module_name=Utf8.decode "env"; item_name=Utf8.decode "enterLoopCritical"; idesc=it (FuncImport pop_type)};
       it {module_name=Utf8.decode "env"; item_name=Utf8.decode "pushFuncCritical"; idesc=it (FuncImport set_type)};
    ] in
    let imps = m.imports @ added in
    let ctx = {
      pop = it (Int32.of_int (i_num+0));
      enter_loop = it (Int32.of_int (i_num+1));
      push_func = it (Int32.of_int (i_num+2));
      f_loops = [];
      loc = 0l;
    } in
    (* remap calls *)
    let remap x = let x = Int32.to_int x in if x >= i_num then Int32.of_int (x + List.length added) else Int32.of_int x in
    let funcs = List.map (Merge.remap remap (fun x -> x) (fun x -> x)) m.funcs in
    {m with funcs=List.map (process_function ctx) funcs;
            types=ftypes;
            imports=imps;
            exports=List.map (Merge.remap_export remap (fun x -> x) (fun x -> x) "") m.exports;
            elems=List.map (Merge.remap_elements remap) m.elems; }
(*
    {m with funcs=funcs; types=ftypes; imports=imps; elems=List.map (Merge.remap_elements remap) m.elems; exports = List.map (Merge.remap_export remap (fun x -> x) (fun x -> x) "") m.exports}
    {m with types=ftypes } 
    m
    *)
    )

