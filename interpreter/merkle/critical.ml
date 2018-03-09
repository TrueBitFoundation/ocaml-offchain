
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
  push : var; (* number of steps taken *)
  pop : var; (* stack pointer *)
  pop_loop : var; (* target step *)
}

(*
let find_import m name =
  let res = ref 0 in
  let rec check acc = function
   | [] -> raise Not_found
   do_it el (fun el ->
     if Utf8.encode el.item_name then acc
     else check (acc+1)
     ) in
  List.iter check m.imports in
  Int32.of_int !res
*)

(* perhaps should get everything as args, just be a C function: add them to env *)

let rec inner_loops inst =
  let loc = Int32.of_int inst.at.left.column in
  match inst.it with
  | Block (ty, lst) -> List.flatten (List.map inner_loops lst)
  | Loop (ty, lst) -> loc :: List.flatten (List.map inner_loops lst)
  | If (ty, l1, l2) -> List.flatten (List.map inner_loops l1) @ List.flatten (List.map inner_loops l2)
  | a -> []

(* for each block, find all loops, after of block check if that loop exited *)

let rec process_inst ctx inst =
  let loc = Int32.of_int inst.at.left.column in
  let loops = List.flatten (List.map (fun loc -> List.map it [Const (it (I32 loc)); Call ctx.pop_loop]) (inner_loops inst)) in
  let res = match inst.it with
  | Block (ty, lst) -> List.map it [Block (ty, List.flatten (List.map (process_inst ctx) lst))]
  | Loop (ty, lst) ->
    List.map it [Const (it (I32 loc)); Call ctx.pop_loop; Const (it (I32 loc)); Call ctx.push; Loop (ty, List.flatten (List.map (process_inst ctx) lst))]
  | If (ty, l1, l2) -> List.map it [If (ty, List.flatten (List.map (process_inst ctx) l1), List.flatten (List.map (process_inst ctx) l2))]
  | Call x -> List.map it [Const (it (I32 loc)); Call ctx.push; Call x; Call ctx.pop]
  | CallIndirect x -> List.map it [Const (it (I32 loc)); Call ctx.push; CallIndirect x; Call ctx.pop]
  | a -> List.map it [a] in
  res @ loops

let process_function ctx f =
  do_it f (fun f -> {f with body=List.flatten (List.map (process_inst ctx) f.body)})

let process m =
  do_it m (fun m ->
    (* add function types *)
    let i_num = List.length (Merkle.func_imports (it m)) in
    let ftypes = m.types @ [it (FuncType ([], [I32Type])); it (FuncType ([I32Type], [])); it (FuncType ([], []))] in
    let ftypes_len = List.length m.types in
    let set_type = it (Int32.of_int ftypes_len) in
    let get_type = it (Int32.of_int (ftypes_len+1)) in
    let pop_type = it (Int32.of_int (ftypes_len+2)) in
    (* add imports *)
    let imps = m.imports @ [
       it {module_name=Utf8.decode "env"; item_name=Utf8.decode "setStack"; idesc=it (FuncImport set_type)};
       it {module_name=Utf8.decode "env"; item_name=Utf8.decode "setStep"; idesc=it (FuncImport set_type)};
       it {module_name=Utf8.decode "env"; item_name=Utf8.decode "getStack"; idesc=it (FuncImport get_type)};
       it {module_name=Utf8.decode "env"; item_name=Utf8.decode "getStep"; idesc=it (FuncImport get_type)};
       it {module_name=Utf8.decode "env"; item_name=Utf8.decode "getTarget"; idesc=it (FuncImport get_type)};
       it {module_name=Utf8.decode "env"; item_name=Utf8.decode "pushCritical"; idesc=it (FuncImport set_type)};
       it {module_name=Utf8.decode "env"; item_name=Utf8.decode "popCritical"; idesc=it (FuncImport pop_type)};
       it {module_name=Utf8.decode "env"; item_name=Utf8.decode "popLoopCritical"; idesc=it (FuncImport set_type)};
    ] in
    let ctx = {
      push = it (Int32.of_int (i_num+5));
      pop = it (Int32.of_int (i_num+6));
      pop_loop = it (Int32.of_int (i_num+7));
    } in
    (* remap calls *)
    let funcs = List.map (Merge.remap (fun x -> let x = Int32.to_int x in if x >= i_num then Int32.of_int (x + 7) else Int32.of_int x) (fun x -> x) (fun x -> x)) m.funcs in
    {m with funcs=List.map (process_function ctx) funcs; types=ftypes; imports=imps})

