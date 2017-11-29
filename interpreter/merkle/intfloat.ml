
open Source
open Ast
open Types
open Values
open Merkle 

(* just simply merge two files *)

let do_it x f = {x with it=f x.it}

let simple_add n i = Int32.add i (Int32.of_int n)

let merge a b =
  let funcs_a = a.it.funcs in
  let num = List.length (Merkle.func_imports a) + List.length funcs_a in
  let num_ft = List.length a.it.types in
  let funcs_b = List.map (Merge.remap (simple_add num) (fun x -> x) (simple_add num_ft)) b.it.funcs in
  {a with it={(a.it) with funcs = funcs_a@funcs_b;
     globals = a.it.globals @ b.it.globals;
     imports = List.rev a.it.imports;
     exports = a.it.exports@List.filter Merge.drop_table (List.map (Merge.remap_export (simple_add num) (fun x -> x) (simple_add num_ft) "") b.it.exports);
     elems = a.it.elems;
     types=a.it.types@b.it.types;
     data=a.it.data@b.it.data}}

let convert_type' = function
 | I32Type -> I32Type
 | F32Type -> I32Type
 | I64Type -> I64Type
 | F64Type -> I64Type

let convert_type t = do_it t convert_type'

let convert_ftype ft =
  do_it ft (function FuncType (l1, l2) -> FuncType (List.map convert_type' l1, List.map convert_type' l2))

let rec convert_inst' = function
 | Block (ty, lst) -> Block (List.map convert_type' ty, List.map convert_inst lst) 
 | Loop (ty, lst) -> Loop (List.map convert_type' ty, List.map convert_inst lst)
 | If (ty, texp, fexp) -> If (List.map convert_type' ty, List.map convert_inst texp, List.map convert_inst fexp)
 | a -> a

and convert_inst x = do_it x (fun x -> convert_inst' x)

let convert_func f =
  do_it f (fun f -> {f with body=List.map convert_inst f.body; locals=List.map convert_type' f.locals})

(* convert all types *)
let convert_types m =
  (* also constants have to be converted *)
  do_it m (fun m ->
    {m with types=List.map convert_ftype m.types;
            funcs=List.map convert_func m.funcs})

(* exported function by name *)
let find_function m (name:string) =
  let rec find = function
   | [] -> raise Not_found
   | a::tl ->
     if Utf8.encode a.it.name = name then
       ( match a.it.edesc.it with
       | FuncExport v -> v.it
       | _ -> find tl )
     else find tl in
  {it=find m.it.exports; at=no_region}

(*

| Test of testop                    (* numeric test *)
| Compare of relop                  (* numeric comparison *)
| Unary of unop                     (* unary numeric operator *)
| Binary of binop                   (* binary numeric operator *)
| Convert of cvtop                  (* conversion *)

*)

(* add ops ... *)


let convert_float m =
  let rec convert_op' = function
   | Block (ty, lst) -> [Block (ty, List.flatten (List.map convert_op lst))]
   | Loop (ty, lst) -> [Loop (ty, List.flatten (List.map convert_op lst))]
   | If (ty, texp, fexp) -> [If (ty, List.flatten (List.map convert_op texp), List.flatten (List.map convert_op fexp))]
   | Binary (F32 F32Op.Add) -> [Call (find_function m "f32_add")]
   | Binary (F32 F32Op.Div) -> [Call (find_function m "f32_div")]
   | Convert (F32 F32Op.ConvertSI32) -> [Call (find_function m "i32_to_f32")]
   | Convert (I32 I32Op.TruncSF32) -> [Const (elem (I32 (Int32.of_int 0))); Const (elem (I32 (Int32.of_int 0))); Call (find_function m "f32_to_i32")]
   | a -> [a]
  and convert_op x = List.map elem (convert_op' x.it) in
  let convert_func f = do_it f (fun f -> {f with body=List.flatten (List.map convert_op f.body)}) in
  (* also constants have to be converted *)
  do_it m (fun m -> {m with funcs=List.map convert_func m.funcs})

let process a b =
  convert_float (convert_types (merge a b))
  




