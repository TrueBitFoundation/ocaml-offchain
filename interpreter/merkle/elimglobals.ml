
open Ast
open Source
(*
open Types
open Values
*)

let do_it x f = {x with it=f x.it}

let it e = {it=e; at=no_region}

(* there should be somekind of type map *)
let rec remap_inst ctx inst = do_it inst (function
 | Block (ty, lst) -> Block (ty, List.map (remap_inst ctx) lst) 
 | Loop (ty, lst) -> Loop (ty, List.map (remap_inst ctx) lst)
 | If (ty, texp, fexp) -> If (ty, List.map (remap_inst ctx) texp, List.map (remap_inst ctx) fexp)
 | GetGlobal v -> GetGlobal v
 | SetGlobal v -> SetGlobal v
 | a -> a)

let remap ctx f = do_it f (fun f -> {f with body=List.map (remap_inst ctx) f.body})

let process m' =
   do_it m' (fun m ->
     let imports = Import.link m' in
     let instance = Eval.init m' imports in
     let ctx = () in
     {m with funcs=List.map (remap ctx) m.funcs}
   )

