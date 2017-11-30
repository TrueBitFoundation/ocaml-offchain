
open Source
open Ast
open Merkle
open Values

let do_it x f = {x with it=f x.it}

(* offset in load, store and memory segments *)

let rec convert_inst' num = function
 | Block (ty, lst) -> Block (ty, List.map (convert_inst num) lst)
 | Loop (ty, lst) -> Loop (ty, List.map (convert_inst num) lst)
 | If (ty, texp, fexp) -> If (ty, List.map (convert_inst num) texp, List.map (convert_inst num) fexp)
 | Store op -> Store {op with offset=Int32.add num op.offset}
 | Load op -> Load {op with offset=Int32.add num op.offset}
 | a -> a

and convert_inst num x = do_it x (fun x -> convert_inst' num x)

let convert_func num f =
  do_it f (fun f -> {f with body=List.map (convert_inst num) f.body})

let convert_mem num (seg:string segment) = do_it seg (fun seg ->
  let ofs = do_it seg.offset (fun lst -> lst @ [elem (Const (elem (I32 num))); elem (Binary (I32 I32Op.Add))]) in
  {seg with offset=ofs})

(* also constants have to be converted *)
let process m num =
  let num = Int32.of_int num in
  do_it m (fun m -> {m with funcs=List.map (convert_func num) m.funcs; data=List.map (convert_mem num) m.data})

