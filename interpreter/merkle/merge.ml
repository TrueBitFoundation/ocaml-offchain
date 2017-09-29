
open Ast
open Source

(* remap function calls *)
let rec remap_func' map gmap ftmap = function
 | Block (ty, lst) -> Block (ty, List.map (remap_func map gmap ftmap) lst) 
 | Loop (ty, lst) -> Loop (ty, List.map (remap_func map gmap ftmap) lst)
 | If (ty, texp, fexp) -> If (ty, List.map (remap_func map gmap ftmap) texp, List.map (remap_func map gmap ftmap) fexp)
 | GetGlobal v -> GetGlobal {v with it = gmap v.it}
 | SetGlobal v -> SetGlobal {v with it = gmap v.it}
 | Call v -> Call {v with it = map v.it}
 | CallIndirect v -> CallIndirect {v with it = ftmap v.it}
 | a -> a

and remap_func map gmap ftmap i = {i with it = remap_func' map gmap ftmap i.it}

let rec remap' map gmap ftmap f = {f with ftype={(f.ftype) with it = ftmap f.ftype.it}; body=List.map (remap_func map gmap ftmap) f.body}

and remap map gmap ftmap i = {i with it = remap' map gmap ftmap i.it}

let func_imports m =
  let rec do_get = function
   | [] -> []
   | ({it={idesc={it=FuncImport _;_};_};_} as el)::tl -> el :: do_get tl
   | _::tl -> do_get tl in
  List.rev (do_get m.it.imports)

let global_imports m =
  let rec do_get = function
   | [] -> []
   | ({it={idesc={it=GlobalImport _;_};_};_} as el)::tl -> el :: do_get tl
   | _::tl -> do_get tl in
  List.rev (do_get m.it.imports)

let other_imports m =
  let rec do_get = function
   | [] -> []
   | {it={idesc={it=FuncImport _;_};_};_}::tl -> do_get tl
   | {it={idesc={it=GlobalImport _;_};_};_}::tl -> do_get tl
   | el::tl -> el :: do_get tl in
  List.rev (do_get m.it.imports)

(* probably all funcs will have to stay *)
let merge a b =
  let f_imports = ref [] in
  let g_imports = ref [] in
  let map1 = Hashtbl.create 10 in
  let gmap1 = Hashtbl.create 10 in
  let map2 = Hashtbl.create 10 in
  let gmap2 = Hashtbl.create 10 in
  (* check from exports, if some imports should be linked *)
  let taken_imports = Hashtbl.create 10 in
  let taken_globals = Hashtbl.create 10 in
  let reserve_export x = Hashtbl.add taken_imports (Utf8.encode x.it.name) 0 in
  List.iter reserve_export a.it.exports;
  List.iter reserve_export b.it.exports;
  let add_import taken imports map num imp =
    (* check if import was already taken *)
    let name = "_" ^ Utf8.encode imp.it.module_name ^ "_" ^ Utf8.encode imp.it.item_name in
    if not (Hashtbl.mem taken name) then begin
      Hashtbl.add map (Int32.of_int num) (Int32.of_int (List.length !imports));
      imports := imp :: !imports;
      Hashtbl.add taken name 0
    end in
  (* first just have to calculate total number of imports *)
  List.iteri (fun n x -> add_import taken_imports f_imports map1 n x) (func_imports a);
  List.iteri (fun n x -> add_import taken_imports f_imports map2 n x) (func_imports b);
  List.iteri (fun n x -> add_import taken_globals g_imports gmap1 n x) (global_imports a);
  List.iteri (fun n x -> add_import taken_globals g_imports gmap2 n x) (global_imports b);
  (* remap exports *)
  
  (* funcs will have to be remapped *)
  let funcs_a = List.map (remap (Hashtbl.find map1) (Hashtbl.find gmap1) (fun x -> x)) a.it.funcs in
  let funcs_b = List.map (remap (Hashtbl.find map2) (Hashtbl.find gmap2) (fun x -> Int32.add x (Int32.of_int (List.length a.it.types)))) b.it.funcs in
  let more_imports = other_imports a @ other_imports b in
  {a with it={(a.it) with funcs = funcs_a@funcs_b; globals = a.it.globals@b.it.globals;
     imports = !f_imports @ !g_imports @ more_imports; exports = a.it.exports@b.it.exports; types=a.it.types@b.it.types}}

