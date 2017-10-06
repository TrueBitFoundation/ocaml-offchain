
(* Use remapping from merge *)
open Merge
open Ast
open Source

(* remap function calls *)
let rec remap_func' map gmap gmap2 ftmap = function
 | Block (ty, lst) -> Block (ty, List.map (remap_func map gmap gmap2 ftmap) lst) 
 | Loop (ty, lst) -> Loop (ty, List.map (remap_func map gmap gmap2 ftmap) lst)
 | If (ty, texp, fexp) -> If (ty, List.map (remap_func map gmap gmap2 ftmap) texp, List.map (remap_func map gmap gmap2 ftmap) fexp)
 | GetGlobal v ->
   ( try gmap2 v.it with Not_found -> GetGlobal {v with it = gmap v.it} )
 | SetGlobal v -> SetGlobal {v with it = gmap v.it}
 | Call v -> Call {v with it = map v.it}
 | CallIndirect v -> CallIndirect {v with it = ftmap v.it}
 | a -> a

and remap_func map gmap gmap2 ftmap i = {i with it = remap_func' map gmap gmap2 ftmap i.it}

let rec remap' map gmap gmap2 ftmap f = {f with ftype={(f.ftype) with it = ftmap f.ftype.it}; body=List.map (remap_func map gmap gmap2 ftmap) f.body}
and remap map gmap gmap2 ftmap i = {i with it = remap' map gmap gmap2 ftmap i.it}

let remap_global map gmap gmap2 ftmap x =
  let res = {x with it={x.it with value = {x.it.value with it=List.map (remap_func map gmap gmap2 ftmap) x.it.value.it}}} in
  res

let do_it f x = {x with it=f x.it}

let remap_elem_segments map gmap gmap2 ftmap el = do_it (fun (x:'a segment') -> {x with offset=do_it (List.map (remap_func map gmap gmap2 ftmap)) x.offset}) el

let conv_to_int x =
  if Char.code x.[0] = 34 then int_of_string (String.sub x 1 (String.length x - 2))
  else int_of_string x

let rec pairify = function
 | a::b::tl -> (a,b) :: pairify tl
 | _ -> []

(* First load the json file *)
let load_file fn =
  let open Yojson.Basic in
  let data = from_channel (open_in fn) in
  let lst = Util.to_assoc (Util.member "env" data) in
  let globals = List.map (fun (a,b) -> (a, int_of_string (to_string b))) lst in
  let mem = List.map (fun x -> conv_to_int (to_string x)) (Util.to_list (Util.member "mem" data)) in
  globals, pairify mem

let add_import taken special imports map map2 num imp =
    (* check if import was already taken *)
    let name = "_" ^ Utf8.encode imp.it.module_name ^ "_" ^ Utf8.encode imp.it.item_name in
    if not (Hashtbl.mem taken name) then begin
      let loc = Int32.of_int (List.length !imports) in
      Hashtbl.add map (Int32.of_int num) loc;
      imports := imp :: !imports;
      Run.trace ("Got import " ^ name);
      Hashtbl.add taken name loc
    end else begin
      let loc = Hashtbl.find taken name in
      Run.trace ("Dropping import " ^ name);
      Hashtbl.add map (Int32.of_int num) loc
    end;
    if Hashtbl.mem special name then begin
      Hashtbl.add map2 (Int32.of_int num) (Hashtbl.find special name)
    end

let elem x = {it=x; at=no_region}

let int_global i = GetGlobal {it=Int32.of_int i; at=no_region}

let int_const y = Const (elem (Values.I32 (Int32.of_int y)))

let int_binary i =
  let res = Bytes.create 4 in
  Bytes.set res 0 (Char.chr (i land 0xff));
  Bytes.set res 1 (Char.chr ((i lsr 8) land 0xff));
  Bytes.set res 2 (Char.chr ((i lsr 16) land 0xff));
  Bytes.set res 3 (Char.chr ((i lsr 24) land 0xff));
  res

let generate_data (addr, i) : string segment =
  elem {
    offset=elem [elem (int_const (addr*4))];
    index=elem 0l;
    init=int_binary i;
  }

let add_globals m fn =
  let g_imports = ref [] in
  let gmap1 = Hashtbl.create 10 in
  let gmap2 = Hashtbl.create 10 in
  let ftmap1 x = x in
  (* remove imports that were defined in the file *)
  let globals, mem = load_file fn in
  let taken_globals = Hashtbl.create 10 in
  let special_globals = Hashtbl.create 10 in
  
  let reserve_export i (x,y) =
    let name = "_env_" ^ x in
    let inst = Const (elem (Values.I32 (Int32.of_int y))) in
    Hashtbl.add special_globals name inst;
    Run.trace ("Blah " ^ name ^ " fddd " ^ string_of_int (555+i));
    Hashtbl.add taken_globals name (Int32.add 555l (Int32.of_int i)) in
  List.iteri reserve_export globals;
  List.iteri (fun n x -> add_import taken_globals special_globals g_imports gmap1 gmap2 n x) (global_imports m);
  (* add the usual globals to gmap1 *)

  let num_ga = List.length (global_imports m) in
  
  let num_g = List.length !g_imports in
  let offset_ga = num_g - num_ga in
  
  List.iteri (fun i _ ->
    Run.trace ("global " ^ string_of_int (i+num_ga) ^ " -> " ^ string_of_int (i + num_ga + offset_ga));
    Hashtbl.add gmap1 (Int32.of_int (i + num_ga)) (Int32.of_int (i + num_ga + offset_ga))) m.it.globals;

  List.iter (fun (x,y) -> Run.trace ("Global " ^ x ^ " = " ^ string_of_int y)) globals;
  (* initialize these globals differently *)
  (* when initializing globals, cannot access previous globals *)
  (* remap exports *)
  let exports_a = List.map (remap_export (fun x -> x) (Hashtbl.find gmap1) ftmap1 "") m.it.exports in
  (* funcs will have to be remapped *)
  let funcs_a = List.map (remap (fun x -> x) (Hashtbl.find gmap1) (Hashtbl.find gmap2) ftmap1) m.it.funcs in
  (* table elements have to be remapped *)
  Run.trace ("Remapping globals");
  let new_data = List.map generate_data mem in
  {m with it={(m.it) with funcs = funcs_a; data=m.it.data@new_data;
     globals = List.map (remap_global (fun x -> x) (Hashtbl.find gmap1) (Hashtbl.find gmap2) ftmap1) m.it.globals;
     imports = List.rev !g_imports @ func_imports m @ other_imports m;
     exports = exports_a;
     elems = List.map (remap_elem_segments (fun x -> x) (Hashtbl.find gmap1) (Hashtbl.find gmap2) ftmap1) m.it.elems}}

