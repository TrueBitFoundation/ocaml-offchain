
(* interpreter with merkle proofs *)

open Ast
open Source
open Types
open Values

let trace = Byteutil.trace

(* perhaps we need to link the modules first *)

(* have a separate call stack? *)

(* perhaps the memory will include the stack? nope *)

let value_bool v = not (v = I32 0l)

let value_to_int = function
 | I32 i -> Int32.to_int i
 | I64 i -> Int64.to_int i
 | _ -> 0

let value_to_int64 = function
 | I32 i -> Int64.of_int32 i
 | I64 i -> i
 | _ -> 0L

let i x = I32 (Int32.of_int x)

type inst =
 | EXIT
 | UNREACHABLE
 | NOP
 | JUMP of int
 | JUMPI of int
 | JUMPFORWARD of int (* size of jump table *)
 | CALL of int
 | LABEL of int
 | RETURN
 | LOAD of loadop
 | STORE of storeop
 | DROP of int
 | DROP_N
 | DUP of int
 | SWAP of int              (* TODO: doesn't really swap, just pushes deep into stack. change the name *)
 | LOADGLOBAL of int
 | STOREGLOBAL of int
 | CURMEM
 | GROW                     (* Grow memory *)
 | CALLI                    (* indirect call *)
 | CHECKCALLI of Int64.t    (* check type of indirect call *)
 | PUSH of value                  (* constant *)
 | TEST of testop                    (* numeric test *)
 | CMP of relop                  (* numeric comparison *)
 | UNA of unop                     (* unary numeric operator *)
 | BIN of binop                   (* binary numeric operator *)
 | CONV of cvtop
 | STUB of string
 | INPUTSIZE
 | INPUTNAME
 | INPUTDATA
 | OUTPUTSIZE (* this will create the new file? *)
 | OUTPUTNAME
 | OUTPUTDATA
 | INITCALLTABLE of int
 | INITCALLTYPE of int
 | SETSTACK of int
 | SETCALLSTACK of int
 | SETTABLE of int
 | SETGLOBALS of int
 | SETMEMORY of int

type control = {
  target : int;
  rets : int;
  level : int;
}

type context = {
  ptr : int;
  bptr : int;
  label : int;
  f_types : (Int32.t, func_type) Hashtbl.t;
  f_types2 : (Int32.t, func_type) Hashtbl.t;
  block_return : control list;
}

(* Push the break points to stack? they can have own stack, also returns will have the same *)

let rec make a n = if n = 0 then [] else a :: make a (n-1) 

let rec adjust_stack_aux diff num =
  if num = 0 then [] else
  begin
    [DUP num; SWAP (diff + num + 1); DROP 1] @ adjust_stack_aux diff (num-1)
  end

let adjust_stack diff num =
  if diff = 0 then [] else
  if diff < 0 then ( trace "Cannot adjust" ; [] ) else
  ( (* trace ("Adjusting stack: " ^ string_of_int num  ^ " return values, " ^ string_of_int diff ^ " extra values"); *)
    adjust_stack_aux diff num @ [DROP diff] )

let rec compile ctx expr = compile' ctx expr.it
and compile' ctx = function
 | Unreachable ->
   ctx, [UNREACHABLE]
 | Nop ->
   ctx, [NOP]
 | Block (ty, lst) ->
   let rets = List.length ty in
   (* trace ("block start " ^ string_of_int ctx.ptr); *)
   let end_label = ctx.label in
   let old_return = ctx.block_return in
   let old_ptr = ctx.ptr in
   let ctx = {ctx with label=ctx.label+1; bptr=ctx.bptr+1; block_return={level=old_ptr+rets; rets=rets; target=end_label}::ctx.block_return} in
   let ctx, body = compile_block ctx lst in
   (* trace ("block end " ^ string_of_int ctx.ptr); *)
   {ctx with bptr=ctx.bptr-1; block_return=old_return; ptr=old_ptr+rets}, body @ [LABEL end_label]
 | Const lit -> {ctx with ptr = ctx.ptr+1}, [PUSH lit.it]
 | Test t -> ctx, [TEST t]
 | Compare i ->
   (* trace "cmp"; *)
   {ctx with ptr = ctx.ptr-1}, [CMP i]
 | Unary i -> ctx, [UNA i]
 | Binary i -> 
   (* trace "bin"; *)
   {ctx with ptr = ctx.ptr-1}, [BIN i]
 | Convert i -> ctx, [CONV i]
 | Loop (_, lst) ->
   let start_label = ctx.label in
   let old_return = ctx.block_return in
   (* trace ("loop start " ^ string_of_int ctx.ptr); *)
   let ctx = {ctx with label=ctx.label+1; bptr=ctx.bptr+1; block_return={level=ctx.ptr; rets=0; target=start_label}::old_return} in
   let ctx, body = compile_block ctx lst in
   (* trace ("loop end " ^ string_of_int ctx.ptr); *)
   {ctx with bptr=ctx.bptr-1; block_return=old_return}, [LABEL start_label] @ body
 | If (ty, texp, fexp) ->
   (* trace ("if " ^ string_of_int ctx.ptr); *)
   let if_label = ctx.label in
   let end_label = ctx.label+1 in
   let a_ptr = ctx.ptr-1 in
   let ctx = {ctx with ptr=a_ptr; label=ctx.label+2} in
   let ctx, tbody = compile' ctx (Block (ty, texp)) in
   let ctx, fbody = compile' {ctx with ptr=a_ptr} (Block (ty, fexp)) in
   ctx, [JUMPI if_label] @ fbody @ [JUMP end_label; LABEL if_label] @ tbody @ [LABEL end_label]
 | Br x ->
   let num = Int32.to_int x.it in
   let c = List.nth ctx.block_return num in
   (* trace ("br: " ^ string_of_int c.rets ^ " return values, " ^ string_of_int c.level ^ " return pointer, " ^ string_of_int ctx.ptr ^ " current pointer"); *)
   let adjust = adjust_stack (ctx.ptr - c.level) c.rets in
   {ctx with ptr=ctx.ptr - c.rets}, adjust @ [JUMP c.target]
 | BrIf x ->
   (* trace ("brif " ^ Int32.to_string x.it); *)
   let num = Int32.to_int x.it in
   let c = List.nth ctx.block_return num in
   let adjust = adjust_stack (ctx.ptr - c.level - 1) c.rets in
   let continue_label = ctx.label in
   let end_label = ctx.label+1 in
   {ctx with label=ctx.label+2; ptr = ctx.ptr-1},
   [JUMPI continue_label; JUMP end_label; LABEL continue_label] @ adjust @ [JUMP c.target; LABEL end_label]
 | BrTable (tab, def) ->
   let num = Int32.to_int def.it in
   let { rets; _ } = List.nth ctx.block_return num in
   (* Every level might need different adjustment, so generate the pieces here... at least the number returned values should be same *)
   let make_piece i idx =
     let num = Int32.to_int idx.it in
     let {level=ptr; target; _ } = List.nth ctx.block_return num in
     let adjust = adjust_stack (ctx.ptr - ptr - 1) rets in
     [LABEL (ctx.label+i)] @ adjust @ [JUMP target] in
   let jumps = List.mapi (fun i _ -> JUMP (ctx.label+i)) (tab@[def]) in
   let pieces = List.mapi make_piece (tab@[def]) in
   {ctx with ptr = ctx.ptr-1-rets; label=ctx.label + List.length tab + 2},
   [JUMPFORWARD (List.length tab)] @ jumps @ List.flatten pieces
 | Return ->
   let num = ctx.bptr-1 in
   let {level=ptr; rets; target } = List.nth ctx.block_return num in
   (* trace ("return: " ^ string_of_int rets ^ " return values, " ^ string_of_int ptr ^ " return pointer, " ^ string_of_int ctx.ptr ^ " current pointer"); *)
   let adjust = adjust_stack (ctx.ptr - ptr) rets in
   {ctx with ptr=ctx.ptr - rets}, adjust @ [JUMP target]
 | Drop ->
    (* trace "drop"; *)
    {ctx with ptr=ctx.ptr-1}, [DROP 1]
 | GrowMemory -> {ctx with ptr=ctx.ptr-1}, [GROW]
 | CurrentMemory -> {ctx with ptr=ctx.ptr+1}, [CURMEM]
 | GetGlobal x -> {ctx with ptr=ctx.ptr+1}, [LOADGLOBAL (Int32.to_int x.it)]
 | SetGlobal x ->
   (* trace "set global"; *)
   {ctx with ptr=ctx.ptr-1}, [STOREGLOBAL (Int32.to_int x.it)]
 | Call v ->
   (* Will just push the pc *)
   (* trace ("Function call " ^ Int32.to_string v.it); *)
   let FuncType (par,ret) = Hashtbl.find ctx.f_types v.it in
   {ctx with ptr=ctx.ptr+List.length ret-List.length par}, [CALL (Int32.to_int v.it)]
 | CallIndirect v ->
   let FuncType (par,ret) = Hashtbl.find ctx.f_types2 v.it in
   (* trace ("call indirect type: " ^ Int64.to_string (Byteutil.ftype_hash (FuncType (par,ret)))); *)
   {ctx with ptr=ctx.ptr+List.length ret-List.length par-1}, [CHECKCALLI (Byteutil.ftype_hash (FuncType (par,ret))); CALLI]
 | Select ->
   (* trace "select"; *)
   let else_label = ctx.label in
   let end_label = ctx.label+1 in
   let ctx = {ctx with ptr=ctx.ptr-2; label=ctx.label+2} in
   ctx, [JUMPI else_label; SWAP 2; DROP 1; JUMP end_label; LABEL else_label; DROP 1; LABEL end_label]
 (* Dup ptr will give local 0 *)
 | GetLocal v ->
   (* trace ("get local " ^ string_of_int (Int32.to_int v.it) ^ " from " ^  string_of_int (ctx.ptr - Int32.to_int v.it)); *)
   {ctx with ptr=ctx.ptr+1}, [DUP (ctx.ptr - Int32.to_int v.it)]
 | SetLocal v ->
   (* trace "set local"; *)
   {ctx with ptr=ctx.ptr-1}, [SWAP (ctx.ptr - Int32.to_int v.it); DROP 1]
 | TeeLocal v ->
   ctx, [SWAP (ctx.ptr - Int32.to_int v.it)]
 | Load op -> ctx, [LOAD op]
 | Store op ->
   (* trace "store"; *)
   {ctx with ptr=ctx.ptr-2}, [STORE op]

and compile_block ctx = function
 | [] -> ctx, []
 | a::tl ->
    let ctx, a = compile ctx a in
    let ctx, rest = compile_block ctx tl in
    ctx, a @ rest

(* Initialize local variables with correct types *)

let compile_func ctx func =
  let FuncType (par,ret) = Hashtbl.find ctx.f_types2 func.it.ftype.it in
  trace ("---- function start params:" ^ string_of_int (List.length par) ^ " locals: " ^ string_of_int (List.length func.it.locals) ^ " type: " ^ Int32.to_string func.it.ftype.it);
  trace ("Type hash: " ^ Int64.to_string (Byteutil.ftype_hash (FuncType (par,ret))));
  (* Just params are now in the stack *)
  let ctx, body = compile' {ctx with ptr=ctx.ptr+List.length par+List.length func.it.locals} (Block (ret, func.it.body)) in
(*  trace ("---- function end " ^ string_of_int ctx.ptr); *)
  ctx,
  List.map (fun x -> PUSH (default_value x)) func.it.locals @
  body @
  List.flatten (List.mapi (fun i _ -> [DUP (List.length ret - i); SWAP (ctx.ptr-i+1); DROP 1]) ret) @
  [DROP (List.length par + List.length func.it.locals); RETURN]

(* This resolves only one function *)
let resolve_inst tab = function
 | LABEL _ -> NOP
 | JUMP l ->
   let loc = Hashtbl.find tab l in
(*   trace ("resolve jump " ^ string_of_int l ^ " -> " ^ string_of_int loc); *)
   JUMP loc
 | JUMPI l ->
   let loc = Hashtbl.find tab l in
(*   trace ("resolve jumpi " ^ string_of_int l ^ " -> " ^ string_of_int loc); *)
   JUMPI loc
 | a -> a

let resolve_to n lst =
  let tab = Hashtbl.create 10 in
  List.iteri (fun i inst -> match inst with LABEL l -> (* trace ("label " ^ string_of_int l); *) Hashtbl.add tab l (i+n)| _ -> ()) lst;
  List.map (resolve_inst tab) lst

let resolve_inst2 tab = function
 | CALL l -> CALL (Hashtbl.find tab l)
 | a -> a

let empty_ctx = {ptr=0; label=0; bptr=0; block_return=[]; f_types2=Hashtbl.create 1; f_types=Hashtbl.create 1}

let make_tables m =
  let ftab = Hashtbl.create 10 in
  let ttab = Hashtbl.create 10 in
  List.iteri (fun i f -> Hashtbl.add ttab (Int32.of_int i) f.it) m.types;
  let rec get_imports i = function
   | [] -> []
   | {it=im; _} :: tl ->
     match im.idesc.it with
     | FuncImport tvar ->
        let ty = Hashtbl.find ttab tvar.it in
        Hashtbl.add ftab (Int32.of_int i) ty;
        im :: get_imports (i+1) tl
     | _ -> get_imports i tl in
  let f_imports = get_imports 0 m.imports in
  let num_imports = List.length f_imports in
  List.iteri (fun i f ->
    let ty = Hashtbl.find ttab f.it.ftype.it in
    Hashtbl.add ftab (Int32.of_int (i + num_imports)) ty) m.funcs;
  ftab, ttab

let func_imports m =
  let rec do_get = function
   | [] -> []
   | ({it={idesc={it=FuncImport _;_};_};_} as el)::tl -> el :: do_get tl
   | _::tl -> do_get tl in
  do_get m.it.imports

let global_imports m =
  let rec do_get = function
   | [] -> []
   | ({it={idesc={it=GlobalImport _;_};_};_} as el)::tl -> el :: do_get tl
   | _::tl -> do_get tl in
  do_get m.it.imports

let other_imports m =
  let rec do_get = function
   | [] -> []
   | {it={idesc={it=FuncImport _;_};_};_}::tl -> do_get tl
   | {it={idesc={it=GlobalImport _;_};_};_}::tl -> do_get tl
   | el::tl -> el :: do_get tl in
  do_get m.it.imports

let find_function m func =
  let ftab = Hashtbl.create 10 in
  let ttab = Hashtbl.create 10 in
  List.iteri (fun i f -> Hashtbl.add ttab (Int32.of_int i) f.it) m.types;
  let rec get_imports i = function
   | [] -> []
   | {it=im; _} :: tl ->
     match im.idesc.it with
     | FuncImport tvar ->
        let ty = Hashtbl.find ttab tvar.it in
        Hashtbl.add ftab (Int32.of_int i) ty;
        im :: get_imports (i+1) tl
     | _ -> get_imports i tl in
  let num_imports = List.length (get_imports 0 m.imports) in
  let entry = ref (-1) in
  List.iteri (fun i f ->
    if f = func then ( entry := i + num_imports )) m.funcs;
  !entry

let find_function_index m inst name =
  ( match Instance.export inst name with
  | Some (Instance.ExternalFunc (Instance.AstFunc (_, func))) -> find_function m func
  | _ -> raise Not_found )

let find_global_index m inst name =
  let num_imports = 0l (* Int32.of_int (List.length (global_imports m)) *) in
  let rec get_exports = function
   | [] -> trace ("Cannot Find global: " ^ Utf8.encode name); raise Not_found
   | {it=im; _} :: tl ->
     match im.edesc.it with
     | GlobalExport tvar -> if im.name = name then Int32.add tvar.it num_imports else get_exports tl
     | _ -> get_exports tl in
  Int32.to_int (get_exports m.it.exports)

let malloc_string mdle malloc str =
  let open Memory in
  let open Types in
  let len = String.length str + 1 in
  let res = ref [] in
  for j = 0 to len-2 do
    res := [DUP 1; PUSH (i (Char.code str.[j])); STORE {ty=I32Type; align=0; offset=Int32.of_int (j + !Flags.memory_offset); sz=Some Mem8}] :: !res
  done;
  res := [DUP 1; PUSH (i 0); STORE {ty=I32Type; align=0; offset=Int32.of_int (len-1); sz=Some Mem8}] :: !res;
  (* array address is left *)
  [PUSH (i len); CALL malloc] @ List.flatten (List.rev (!res))

let make_args mdle inst lst =
  let malloc = find_function_index mdle inst (Utf8.decode "_malloc") in
  [PUSH (i (List.length lst)); (* argc *)
   PUSH (i (List.length lst * 4)); CALL malloc] @ (* argv *)
  List.flatten (List.mapi (fun i str -> [DUP 1] @ malloc_string mdle malloc str @ [STORE {ty=I32Type; align=0; offset=Int32.of_int (i*4 + !Flags.memory_offset); sz=None}]) lst)

let init_system mdle inst =
  try [CALL (find_function_index mdle inst (Utf8.decode "_initSystem"))]
  with Not_found -> []

let simple_call mdle inst name =
  try [CALL (find_function_index mdle inst (Utf8.decode name))]
  with Not_found -> []

let make_cxx_init mdle inst = simple_call mdle inst "__GLOBAL__I_000101" @ simple_call mdle inst "__GLOBAL__sub_I_iostream_cpp"

let generic_stub m inst mname fname =
  try
  [STUB (mname ^ " . " ^ fname);
   CALL (find_function_index m inst (Utf8.decode "_callArguments"));
   DROP_N;
   CALL (find_function_index m inst (Utf8.decode "_callMemory"));
   (* Just handle zero or one return values *)
   CALL (find_function_index m inst (Utf8.decode "_callReturns"));
   JUMPI (-2);
   JUMP (-3);
   LABEL (-2);
   CALL (find_function_index m inst (Utf8.decode "_getReturn")); (* here we should do a type adjustment???? *)
   LABEL (-3);
   RETURN]
  with Not_found -> [STUB (mname ^ " . " ^ fname); RETURN]

let vm_init () =
  [ SETSTACK !Flags.stack_size;
    SETMEMORY !Flags.memory_size;
    SETCALLSTACK !Flags.call_size;
    SETGLOBALS !Flags.globals_size;
    SETTABLE !Flags.table_size ]

let elem x = {it=x; at=no_region}

let compile_test m func vs init inst =
  trace ("Function types: " ^ string_of_int (List.length m.types));
  trace ("Functions: " ^ string_of_int (List.length m.funcs));
  trace ("Tables: " ^ string_of_int (List.length m.tables));
  trace ("Data: " ^ string_of_int (List.length m.data));
  trace ("Elem: " ^ string_of_int (List.length m.elems));
  let ftab = Hashtbl.create 10 in
  let ttab = Hashtbl.create 10 in
  List.iteri (fun i f -> Hashtbl.add ttab (Int32.of_int i) f.it) m.types;
  let entry = ref 0 in
  (* handle imports first *)
  let rec get_imports i = function
   | [] -> []
   | {it=im; _} :: tl ->
     match im.idesc.it with
     | FuncImport tvar ->
        let ty = Hashtbl.find ttab tvar.it in
        Hashtbl.add ftab (Int32.of_int i) ty;
        im :: get_imports (i+1) tl
     | _ -> get_imports i tl in
  let f_imports = get_imports 0 m.imports in
  let num_imports = List.length f_imports in
  trace ("Found " ^ string_of_int num_imports ^ " imported functions");
  List.iteri (fun i f ->
    if f = func then ( trace "found invoked function" ; entry := i + num_imports );
    let ty = Hashtbl.find ttab f.it.ftype.it in
    Hashtbl.add ftab (Int32.of_int (i + num_imports)) ty) m.funcs;
  (* perhaps could do something with the function type *)
  (* one idea would be to use a debugging message *)
  let exit_code =
    try [CALL (find_function_index m inst (Utf8.decode "_finalizeSystem")); EXIT]
    with Not_found -> [EXIT] in
  let import_codes = List.map (fun im ->
     let mname = Utf8.encode im.module_name in
     let fname = Utf8.encode im.item_name in
     trace ("importing " ^ fname ^ " from " ^ mname);
     if mname = "env" && fname = "_inputName" then [INPUTNAME;RETURN] else
     if mname = "env" && fname = "_inputSize" then [INPUTSIZE;RETURN] else
     if mname = "env" && fname = "_inputData" then [INPUTDATA;RETURN] else
     if mname = "env" && fname = "_outputName" then [OUTPUTNAME;RETURN] else
     if mname = "env" && fname = "_outputSize" then [OUTPUTSIZE;RETURN] else
     if mname = "env" && fname = "_outputData" then [OUTPUTDATA;RETURN] else
     if mname = "env" && fname = "_sbrk" then
       [STUB "sbrk";
        LOADGLOBAL (find_global_index (elem m) inst (Utf8.decode "DYNAMICTOP_PTR"));
        LOAD {ty=I32Type; align=0; offset=Int32.of_int !Flags.memory_offset; sz=None};
        DUP 1;
        DUP 3;
        BIN (I32 I32Op.Add);
        LOADGLOBAL (find_global_index (elem m) inst (Utf8.decode "DYNAMICTOP_PTR"));
        DUP 2;
        STORE {ty=I32Type; align=0; offset=Int32.of_int !Flags.memory_offset; sz=None};
        DUP 2;
        SWAP 4;
        DROP 3;
        RETURN]
       else
     (* invoke index, a1, a2*)
     if mname = "env" && String.length fname > 7 && String.sub fname 0 7 = "invoke_" then
       let number = String.sub fname 7 (String.length fname - 7) in
       [CALL (find_function_index m inst (Utf8.decode ("dynCall_" ^ number))); RETURN] else
     if mname = "env" && String.length fname > 8 && String.sub fname 0 8 = "_invoke_" then
       let number = String.sub fname 8 (String.length fname - 8) in
       try [CALL (find_function_index m inst (Utf8.decode ("_dynCall_" ^ number))); RETURN]
       with Not_found ->
         prerr_endline ("Warning: cannot find dynamic call number " ^ number);
         [RETURN] else
     if mname = "env" && fname = "abort" then [UNREACHABLE] else
     if mname = "env" && fname = "_exit" then exit_code else
     if mname = "env" && fname = "getTotalMemory" then
        try [LOADGLOBAL (find_global_index (elem m) inst (Utf8.decode "TOTAL_MEMORY")); RETURN]
        with Not_found ->
        ( prerr_endline "Warning, cannot find global variable TOTAL_MEMORY. Use emscripten-module-wrapper to run files that were generated by emscripten";
          [PUSH (i (1024*1024*1500)); RETURN] ) else
     if mname = "env" && fname = "setTempRet0" then [RETURN] else (* hopefully this is enough *)
     if mname = "env" && fname = "_debugString" then [STUB (mname ^ " . " ^ fname); RETURN] else
     if mname = "env" && fname = "_debugBuffer" then [STUB (mname ^ " . " ^ fname); DROP 1; RETURN] else
     if mname = "env" && fname = "_debugInt" then [STUB (mname ^ " . " ^ fname); RETURN] else
     if mname = "env" && fname = "_getSystem" then [LOADGLOBAL (find_global_index (elem m) inst (Utf8.decode "_system_ptr")); RETURN] else
     if mname = "env" && fname = "_setSystem" then [STOREGLOBAL (find_global_index (elem m) inst (Utf8.decode "_system_ptr")); RETURN] else
     generic_stub m inst mname fname
     (*
     if mname = "env" && fname = "_getenv" then [DROP 1; PUSH (i 0); RETURN] else
     if mname = "env" && fname = "_debugRead" then [STUB (mname ^ " . " ^ fname); DROP 1; RETURN] else
     (* opening file *)
     if mname = "env" && fname = "___syscall5" then [STUB (mname ^ " . " ^ fname); DROP 2; PUSH (i (-1)); RETURN] else
     if mname = "env" && fname = "___syscall3" then [STUB (mname ^ " . " ^ fname); DROP 2; PUSH (i 0); RETURN] else
     if mname = "env" && fname = "___syscall6" then [STUB (mname ^ " . " ^ fname); DROP 2; PUSH (i 0); RETURN] else
     if mname = "env" && fname = "___syscall140" then [STUB (mname ^ " . " ^ fname); DROP 2; PUSH (i 0); RETURN] else
     (* writing to file descriptor *)
     if mname = "env" && fname = "___syscall146" then [STUB (mname ^ " . " ^ fname); DROP 2; PUSH (i 1); RETURN] else
     [STUB (mname ^ " . " ^ fname); RETURN] *) ) f_imports in
  let module_codes = List.map (fun f ->
     if f = func then trace "*************** CURRENT ";
     compile_func {empty_ctx with f_types2=ttab; f_types=ftab} f) m.funcs in
  let f_resolve = Hashtbl.create 10 in
  let rec build n acc l_acc = function
   | [] -> acc
   | fcode::tl ->
     Hashtbl.add f_resolve n l_acc;
     trace ("Function " ^ string_of_int n ^ " at " ^ string_of_int l_acc);
     let x = resolve_to l_acc fcode in
     build (n+1) (x::acc) (List.length x + l_acc) tl in
  let test_code = init @ List.map (fun v -> PUSH v) vs @ [CALL !entry] @ exit_code in
  let codes = build 0 [test_code] (List.length test_code) (import_codes @ List.map snd module_codes) in
  let flat_code = List.flatten (List.rev codes) in
  List.rev (List.rev_map (resolve_inst2 f_resolve) flat_code), f_resolve

