
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
 | PUSHBRK of int
 | PUSHBRKRETURN of int
(*
 | L_JUMP of int
 | L_JUMPI of int
 | L_CALL of int
 | L_LABEL of int
 | L_PUSHBRK of int
*)
 | POPBRK
 | BREAK of int
 | RETURN
 | LOAD of loadop
 | STORE of storeop
 | DROP
 | DUP of int
 | SWAP of int
 | LOADGLOBAL of int
 | STOREGLOBAL of int
 | CURMEM
 | GROW
 | CALLI (* indirect call, check from table *)
 | CHECKCALLI of Int64.t (* indirect call, check from table *)
 | PUSH of value                  (* constant *)
 | TEST of testop                    (* numeric test *)
 | CMP of relop                  (* numeric comparison *)
 | UNA of unop                     (* unary numeric operator *)
 | BIN of binop                   (* binary numeric operator *)
 | CONV of cvtop

type context = {
  ptr : int;
  bptr : int;
  label : int;
  f_types : (Int32.t, func_type) Hashtbl.t;
  f_types2 : (Int32.t, func_type) Hashtbl.t;
  block_return : (int * int) list;
}

(* Push the break points to stack? they can have own stack, also returns will have the same *)

let rec make a n = if n = 0 then [] else a :: make a (n-1) 

let rec adjust_stack_aux diff num =
  if num = 0 then [] else
  begin
    [DUP num; SWAP (diff + num + 1); DROP] @ adjust_stack_aux diff (num-1)
  end

let adjust_stack diff num =
  if diff = 0 then [] else
  if diff < 0 then ( trace "Cannot adjust" ; [] ) else
  ( trace ("Adjusting stack: " ^ string_of_int num  ^ " return values, " ^ string_of_int diff ^ " extra values");
    adjust_stack_aux diff num @ make DROP diff )

let rec compile ctx expr = compile' ctx expr.it
and compile' ctx = function
 | Unreachable ->
   ctx, [UNREACHABLE]
 | Nop ->
   ctx, [NOP]
 | Block (ty, lst) ->
   let rets = List.length ty in
   trace ("block start " ^ string_of_int ctx.ptr);
   let end_label = ctx.label in
   let old_return = ctx.block_return in
   let old_ptr = ctx.ptr in
   let ctx = {ctx with label=ctx.label+1; bptr=ctx.bptr+1; block_return=(old_ptr+rets, rets)::ctx.block_return} in
   let ctx, body = compile_block ctx lst in
   trace ("block end " ^ string_of_int ctx.ptr);
   let add_brk = if rets = 0 then [PUSHBRK end_label] else [PUSH (i rets); PUSHBRKRETURN end_label] in
   {ctx with bptr=ctx.bptr-1; block_return=old_return; ptr=old_ptr+rets}, add_brk @ body @ [POPBRK; LABEL end_label]
 | Const lit -> {ctx with ptr = ctx.ptr+1}, [PUSH lit.it]
 | Test t -> ctx, [TEST t]
 | Compare i ->
   trace "cmp";
   {ctx with ptr = ctx.ptr-1}, [CMP i]
 | Unary i -> ctx, [UNA i]
 | Binary i -> 
   trace "bin";
   {ctx with ptr = ctx.ptr-1}, [BIN i]
 | Convert i -> ctx, [CONV i]
 | Loop (_, lst) ->
   let start_label = ctx.label in
   let end_label = ctx.label+1 in
   let sptr = ctx.ptr in
   let old_return = ctx.block_return in
   trace ("loop start " ^ string_of_int sptr);
   let ctx = {ctx with label=ctx.label+2; bptr=ctx.bptr+1; block_return=(ctx.ptr, 0)::old_return} in
   let ctx, body = compile_block ctx lst in
   trace ("loop end " ^ string_of_int ctx.ptr);
   {ctx with bptr=ctx.bptr-1; block_return=old_return}, [LABEL start_label; PUSHBRK start_label] @ body @ [POPBRK; LABEL end_label]
 | If (ty, texp, fexp) ->
   trace ("if " ^ string_of_int ctx.ptr);
   let if_label = ctx.label in
   let end_label = ctx.label+1 in
   let a_ptr = ctx.ptr-1 in
   let ctx = {ctx with ptr=a_ptr; label=ctx.label+2} in
   (*
   let ctx, tbody = compile_block ctx texp in
   let ctx, fbody = compile_block {ctx with ptr=a_ptr} fexp in
   *)
   let ctx, tbody = compile' ctx (Block (ty, texp)) in
   let ctx, fbody = compile' {ctx with ptr=a_ptr} (Block (ty, fexp)) in
   ctx, [JUMPI if_label] @ fbody @ [JUMP end_label; LABEL if_label] @ tbody @ [LABEL end_label]
 | Br x ->
   let num = Int32.to_int x.it in
   let ptr, rets = List.nth ctx.block_return num in
   trace ("br: " ^ string_of_int rets ^ " return values, " ^ string_of_int ptr ^ " return pointer, " ^ string_of_int ctx.ptr ^ " current pointer");
   let adjust = adjust_stack (ctx.ptr - ptr) rets in
   {ctx with ptr=ctx.ptr - rets}, adjust @ [BREAK num]
 | BrIf x ->
   trace ("brif " ^ Int32.to_string x.it);
   let num = Int32.to_int x.it in
   let ptr, rets = List.nth ctx.block_return num in
   let adjust = adjust_stack (ctx.ptr - ptr - 1) rets in
   let continue_label = ctx.label in
   let end_label = ctx.label+1 in
   {ctx with label=ctx.label+2; ptr = ctx.ptr-1},
   [JUMPI continue_label; JUMP end_label; LABEL continue_label] @ adjust @ [BREAK num; LABEL end_label]
 | BrTable (tab, def) ->
   let num = Int32.to_int def.it in
   let _, rets = List.nth ctx.block_return num in
   (* Every level might need different adjustment, so generate the pieces here... at least the number returned values should be same *)
   let make_piece i idx =
     let num = Int32.to_int idx.it in
     let ptr, _ = List.nth ctx.block_return num in
     let adjust = adjust_stack (ctx.ptr - ptr - 1) rets in
     [LABEL (ctx.label+i)] @ adjust @ [BREAK num] in
   let jumps = List.mapi (fun i _ -> JUMP (ctx.label+i)) (tab@[def]) in
   let pieces = List.mapi make_piece (tab@[def]) in
   {ctx with ptr = ctx.ptr-1-rets; label=ctx.label + List.length tab + 2},
   [JUMPFORWARD (List.length tab)] @ jumps @ List.flatten pieces
 | Return ->
   let num = ctx.bptr-1 in
   let ptr, rets = List.nth ctx.block_return num in
   trace ("return: " ^ string_of_int rets ^ " return values, " ^ string_of_int ptr ^ " return pointer, " ^ string_of_int ctx.ptr ^ " current pointer");
   let adjust = adjust_stack (ctx.ptr - ptr) rets in
   {ctx with ptr=ctx.ptr - rets}, adjust @ [BREAK num]
 | Drop ->
    trace "drop";
    {ctx with ptr=ctx.ptr-1}, [DROP]
 | GrowMemory -> {ctx with ptr=ctx.ptr-1}, [GROW]
 | CurrentMemory -> {ctx with ptr=ctx.ptr+1}, [CURMEM]
 | GetGlobal x -> {ctx with ptr=ctx.ptr+1}, [LOADGLOBAL (Int32.to_int x.it)]
 | SetGlobal x ->
   trace "set global";
   {ctx with ptr=ctx.ptr-1}, [STOREGLOBAL (Int32.to_int x.it)]
 | Call v ->
   (* Will just push the pc *)
   let FuncType (par,ret) = Hashtbl.find ctx.f_types v.it in
   {ctx with ptr=ctx.ptr+List.length ret-List.length par}, [CALL (Int32.to_int v.it)]
 | CallIndirect v ->
   let FuncType (par,ret) = Hashtbl.find ctx.f_types2 v.it in
   trace ("call indirect type: " ^ Int64.to_string (Byteutil.ftype_hash (FuncType (par,ret))));
   {ctx with ptr=ctx.ptr+List.length ret-List.length par-1}, [CHECKCALLI (Byteutil.ftype_hash (FuncType (par,ret))); CALLI]
 | Select ->
   trace "select";
   let else_label = ctx.label in
   let end_label = ctx.label+1 in
   let ctx = {ctx with ptr=ctx.ptr-2; label=ctx.label+2} in
   ctx, [JUMPI else_label; SWAP 2; DROP; JUMP end_label; LABEL else_label; DROP; LABEL end_label]
 (* Dup ptr will give local 0 *)
 | GetLocal v ->
   trace ("get local " ^ string_of_int (Int32.to_int v.it) ^ " from " ^  string_of_int (ctx.ptr - Int32.to_int v.it));
   {ctx with ptr=ctx.ptr+1}, [DUP (ctx.ptr - Int32.to_int v.it)]
 | SetLocal v ->
   trace "set local";
   {ctx with ptr=ctx.ptr-1}, [SWAP (ctx.ptr - Int32.to_int v.it); DROP]
 | TeeLocal v ->
   ctx, [SWAP (ctx.ptr - Int32.to_int v.it)]
 | Load op -> ctx, [LOAD op]
 | Store op ->
   trace "store";
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
  trace ("---- function end " ^ string_of_int ctx.ptr);
  ctx,
(*  make (PUSH (I32 Int32.zero)) (List.length func.it.locals) @ *)
  List.map (fun x -> PUSH (default_value x)) func.it.locals @
  body @
  List.flatten (List.mapi (fun i _ -> [DUP (List.length ret - i); SWAP (ctx.ptr-i+1); DROP]) ret) @
  make DROP (List.length par + List.length func.it.locals) @
  [RETURN]

(* This resolves only one function, think more *)
let resolve_inst tab = function
 | LABEL _ -> NOP
 | JUMP l ->
   let loc = Hashtbl.find tab l in
   trace ("resolve jump " ^ string_of_int l ^ " -> " ^ string_of_int loc);
   JUMP loc
 | JUMPI l ->
   let loc = Hashtbl.find tab l in
   trace ("resolve jumpi " ^ string_of_int l ^ " -> " ^ string_of_int loc);
   JUMPI loc
(* | CALL l -> CALL (Hashtbl.find tab l) *)
 | PUSHBRK l -> PUSHBRK (Hashtbl.find tab l)
 | PUSHBRKRETURN l -> PUSHBRKRETURN (Hashtbl.find tab l)
 | a -> a

let resolve_to n lst =
  let tab = Hashtbl.create 10 in
  List.iteri (fun i inst -> match inst with LABEL l -> (* trace ("label " ^ string_of_int l); *) Hashtbl.add tab l (i+n)| _ -> ()) lst;
  List.map (resolve_inst tab) lst

let resolve_inst2 tab = function
 | CALL l -> CALL (Hashtbl.find tab l)
 | a -> a

let empty_ctx = {ptr=0; label=0; bptr=0; block_return=[]; f_types2=Hashtbl.create 1; f_types=Hashtbl.create 1}

let compile_module m =
  let ftab = Hashtbl.create 10 in
  let ttab = Hashtbl.create 10 in
  List.iteri (fun i f -> Hashtbl.add ttab (Int32.of_int i) f.it) m.types;
  List.iteri (fun i f ->
    let ty = Hashtbl.find ttab f.it.ftype.it in
    Hashtbl.add ftab (Int32.of_int i) ty) m.funcs;
  let module_codes = List.map (compile_func {empty_ctx with f_types2=ttab; f_types=ftab}) m.funcs in
  let f_resolve = Hashtbl.create 10 in
  let rec build n acc = function
   | [] -> acc
   | (_,md)::tl ->
     let sz = List.length acc in
     Hashtbl.add f_resolve n sz;
     build (n+1) (acc@resolve_to sz md) tl in
  let flat_code = build 0 [] module_codes in
  List.map (resolve_inst2 f_resolve) flat_code

let make_tables m =
  let ftab = Hashtbl.create 10 in
  let ttab = Hashtbl.create 10 in
  List.iteri (fun i f -> Hashtbl.add ttab (Int32.of_int i) f.it) m.types;
  List.iteri (fun i f ->
    let ty = Hashtbl.find ttab f.it.ftype.it in
    Hashtbl.add ftab (Int32.of_int i) ty) m.funcs;
  ftab, ttab

let compile_test m func vs =
  trace ("Function types: " ^ string_of_int (List.length m.types));
  trace ("Functions: " ^ string_of_int (List.length m.funcs));
  trace ("Tables: " ^ string_of_int (List.length m.tables));
  trace ("Data: " ^ string_of_int (List.length m.data));
  trace ("Elem: " ^ string_of_int (List.length m.elems));
  let ftab = Hashtbl.create 10 in
  let ttab = Hashtbl.create 10 in
  List.iteri (fun i f -> Hashtbl.add ttab (Int32.of_int i) f.it) m.types;
  let entry = ref 0 in
  List.iteri (fun i f ->
    if f = func then ( (* prerr_endline "found it" ; *) entry := i );
    let ty = Hashtbl.find ttab f.it.ftype.it in
    Hashtbl.add ftab (Int32.of_int i) ty) m.funcs;
  let module_codes = List.map (fun f ->
     if f = func then trace "*************** CURRENT ";
     compile_func {empty_ctx with f_types2=ttab; f_types=ftab} f) m.funcs in
  let f_resolve = Hashtbl.create 10 in
  let rec build n acc = function
   | [] -> acc
   | (_,md)::tl ->
     let sz = List.length acc in
     Hashtbl.add f_resolve n sz;
     build (n+1) (acc@resolve_to sz md) tl in
  let test_code = List.map (fun v -> PUSH v) vs @ [CALL !entry; EXIT] in
  let flat_code = build 0 test_code module_codes in
  List.map (resolve_inst2 f_resolve) flat_code, f_resolve

(* perhaps for now just make a mega module *)
let compile_modules lst =
  let mega = {empty_module with
     types=List.flatten (List.map (fun m -> m.types) lst);
     funcs=List.flatten (List.map (fun m -> m.funcs) lst);
  } in
  compile_module mega


