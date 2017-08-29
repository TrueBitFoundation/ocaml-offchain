
(* interpreter with merkle proofs *)

open Ast
open Source
open Types
open Values

(* perhaps we need to link the modules first *)

(* have a separate call stack? *)

(* perhaps the memory will include the stack? nope *)

type inst =
 | UNREACHABLE
 | NOP
 | JUMP of int
 | JUMPI of int
 | CALL of int
 | LABEL of int
 | PUSHBRK of int
(*
 | L_JUMP of int
 | L_JUMPI of int
 | L_CALL of int
 | L_LABEL of int
 | L_PUSHBRK of int
*)
 | POPBRK
 | BREAK
 | RETURN
 | LOAD of int
 | STORE of int
 | DROP
 | DUP of int
 | SWAP of int
 | LOADGLOBAL of int
 | STOREGLOBAL of int
 | CURMEM
 | GROW
 | POPI1 of int
 | POPI2 of int
 | BREAKTABLE
 | CALLI of int (* indirect call, check from table *)
 | PUSH of literal                  (* constant *)
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
}

(* Push the break points to stack? they can have own stack, also returns will have the same *)

let rec compile ctx expr = compile' ctx expr.it
and compile' ctx = function
 | Unreachable -> ctx, [UNREACHABLE]
 | Nop -> ctx, [NOP]
 | Block (ty, lst) ->
   let end_label = ctx.label in
   let ctx = {ctx with label=ctx.label+1; bptr=ctx.bptr+1} in
   let ctx, body = compile_block ctx lst in
   {ctx with bptr=ctx.bptr-1}, [PUSHBRK end_label] @ body @ [LABEL end_label; POPBRK]
 | Const lit -> {ctx with ptr = ctx.ptr+1}, [PUSH lit]
 | Test t -> {ctx with ptr = ctx.ptr-1}, [TEST t]
 | Compare i -> {ctx with ptr = ctx.ptr-1}, [CMP i]
 | Unary i -> ctx, [UNA i]
 | Binary i -> {ctx with ptr = ctx.ptr-1}, [BIN i]
 | Convert i -> ctx, [CONV i]
 | Loop (_, lst) ->
   let start_label = ctx.label in
   let end_label = ctx.label+1 in
   let ctx = {ctx with label=ctx.label+2; bptr=ctx.bptr+2} in
   let ctx, body = compile_block ctx lst in
   {ctx with ptr=ctx.bptr-2}, [LABEL start_label; PUSHBRK end_label;  PUSHBRK start_label] @ body @ [JUMP ctx.label; LABEL end_label; POPBRK; POPBRK]
 | If (ty, texp, fexp) ->
   let else_label = ctx.label in
   let end_label = ctx.label+1 in
   let ctx = {ctx with ptr=ctx.ptr-1; label=ctx.label+2} in
   let ctx, tbody = compile' ctx (Block (ty, texp)) in
   let ctx, fbody = compile' ctx (Block (ty, fexp)) in
   ctx, [JUMPI else_label] @ tbody @ [JUMP end_label; LABEL else_label] @ fbody @ [LABEL end_label]
 | Br x -> compile_break ctx (Int32.to_int x.it)
 | BrIf x ->
   let continue_label = ctx.label in
   let ctx = {ctx with label=ctx.label+1; ptr = ctx.ptr-1} in
   let ctx, rest = compile_break ctx (Int32.to_int x.it) in
   ctx, [JUMPI continue_label] @ rest @ [LABEL continue_label]
 | BrTable (tab, def) ->
   (* push the list there, then use a special instruction *)
   let lst = List.map (fun x -> PUSH {at=no_region; it=Values.I32 x.it}) (def::tab) in
   ctx, lst @ [DUP (List.length lst); POPI1 (List.length lst); POPI2 (List.length lst); BREAKTABLE]
 | Return ->  compile_break ctx ctx.bptr
 | Drop -> {ctx with ptr=ctx.ptr-1}, [DROP]
 | GrowMemory -> {ctx with ptr=ctx.ptr-1}, [GROW]
 | CurrentMemory -> {ctx with ptr=ctx.ptr+1}, [CURMEM]
 | GetGlobal x -> {ctx with ptr=ctx.ptr+1}, [LOADGLOBAL (Int32.to_int x.it)]
 | SetGlobal x -> {ctx with ptr=ctx.ptr-1}, [STOREGLOBAL (Int32.to_int x.it)]
 | Call v ->
   (* Will just push the pc *)
   let FuncType (par,ret) = Hashtbl.find ctx.f_types v.it in
   {ctx with ptr=ctx.ptr+List.length ret-List.length par}, [CALL (Int32.to_int v.it)]
 | CallIndirect v ->
   let FuncType (par,ret) = Hashtbl.find ctx.f_types v.it in
   {ctx with ptr=ctx.ptr+List.length ret-List.length par}, [CALLI 0]
 | Select ->
   let else_label = ctx.label in
   let end_label = ctx.label+1 in
   let ctx = {ctx with ptr=ctx.ptr-2; label=ctx.label+2} in
   ctx, [JUMPI else_label; DROP; DROP; JUMP end_label; LABEL else_label; DUP 1; SWAP 2; DROP; DROP; DROP; LABEL end_label]
 (* Dup ptr will give local 0 *)
 | GetLocal v ->
   {ctx with ptr=ctx.ptr+1}, [DUP (Int32.to_int v.it+ctx.ptr)]
 | SetLocal v ->
   {ctx with ptr=ctx.ptr-1}, [SWAP (Int32.to_int v.it+ctx.ptr); DROP]
 | TeeLocal v ->
   ctx, [SWAP (Int32.to_int v.it+ctx.ptr)]
 | Load op -> ctx, [LOAD (Int32.to_int op.offset)]
 | Store op -> {ctx with ptr=ctx.ptr-1}, [STORE (Int32.to_int op.offset)]

and compile_break ctx = function
 | 0 -> ctx, [BREAK]
 | i ->
   let ctx, rest = compile_break ctx (i-1) in
   ctx, [POPBRK] @ rest

and compile_block ctx = function
 | [] -> ctx, []
 | a::tl ->
    let ctx, a = compile ctx a in
    let ctx, rest = compile_block ctx tl in
    ctx, a @ rest

let rec make a n = if n = 0 then [] else a :: make a (n-1) 

let compile_func ctx func =
  let FuncType (par,ret) = Hashtbl.find ctx.f_types func.it.ftype.it in
  (* Just params are now in the stack *)
  let ctx, body = compile' {ctx with ptr=ctx.ptr+List.length par+List.length func.it.locals} (Block ([], func.it.body)) in
  ctx,
  make (PUSH {it=I32 Int32.zero; at=no_region}) (List.length func.it.locals) @
  body @
  List.flatten (List.mapi (fun i _ -> [DUP (List.length ret - i); SWAP (ctx.ptr-i); DROP]) ret) @
  make DROP (List.length par + List.length func.it.locals) @
  [RETURN]

(* This resolves only one function, think more *)
let resolve_inst tab = function
 | LABEL _ -> NOP
 | JUMP l -> JUMP (Hashtbl.find tab l)
 | JUMPI l -> JUMPI (Hashtbl.find tab l)
(* | CALL l -> CALL (Hashtbl.find tab l) *)
 | PUSHBRK l -> PUSHBRK (Hashtbl.find tab l)
 | a -> a

let resolve_to n lst =
  let tab = Hashtbl.create 10 in
  List.iteri (fun i inst -> match inst with LABEL l -> Hashtbl.add tab l (i+n)| _ -> ()) lst;
  List.map (resolve_inst tab) lst

let resolve_inst2 tab = function
 | CALL l -> CALL (Hashtbl.find tab l)
 | a -> a

let compile_module m =
  let ftab = Hashtbl.create 10 in
  let ttab = Hashtbl.create 10 in
  List.iteri (fun i f -> Hashtbl.add ttab (Int32.of_int i) f.it) m.types;
  List.iteri (fun i f ->
    let ty = Hashtbl.find ttab f.it.ftype.it in
    Hashtbl.add ftab (Int32.of_int i) ty) m.funcs;
  let module_codes = List.map (compile_func {ptr=0; label=0; f_types=ftab; bptr=0}) m.funcs in
  let f_resolve = Hashtbl.create 10 in
  let rec build n acc = function
   | [] -> acc
   | (_,md)::tl ->
     let sz = List.length acc in
     Hashtbl.add f_resolve n sz;
     build (n+1) (acc@resolve_to sz md) tl in
  let flat_code = build 0 [] module_codes in
  List.map (resolve_inst2 f_resolve) flat_code

(* perhaps for now just make a mega module *)
let compile_modules lst =
  let mega = {empty_module with
     types=List.flatten (List.map (fun m -> m.types) lst);
     funcs=List.flatten (List.map (fun m -> m.funcs) lst);
  } in
  compile_module mega



