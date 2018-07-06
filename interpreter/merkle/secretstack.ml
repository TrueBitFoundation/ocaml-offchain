
open Merkle
open Ast
open Source
open Types

(* Analyze stack *)

let do_it x f = {x with it=f x.it}

let it e = {it=e; at=no_region}

type control = {
  rets : int;
  level : int;
}

type context = {
  ptr : int;
  locals : int;
  bptr : int;
  stack : Int32.t list;
  f_types : (Int32.t, func_type) Hashtbl.t;
  f_types2 : (Int32.t, func_type) Hashtbl.t;
  block_return : control list;
  mutable marked : Int32.t list;
}

let relabel lst =
   let uniq = ref 1 in
   let rec compile expr =
      incr uniq;
      {it=compile' expr.it; at={left=no_pos; right={file="label"; line= !uniq; column=0}}}
   and compile' = function
    | Block (ty, lst) -> Block (ty, List.map compile lst)
    | Loop (ty, lst) -> Loop (ty, List.map compile lst)
    | If (ty, texp, fexp) -> If (ty, List.map compile texp, List.map compile fexp)
    | a -> a in
  List.map compile lst

(* the idea would be to add local variables so that there are never hidden elements in the stack when making a call *)

let rec popn n = function
 | a::tl when n > 0 -> popn (n-1) tl
 | lst -> lst

let rec take n = function
 | a::tl when n > 0 -> a :: take (n-1) tl
 | lst -> []

let una_stack id x = {x with stack=id::popn 1 x.stack}
let bin_stack id x = {x with stack=id::popn 2 x.stack}
let n_stack n id x = {x with stack=id::popn n x.stack}

let mark_expr ctx lst = ctx.marked <- lst@ctx.marked

let rec compile (ctx : context) expr = compile' ctx (Int32.of_int expr.at.right.line) expr.it
and compile' ctx id = function
 | Block (ty, lst) ->
   let rets = List.length ty in
   let extra = ctx.ptr - ctx.locals in
   if extra > 0 then trace ("block start " ^ string_of_int extra);
   let old_return = ctx.block_return in
   let old_ptr = ctx.ptr in
   let old_stack = ctx.stack in
   let ctx = {ctx with bptr=ctx.bptr+1; block_return={level=old_ptr+rets; rets=rets}::ctx.block_return} in
   let ctx = compile_block ctx lst in
   if extra > 0 then trace ("block end");
   {ctx with bptr=ctx.bptr-1; block_return=old_return; ptr=old_ptr+rets; stack=make id rets @ old_stack}
 (* Loops have no return types currently *)
 | Loop (_, lst) ->
   let old_return = ctx.block_return in
   let extra = ctx.ptr - ctx.locals in
   if extra > 0 then trace ("loop start " ^ string_of_int extra);
   let ctx = {ctx with bptr=ctx.bptr+1; block_return={level=ctx.ptr; rets=0}::old_return} in
   let ctx = compile_block ctx lst in
   if extra > 0 then trace ("loop end " ^ string_of_int extra);
   {ctx with bptr=ctx.bptr-1; block_return=old_return}
 | Call v ->
   (* Will just push the pc *)
   let FuncType (par,ret) = Hashtbl.find ctx.f_types v.it in
   let extra = ctx.ptr - ctx.locals - List.length par in
   if extra > 0 then trace ("call " ^ string_of_int extra);
   mark_expr ctx (take extra (popn (List.length par) ctx.stack));
   {ctx with ptr=ctx.ptr+List.length ret-List.length par; stack=make id (List.length ret) @ popn (List.length par) ctx.stack}
 | CallIndirect v ->
   let FuncType (par,ret) = Hashtbl.find ctx.f_types2 v.it in
   let extra = ctx.ptr - ctx.locals - List.length par - 1 in
   if extra > 0 then trace ("calli " ^ string_of_int extra);
   mark_expr ctx (take extra (popn (List.length par+1) ctx.stack));
   {ctx with ptr=ctx.ptr+List.length ret-List.length par-1; stack=make id (List.length ret) @ popn (List.length par + 1) ctx.stack}
 | If (ty, texp, fexp) ->
   let a_ptr = ctx.ptr-1 in
   let ctx = {ctx with ptr=a_ptr} in
   let ctx = compile' ctx id (Block (ty, texp)) in
   let ctx = compile' {ctx with ptr=a_ptr} id (Block (ty, fexp)) in
   ctx
 | Const lit -> {ctx with ptr = ctx.ptr+1; stack=id::ctx.stack}
 | Test t -> una_stack id ctx
 | Compare i -> bin_stack id {ctx with ptr = ctx.ptr-1}
 | Unary i -> una_stack id ctx
 | Binary i -> bin_stack id {ctx with ptr = ctx.ptr-1}
 | Convert i -> una_stack id ctx
 | Unreachable -> ctx
 | Nop -> ctx
 (* breaks might be used to return values, check this *)
 | Br x ->
   let num = Int32.to_int x.it in
   let c = List.nth ctx.block_return num in
   {ctx with ptr=ctx.ptr - c.rets; stack=popn c.rets ctx.stack}
 | BrIf x ->
   let num = Int32.to_int x.it in
   {ctx with ptr = ctx.ptr-1; stack=popn 1 ctx.stack}
 | BrTable (tab, def) ->
   let num = Int32.to_int def.it in
   let { rets; _ } = List.nth ctx.block_return num in
   {ctx with ptr = ctx.ptr-1-rets; stack=popn (rets+1) ctx.stack}
 | Return ->
   let num = ctx.bptr-1 in
   let {level=ptr; rets} = List.nth ctx.block_return num in
   {ctx with ptr=ctx.ptr - rets}
 | Drop -> {ctx with ptr=ctx.ptr-1}
 | GrowMemory -> {ctx with ptr=ctx.ptr-1; stack=popn 1 ctx.stack}
 | CurrentMemory -> {ctx with ptr=ctx.ptr+1; stack=id::ctx.stack}
 | GetGlobal x -> {ctx with ptr=ctx.ptr+1; stack=id::ctx.stack}
 | SetGlobal x -> {ctx with ptr=ctx.ptr-1; stack=popn 1 ctx.stack}
 | Select -> n_stack 3 id {ctx with ptr=ctx.ptr-2}
 | GetLocal v -> {ctx with ptr=ctx.ptr+1; stack=id::ctx.stack}
 | SetLocal v -> {ctx with ptr=ctx.ptr-1; stack=popn 1 ctx.stack}
 | TeeLocal v -> una_stack id ctx
 | Load op -> una_stack id ctx
 | Store op -> bin_stack id {ctx with ptr=ctx.ptr-2}

and compile_block ctx = function
 | [] -> ctx
 | a::tl ->
    let ctx = compile ctx a in
    let ctx = compile_block ctx tl in
    ctx

let compile_func ctx func =
  let FuncType (par,ret) = Hashtbl.find ctx.f_types2 func.it.ftype.it in
  trace ("---- function start params:" ^ string_of_int (List.length par) ^ " locals: " ^ string_of_int (List.length func.it.locals));
  (* Just params are now in the stack *)
  let locals = List.length par + List.length func.it.locals in
  let ctx = compile' {ctx with ptr=locals; locals=locals} 0l (Block (ret, relabel func.it.body)) in
  trace ("---- function end " ^ string_of_int ctx.ptr);
  func

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

let process m =
  do_it m (fun m ->
     let ftab, ttab = make_tables m in
     let ctx = {ptr=0; bptr=0; block_return=[]; f_types2=ttab; f_types=ftab; locals=0; stack=[]; marked=[]} in
     {m with funcs=List.map (fun x -> compile_func ctx x) m.funcs})

