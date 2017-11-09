
let name = "wasm"
let version = "1.0"

let configure () =
  Import.register (Utf8.decode "spectest") Spectest.lookup;
  Import.register (Utf8.decode "input") Input.lookup;
  Import.register (Utf8.decode "global") Global.lookup;
  Import.register (Utf8.decode "global.Math") Global.lookup_math;
  Import.register (Utf8.decode "asm2wasm") Global.lookup_asm2wasm;
  Import.register (Utf8.decode "env") Env.lookup

let banner () =
  print_endline (name ^ " " ^ version ^ " reference interpreter")

let usage = "Usage: " ^ name ^ " [option] [file ...]"

let args = ref []
let add_arg source = args := !args @ [source]

let quote s = "\"" ^ String.escaped s ^ "\""

let merge_mode = ref false
let globals_file = ref None
let init_code = ref None
let print_imports = ref false
let do_compile = ref false
let run_inited : string option ref = ref None
let underscore_mode = ref false

let argspec = Arg.align
[
  "-", Arg.Set Flags.interactive,
    " run interactively (default if no files given)";
  "-e", Arg.String add_arg, " evaluate string";
  "-i", Arg.String (fun file -> add_arg ("(input " ^ quote file ^ ")")),
    " read script from file";
  "-o", Arg.String (fun file -> add_arg ("(output " ^ quote file ^ ")")),
    " write module to file";
  "-w", Arg.Int (fun n -> Flags.width := n),
    " configure output width (default is 80)";
  "-s", Arg.Set Flags.print_sig, " show module signatures";
  "-u", Arg.Set Flags.unchecked, " unchecked, do not perform validation";
  "-h", Arg.Clear Flags.harness, " exclude harness for JS conversion";
  "-d", Arg.Set Flags.dry, " dry, do not run program";
  "-t", Arg.Set Flags.trace, " trace execution";
  "-v", Arg.Unit banner, " show version";

  "-merge", Arg.Set merge_mode, " merge files";
  "-underscore", Arg.Set underscore_mode, " add underscores to all of the names";
  "-add-globals", Arg.String (fun s -> globals_file := Some s), " add globals to the module";
  "-init-code", Arg.String (fun s -> add_arg ("(input " ^ quote s ^ ")") ; init_code := Some s), " output initial code for a wasm file";
  "-imports", Arg.Set print_imports, " print imports from the wasm file";
  "-compile", Arg.Set do_compile, "Compiles wasm file to C";

  "-trace-stack", Arg.Set Flags.trace_stack, " trace execution stack";
  "-m", Arg.Set Flags.merkle, " merkle proof mode";
  "-micro", Arg.Set Flags.microstep, " merkle proof mode (microsteps)";
  "-merkletest", Arg.Int (fun n -> Mbinary.test n; exit 0), " just run a merkle root computation test with a number of leafs";
  "-init", Arg.Set Flags.init, " output initial state hash of a test case";
  "-init-vm", Arg.Set Flags.init_vm, " output initial vm of a test case";
  "-result", Arg.Set Flags.result, " output final state hash of a test case and the number of steps";
  "-case", Arg.Int (fun n -> Flags.case := n), " for which test case the hash or proofs will be generated";
  "-location", Arg.Int (fun n -> Flags.location := n), " for which step the hash will be generated";
  "-step", Arg.Int (fun n -> Flags.checkstep := n), " for which step the proofs will be generated";
  "-error-step", Arg.Int (fun n -> Flags.checkerror := n), " for which step the intermediate state will be generated";
  "-final", Arg.Int (fun n -> Flags.checkfinal := n), " generate finality proof for the specified step";
  "-insert-error", Arg.Int (fun n -> Flags.insert_error := n), " insert a simple error so that verifier and solver will disagree";
  "-memory-size", Arg.Int (fun sz -> Flags.memory_size := sz), " how many pages the size of the memory should be. One page is 64kb";
  "-table-size", Arg.Int (fun sz -> Flags.table_size := sz), " how many elements should the call table have. Default 64";
  "-globals-size", Arg.Int (fun sz -> Flags.globals_size := sz), " how many elements should the globals table have. Default 64";
  "-stack-size", Arg.Int (fun sz -> Flags.stack_size := sz), " how many elements should the stack have. Default 16384";
  "-call-stack-size", Arg.Int (fun sz -> Flags.call_size := sz), " how many elements should the call stack have. Default 1024";
(*  "-run-inited", Arg.String (fun file -> run_inited := Some file), "run pre-initialized code from a file."; *)
  "-wasm", Arg.String (fun file ->
    add_arg ("(input " ^ quote file ^ ")");
    Flags.run_wasm := true;
    Flags.case := 0;
    add_arg "(invoke \"_main\")"), " run main function from this file";
  "-file", Arg.String (fun file -> Flags.input_files := file :: !Flags.input_files), " add a file to the VM file system";
  "-arg", Arg.String (fun file -> Flags.arguments := file :: !Flags.arguments), " add command line argument to the VM";
  "-input-proof", Arg.String (fun file -> Flags.input_file_proof := Some file), " output proof that an input file is in the initial state";
  "-output-proof", Arg.String (fun file -> Flags.output_file_proof := Some file), " output proof that an output file is in the final state";
  "-input", Arg.Set Flags.input_proof, " output information about input";
  "-output", Arg.Set Flags.output_proof, " output information about output";
]

let () =
  Printexc.record_backtrace true;
  try
    configure ();
    Arg.parse argspec
      (fun file -> add_arg ("(input " ^ quote file ^ ")")) usage;
    List.iter (fun arg -> if not (Run.run_string arg) then exit 1) !args;
    let lst = ref [] in
    Run.Map.iter (fun a b -> if a <> "" then lst := b :: !lst) !Run.modules;
    if !merge_mode then begin
      Run.trace ("Going to merge");
      match !lst with
      | a::b::_ ->
        Run.trace "found modules";
        let merged = Merge.merge b a in
        (* Run.output_stdout (fun () -> merged); *)
        if !Flags.trace then Run.output_stdout (fun () -> merged)
        else Run.create_binary_file "merge.wasm" () (fun () -> merged)
      | _ -> ()
    end;
    ( match !globals_file, !lst with
    | Some fn, m :: _ ->
      let m = Addglobals.add_globals m fn in
      (* Run.output_stdout (fun () -> m); *)
      Run.create_binary_file "globals.wasm" () (fun () -> m)
    | _ -> () );
    ( match !underscore_mode, !lst with
    | true, m :: _ ->
      let m = Underscore.process m in
      (* Run.output_stdout (fun () -> m); *)
      Run.create_binary_file "underscore.wasm" () (fun () -> m)
    | _ -> () );
    ( match !init_code, !lst with
    | Some fn, m :: _ ->
      let open Source in
      let open Mrun in
      let inst = Run.lookup_instance None no_region in
      ( match Instance.export inst (Utf8.decode "_main") with
      | Some (Instance.ExternalFunc (Instance.AstFunc (_, func))) ->
        let vm = Run.setup_vm inst inst.Instance.module_.it func [] in
        let oc = open_out_bin "decoded.bin" in
        for i = 0 to Array.length vm.code - 1 do
          let inst = vm.code.(i) in
          output_bytes oc (Mbinary.microp_word (get_code inst))
        done;
        close_out oc
      | _ -> () )
    | _ -> () );
(*    ( match !run_inited with
    | Some file ->
       let ic = open_in file in
       let len = in_channel_length ic in
       let str = really_input_string ic len in
       (* blah, cannot actually execute it *)
       ()
    | None -> () ); *)
    ( match !do_compile, !lst with
    | true, m :: _ ->
      let open Source in
      let open Mrun in
      let inst = Run.lookup_instance None no_region in
      ( match Instance.export inst (Utf8.decode "_main") with
      | Some (Instance.ExternalFunc (Instance.AstFunc (_, func))) ->
        let vm = Run.setup_vm inst inst.Instance.module_.it func [] in
        print_string (Compiler.compile_all vm.code)
      | _ -> () )
    | _ -> () );
    ( match !print_imports, !lst with
    | true, m :: _ ->
      let open Source in
      let open Ast in
      let lst = Merkle.func_imports m in
      let import_name n = "[\"" ^ Utf8.encode n.it.module_name ^ "\",\"" ^ Utf8.encode n.it.item_name ^ "\"]" in
      Printf.printf "[%s]\n" (String.concat ", " (List.map import_name lst))
    | _ -> () );
    if !args = [] then Flags.interactive := true;
    if !Flags.interactive then begin
      Flags.print_sig := true;
      banner ();
      Run.run_stdin ()
    end
  with exn ->
    flush_all ();
    prerr_endline (Sys.argv.(0) ^ ": uncaught exception " ^ Printexc.to_string exn);
    Printexc.print_backtrace stderr;
    exit 2


