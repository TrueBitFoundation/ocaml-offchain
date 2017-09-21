let name = "wasm"
let version = "1.0"

let configure () =
  Import.register (Utf8.decode "spectest") Spectest.lookup;
  Import.register (Utf8.decode "input") Input.lookup;
  Import.register (Utf8.decode "global") Global.lookup;
  Import.register (Utf8.decode "env") Env.lookup

let banner () =
  print_endline (name ^ " " ^ version ^ " reference interpreter")

let usage = "Usage: " ^ name ^ " [option] [file ...]"

let args = ref []
let add_arg source = args := !args @ [source]

let quote s = "\"" ^ String.escaped s ^ "\""

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
  "-h", Arg.Clear Flags.harness, " exclude harness for JS convesion";
  "-d", Arg.Set Flags.dry, " dry, do not run program";
  "-t", Arg.Set Flags.trace, " trace execution";
  "-v", Arg.Unit banner, " show version";

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
  "-input-file", Arg.String (fun file -> Flags.input_file := Some file), " set the file that contains input from the blockchain";
]

let () =
  Printexc.record_backtrace true;
  try
    configure ();
    Arg.parse argspec
      (fun file -> add_arg ("(input " ^ quote file ^ ")")) usage;
    List.iter (fun arg -> if not (Run.run_string arg) then exit 1) !args;
    if !args = [] then Flags.interactive := true;
    if !Flags.interactive then begin
      Flags.print_sig := true;
      banner ();
      Run.run_stdin ()
    end
  with exn ->
    flush_all ();
    prerr_endline
      (Sys.argv.(0) ^ ": uncaught exception " ^ Printexc.to_string exn);
    Printexc.print_backtrace stderr;
    exit 2
