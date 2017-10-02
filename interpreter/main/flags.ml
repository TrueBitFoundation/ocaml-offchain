
let interactive = ref false
let trace = ref false
let unchecked = ref false
let print_sig = ref false
let dry = ref false
let width = ref 80
let harness = ref true

let merkle = ref false
let microstep = ref false
let trace_stack = ref false

let insert_error = ref (-1)

let checkstep = ref (-1)
let checkfinal = ref (-1)
let checkerror = ref (-1)

let init = ref false
let init_vm = ref false
let result = ref false

let memory_size = ref 2
let table_size = ref 64
let globals_size = ref 64
let stack_size = ref (16*1024)
let call_size = ref 1024

let run_wasm = ref false

let case = ref (-1)
let location = ref (-1)

let input_files : string list ref = ref []
let arguments : string list ref = ref []

