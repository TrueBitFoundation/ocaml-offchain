
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

let init = ref false
let result = ref false

let case = ref (-1)
let location = ref (-1)

let input_file : string option ref = ref None
