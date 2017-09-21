
open Values
open Types
open Instance

let lookup name t =
  match Utf8.encode name, t with
  | "NaN", ExternalGlobalType t -> ExternalGlobal (F64 F64.pos_nan)
  | "Infinity", ExternalGlobalType t -> ExternalGlobal (F64 (F64.div (F64.of_float 1.0) F64.zero))
  | _ -> raise Not_found
