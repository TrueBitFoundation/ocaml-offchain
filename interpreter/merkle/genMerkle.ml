
(* read from merkle tree of any shape *)

(* each file/merkle root has chunks *)

type file = (bytes, bytes) Hashtbl.t

let chunks : (bytes, bytes) Hashtbl.t = Hashtbl.create 1000

let rec read_file_aux id lst prefix_length =
  match lst with
  | [] -> id
  | (start, stop) :: lst ->
     let chunk = Hashtbl.find chunks id in
     let id = Bytes.sub chunk (start-prefix_length) 32 in
     read_file_aux id lst (prefix_length+stop)

let read_file id lst = read_file_aux id lst 0

let rec pairify = function
 | a::b::lst -> (a,b) :: pairify lst
 | _ -> []

(* Parse the file *)
let do_read dta =
  let id = Bytes.sub dta 0 32 in
  let rec read idx =
     if idx+32 >= Bytes.length dta then [] else
     Byteutil.bytes32_to_int (Bytes.sub dta idx 32) :: read (idx+32) in
  read_file id (pairify (read 32))

