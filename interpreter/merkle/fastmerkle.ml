
open Ctypes
open Foreign

let mylib = Dl.dlopen ~flags:[Dl.RTLD_LAZY] ~filename:"/usr/local/bin/fastmerkle.sox"

let makeFunc = foreign "makeProof" ~from:mylib (int @-> string @-> int @-> int @-> returning (ptr char))

(* val char_ptr_of_string : string -> char ptr *)

let rec calc n = if n = 0 then 0 else 1 + calc(n/2)

let rec pow2 n = if n = 0 then 1 else 2 * pow2 (n-1)

let size n = pow2 (calc (n-1))

type w256 = string

let makeProof str num =
   let len = String.length str in
   string_from_ptr (makeFunc len str num 0) (32 * calc (len/32))

let makeProof16 str num =
   let len = String.length str in
   string_from_ptr (makeFunc len str num 1) (32 * calc (len/32))

let calc_hash (arr: w256 array) n =
   let len = size (Array.length arr) * 32 in
   let str = Bytes.make len '\000' in
   for i = 0 to Array.length arr - 1 do
      Bytes.blit_string arr.(i) 0 str (i*32) 32
   done;
   makeProof (Bytes.to_string str) n

let calc_hash16 (arr: w256 array) n =
   let len = max 32 (size (Array.length arr) * 16) in
   let str = Bytes.make len '\000' in
   for i = 0 to Array.length arr - 1 do
      Bytes.blit_string arr.(i) 0 str (i*16) 16
   done;
   makeProof16 (Bytes.to_string str) n

let calc_map_hash f arr n =
   let len = size (Array.length arr) * 32 in
   let str = Bytes.make len '\000' in
   for i = 0 to Array.length arr - 1 do
      Bytes.blit_string (f arr.(i)) 0 str (i*32) 32
   done;
   makeProof (Bytes.to_string str) n

let get_hash (arr: w256 array) = 
   let res = calc_hash arr 0 in
   String.sub res (String.length res - 32) 32

let get_hash16 (arr: w256 array) = 
   let res = calc_hash16 arr 0 in
   String.sub res (String.length res - 32) 32

let map_hash f arr = 
   let res = calc_map_hash f arr 0 in
   String.sub res (String.length res - 32) 32

let w256_to_string bs =
  let res = ref "" in
  for i = 0 to String.length bs - 1 do
    let code = Char.code (String.get bs i) in
    res := !res ^ (if code < 16 then "0" else "") ^ Printf.sprintf "%x" code
  done;
  !res

let get_proof str =
   let len = String.length str / 32 - 1 in
   let res = ref [] in
   for i = 0 to len - 1 do
      res := String.sub str (i*32) 32 :: !res
   done;
   List.rev !res

let zeroword = String.make 32 '\000'

let location_proof arr num =
   let res = calc_hash arr num in
   arr.(num) :: get_proof res

let map_location_proof f arr num =
   let res = calc_map_hash f arr num in
   f arr.(num) :: get_proof res

(*
let _ =
   let arr = Array.make 1024 zeroword in
   prerr_endline (w256_to_string (get_hash arr));
   let str = makeProof (String.make (1024*1024*1024) '\000') 32 in
   prerr_endline (string_of_int (String.length str))
*)

