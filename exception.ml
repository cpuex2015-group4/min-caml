open Lexing

let buffer = ref (Bytes.create 0)

(* bytes$BCf$K;XDj$7$?J8;z$,2?2s=P8=$9$k$+(B *)
let rec char_count buf c from until =
  try
    let i = Bytes.index_from buf from c in
    if i > until then 0 else 1 + (char_count buf c (i+1) until)
  with Not_found -> 0

(* $B8=:_0LCV$,%P%C%U%!Cf$N2?9TL\$+%+%&%s%H$9$k(B *)
let line_number pos = char_count !buffer '\n' 0 (pos-1) + 1

(* lexbuf$B$r<u$1<h$j!$8=:_0LCV>pJs$rJV$9(B *)
(* $B8=:_$N9THV9f$H!$9T$N3+;O0LCV$N%P%C%U%!$N@hF,$+$i$N0LCV(B *)
let curr_pos_info pos =
  let pos = pos in
  let lnum = line_number pos in
  try
    let bol = Bytes.rindex_from !buffer (pos-1) '\n' in
    (lnum, bol)
  with
  | Not_found -> (lnum, 0)
  | Invalid_argument _ -> (lnum, (Bytes.length !buffer))

(* $B9THV9f(B, $B3+;O0LCV(B, $B=*N;0LCV(B, $B%a%C%;!<%8(B *)
exception Lexing_failure of int * int * int * string
exception Parsing_failure of int * int * int * string
exception Typing_failure of int * int * int * string
