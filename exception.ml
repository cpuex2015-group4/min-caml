open Lexing

let buffer = ref (Bytes.create 0)

(* bytes中に指定した文字が何回出現するか *)
let rec char_count buf c from until =
  try
    let i = Bytes.index_from buf from c in
    if i > until then 0 else 1 + (char_count buf c (i+1) until)
  with Not_found -> 0

(* 現在位置がバッファ中の何行目かカウントする *)
let line_number pos = char_count !buffer '\n' 0 (pos-1) + 1

(* lexbufを受け取り，現在位置情報を返す *)
(* 現在の行番号と，行の開始位置のバッファの先頭からの位置 *)
let curr_pos_info pos =
  let pos = pos in
  let lnum = line_number pos in
  try
    let bol = Bytes.rindex_from !buffer (pos-1) '\n' in
    (lnum, bol)
  with
  | Not_found -> (lnum, 0)
  | Invalid_argument _ -> (lnum, (Bytes.length !buffer))

(* 行番号, 開始位置, 終了位置, メッセージ *)
exception Lexing_failure of int * int * int * string
exception Parsing_failure of int * int * int * string
exception Typing_failure of int * int * int * string
