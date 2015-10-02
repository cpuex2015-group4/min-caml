open Lexing

(* bytes中に指定した文字が何回出現するか *)
let rec char_count buf c from until =
  try
    let i = Bytes.index_from buf from c in
    if i > until then 0 else 1 + (char_count buf c (i+1) until)
  with Not_found -> 0

(* 現在位置がバッファ中の何行目かカウントする *)
let line_number buf pos = char_count buf '\n' 0 pos

(* lexbufを受け取り，現在位置情報を返す *)
(* 現在の行番号と，行の開始位置のバッファの先頭からの位置 *)
let curr_pos_info lexbuf =
  let pos = lexbuf.lex_curr_pos in
  let lnum = line_number lexbuf.lex_buffer pos in
  try
    let bol = Bytes.rindex_from lexbuf.lex_buffer (pos-1) '\n' in
    (lnum, bol)
  with Not_found -> (lnum, 0)

(* 行番号, 開始位置, 終了位置, メッセージ *)
exception Lexing_failure of int * int * int * string
