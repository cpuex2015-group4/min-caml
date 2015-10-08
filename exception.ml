open Lexing

let buffer = ref (Bytes.create 0)

(* 開始位置, 終了位置, メッセージ *)
exception Lexing_failure of position * position * string
exception Parsing_failure of position * position * string
exception Typing_failure of position * position * string
