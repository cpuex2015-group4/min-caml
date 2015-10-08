open Lexing

let buffer = ref (Bytes.create 0)

(* $B3+;O0LCV(B, $B=*N;0LCV(B, $B%a%C%;!<%8(B *)
exception Lexing_failure of position * position * string
exception Parsing_failure of position * position * string
exception Typing_failure of position * position * string
