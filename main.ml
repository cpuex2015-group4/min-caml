open Printf
open Exception

let limit = ref 1000
let warning = ref ""

let rec iter n e = (* 最適化処理をくりかえす (caml2html: main_iter) *)
  Format.eprintf "iteration %d@." n;
  if n = 0 then e else
  let e' = Elim.f (ConstFold.f (Inline.f (Assoc.f (Beta.f e)))) in
  if e = e' then e else
  iter (n - 1) e'

let lexbuf outchan l = (* バッファをコンパイルしてチャンネルへ出力する (caml2html: main_lexbuf) *)
  Id.counter := 0;
  Typing.extenv := M.empty;
  Emit.f outchan
    (RegAlloc.f
       (Simm.f
	  (Virtual.f
	     (Closure.f
		(iter !limit
		   (Alpha.f (Cse.f (KNormal.f
			 (Typing.f !warning
			    (Parser.exp Lexer.token l))))))))))

let debug_spec opt outchan l = (* デバッグ出力する (caml2html: debug) *)
  Id.counter := 0;
  Typing.extenv := M.empty;
  let r1 = (Parser.exp Lexer.token l) in
  if opt = "parser" then Debug.parse r1 else
  let r2 = (KNormal.f (Typing.f !warning r1)) in
  if opt = "knormal" then Debug.knormal r2 else
  let r3 = (Cse.f r2) in
  if opt = "cse" then Debug.cse r3 else
  let r4 = (RegAlloc.f
            (Simm.f
              (Virtual.f
                (Closure.f
            (iter !limit (Alpha.f r3)))))) in
  if opt = "asm" then Debug.print_asmprog r4 else
  Emit.f outchan r4

let string s = lexbuf stdout (Lexing.from_string s) (* 文字列をコンパイルして標準出力に表示する (caml2html: main_string) *)

(* 対象ファイルに適用する関数(デフォルトはコンパイル関数lexbuf) *)
let spec = ref lexbuf

let file f = (* ファイルをコンパイルしてファイルに出力する (caml2html: main_file) *)
  let inchan = open_in (f ^ ".ml") in
  let outchan = open_out (f ^ ".s") in
  try
    let lexbuf = (Lexing.from_channel inchan) in
    (* エラー出力用に入力ファイルの内容をバッファに保存する *)
    Exception.buffer := Lexing.(lexbuf.lex_buffer);
    !spec outchan lexbuf;
    close_in inchan;
    close_out outchan;
  with e -> (close_in inchan; close_out outchan; raise e)

let () = (* ここからコンパイラの実行が開始される (caml2html: main_entry) *)
  let files = ref [] in
  (* エラー出力用に入力ファイル名を保存する *)
  let processing_file = ref "" in
  Arg.parse
    [("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
     ("-iter", Arg.Int(fun i -> limit := i), "maximum number of optimizations iterated");
     ("-debug", Arg.String(fun s -> spec := debug_spec s), "debug print [parser, knormal, cse, asm]");
     ("-W", Arg.String(fun opt -> warning := opt), "warning options [return_unit]")]
    (fun s -> files := !files @ [s])
    ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
     Printf.sprintf "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..." Sys.argv.(0));
  try
    List.iter (fun f -> processing_file := f; ignore (file f)) !files; ()
  with
  (* 例外を捕捉してエラー出力を行う *)
    Lexing_failure (i, j, msg)
  | Parsing_failure (i, j, msg)
  | Typing_failure (i, j, msg) ->
      printf "File \"%s.ml\", line %d, character %d-%d:\nError: %s\n"
      !processing_file Lexing.(i.pos_lnum)
      (Lexing.(i.pos_cnum) - Lexing.(i.pos_bol))
      (Lexing.(j.pos_cnum) - Lexing.(j.pos_bol)) msg;
      exit 2
  | Failure (msg) ->
      printf "File \"%s.ml\":\nError: %s\n" !processing_file msg;
      exit 2
