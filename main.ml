open Printf
open Exception

let limit = ref 1000

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
			 (Typing.f
			    (Parser.exp Lexer.token l))))))))))

let debug_parser outchan l = (* パーサの結果をデバッグ出力する (caml2html: debug_parser) *)
  Id.counter := 0;
	(Debug.parse
		(Parser.exp Lexer.token l))

let debug_knormal outchan l = (* K正規化の結果をデバッグ出力する (caml2html: debug_knormal) *)
  Id.counter := 0;
	(Debug.knormal
		 (KNormal.f
			 (Typing.f
			    (Parser.exp Lexer.token l))))

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

let debug_spec s = (* デバッグ関数を選択する *)
  match s with
  | "parser" -> debug_parser
  | "knormal" -> debug_knormal
  (* TODO: exhaust match case *)

let () = (* ここからコンパイラの実行が開始される (caml2html: main_entry) *)
  let files = ref [] in
  (* エラー出力用に入力ファイル名を保存する *)
  let processing_file = ref "" in
  Arg.parse
    [("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
     ("-iter", Arg.Int(fun i -> limit := i), "maximum number of optimizations iterated");
     ("-debug", Arg.String(fun s -> spec := debug_spec s), "debug print [parser, knormal]")]
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
      printf "File \"%s.ml\", line %d, character %d-%d:\nError: %s"
      !processing_file Lexing.(i.pos_lnum)
      (Lexing.(i.pos_cnum) - Lexing.(i.pos_bol))
      (Lexing.(j.pos_cnum) - Lexing.(j.pos_bol)) msg
  | Failure (msg) ->
      printf "File \"%s.ml\":\nError: %s" !processing_file msg
