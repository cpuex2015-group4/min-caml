{
(* lexerが利用する変数、関数、型などの定義 *)
open Parser
open Type
open Exception
}

(* 正規表現の略記 *)
let space = [' ' '\t']
let newline = ['\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
| space+
    { token lexbuf }
| newline
    { Lexing.new_line lexbuf;
      token lexbuf }
| "(*"
    { comment lexbuf; (* ネストしたコメントのためのトリック *)
      token lexbuf }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| "true"
    { BOOL(true) }
| "false"
    { BOOL(false) }
| "not"
    { NOT }
| digit+ (* 整数を字句解析するルール (caml2html: lexer_int) *)
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
| digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?
    { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
| '*'
    { AST }
| '/'
    { SLASH }
| '-' (* -.より後回しにしなくても良い? 最長一致? *)
    { MINUS }
| '+' (* +.より後回しにしなくても良い? 最長一致? *)
    { PLUS }
| "-."
    { MINUS_DOT }
| "+."
    { PLUS_DOT }
| "*."
    { AST_DOT }
| "/."
    { SLASH_DOT }
| '='
    { EQUAL }
| "<>"
    { LESS_GREATER }
| "<="
    { LESS_EQUAL }
| ">="
    { GREATER_EQUAL }
| '<'
    { LESS }
| '>'
    { GREATER }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
| "let"
    { LET }
| "in"
    { IN }
| "rec"
    { REC }
| ','
    { COMMA }
| '_'
    { IDENT(Id.gentmp Type.Unit) }
| "Array.create" (* [XX] ad hoc *)
    { ARRAY_CREATE }
| "create_array" (* [XX] ad hoc *)
    { ARRAY_CREATE }
| '.'
    { DOT }
| "<-"
    { LESS_MINUS }
| ';'
    { SEMICOLON }
| eof
    { EOF }
| lower (digit|lower|upper|'_'|''')* (* 他の「予約語」より後でないといけない *)
    { IDENT(Lexing.lexeme lexbuf) }
| _
    { raise (Lexing_failure (
        Lexing.lexeme_start_p lexbuf,
        Lexing.lexeme_end_p lexbuf,
      	(Printf.sprintf "unknown token `%s`" (Lexing.lexeme lexbuf)))) }
and comment = parse
| "*)"
    { () }
| "(*"
    { comment lexbuf;
      comment lexbuf }
| eof
    { Format.eprintf "warning: unterminated comment@." }
| _
    { comment lexbuf }
