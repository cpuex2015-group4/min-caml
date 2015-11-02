%{
(* parserが利用する変数、関数、型などの定義 *)
open Syntax
open Exception
let addtyp x = (x, Type.gentyp ())
%}

/* (* 字句を表すデータ型の定義 (caml2html: parser_token) *) */
%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token NOT
%token AST
%token SLASH
%token MINUS
%token PLUS
%token MINUS_DOT
%token PLUS_DOT
%token AST_DOT
%token SLASH_DOT
%token EQUAL
%token LESS_GREATER
%token LESS_EQUAL
%token GREATER_EQUAL
%token LESS
%token GREATER
%token IF
%token THEN
%token ELSE
%token <Id.t> IDENT
%token LET
%token IN
%token REC
%token COMMA
%token ARRAY_CREATE
%token DOT
%token LESS_MINUS
%token SEMICOLON
%token LPAREN
%token RPAREN
%token EOF

/* (* 優先順位とassociativityの定義（低い方から高い方へ） (caml2html: parser_prior) *) */
%right prec_let
%right SEMICOLON
%right prec_if
%right LESS_MINUS
%left COMMA
%left EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS PLUS_DOT MINUS_DOT
%left AST_DOT SLASH_DOT
%right prec_unary_minus
%left prec_app
%left DOT

/* (* 開始記号の定義 *) */
%type <Syntax.t> exp
%start exp

%%

simple_exp: /* (* 括弧をつけなくても関数の引数になれる式 (caml2html: parser_simple) *) */
| LPAREN exp RPAREN
    { info (syntax $2) }
| LPAREN RPAREN
    { info Unit }
| BOOL
    { info (Bool($1)) }
| INT
    { info (Int($1)) }
| FLOAT
    { info (Float($1)) }
| IDENT
    { info (Var($1)) }
| simple_exp DOT LPAREN exp RPAREN
    { info (Get($1, $4)) }

exp: /* (* 一般の式 (caml2html: parser_exp) *) */
| simple_exp
    { $1 }
| NOT exp
    %prec prec_app
    { info (Not($2)) }
| MINUS exp
    %prec prec_unary_minus
    { match syntax($2) with
    | Float(f) -> info (Float(-.f)) (* -1.23などは型エラーではないので別扱い *)
    | e -> info (Neg($2)) }
| exp AST exp
    { info (Mul($1, $3)) }
| exp SLASH exp
    { info (Div($1, $3)) }
| exp PLUS exp /* (* 足し算を構文解析するルール (caml2html: parser_add) *) */
    { info (Add($1, $3)) }
| exp MINUS exp
    { info (Sub($1, $3)) }
| exp EQUAL exp
    { info (Eq($1, $3)) }
| exp LESS_GREATER exp
    { info (Not(info(Eq($1, $3)))) }
| exp LESS exp
    { info (Not(info(LE($3, $1)))) }
| exp GREATER exp
    { info (Not(info(LE($1, $3)))) }
| exp LESS_EQUAL exp
    { info (LE($1, $3)) }
| exp GREATER_EQUAL exp
    { info (LE($3, $1)) }
| IF exp THEN exp ELSE exp
    %prec prec_if
    { info (If($2, $4, $6)) }
| MINUS_DOT exp
    %prec prec_unary_minus
    { info (FNeg($2)) }
| exp PLUS_DOT exp
    { info (FAdd($1, $3)) }
| exp MINUS_DOT exp
    { info (FSub($1, $3)) }
| exp AST_DOT exp
    { info (FMul($1, $3)) }
| exp SLASH_DOT exp
    { info (FDiv($1, $3)) }
| LET IDENT EQUAL exp IN exp
    %prec prec_let
    { info (Let(addtyp $2, $4, $6)) }
| LET REC fundef IN exp
    %prec prec_let
    { info (LetRec($3, $5)) }
| exp actual_args
    %prec prec_app
    { info (App($1, $2)) }
| elems
    { info (Tuple($1)) }
| LET LPAREN pat RPAREN EQUAL exp IN exp
    { info (LetTuple($3, $6, $8)) }
| simple_exp DOT LPAREN exp RPAREN LESS_MINUS exp
    { info (Put($1, $4, $7)) }
| exp SEMICOLON exp
    { info (Let((Id.gentmp Type.Unit, Type.Unit), $1, $3)) }
| ARRAY_CREATE simple_exp simple_exp
    %prec prec_app
    { info (Array($2, $3)) }
| error
    { let start_p = Parsing.symbol_start_pos () in
      let end_p = Parsing.symbol_end_pos () in
      let token = Bytes.sub_string !Exception.buffer
        (Parsing.symbol_start ())
        (Parsing.symbol_end () - Parsing.symbol_start ())
      in
      raise (Parsing_failure (
        start_p, end_p,
      	(Printf.sprintf "parse error near `%s`" token))) }

fundef:
| IDENT formal_args EQUAL exp
    { { name = addtyp $1; args = syntax $2; body = $4 } }

formal_args:
| IDENT formal_args
    { info (addtyp $1 :: syntax $2) }
| IDENT
    { info [addtyp $1] }

actual_args:
| actual_args simple_exp
    %prec prec_app
    { $1 @ [$2] }
| simple_exp
    %prec prec_app
    { [$1] }

elems:
| elems COMMA exp
    { ($1 @ [$3]) }
| exp COMMA exp
    { [$1; $3] }

pat:
| pat COMMA IDENT
    { ($1 @ [addtyp $3]) }
| IDENT COMMA IDENT
    { [addtyp $1; addtyp $3] }
