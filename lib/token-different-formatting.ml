open Base

type token_type =
  | ILLEGAL
  | EOF
  (* Identifiers/literals *)
  | IDENT
  | INT
  (* Operators *)
  | ASSIGN
  | PLUS
  | MINUS
  | BANG
  | STAR
  | SLASH
  (* equality *)
  | LT (* < *)
  | GT (* > *)
  | EQ (* == *)
  | NEQ (* != *)
  (* delimiters *)
  | COMMA
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  (* keywords *)
  | FUNCTION
  | LET
  | TRUE
  | FALSE
  | IF
  | ELSE
  | RETURN
[@@deriving show, eq, ord, sexp]

let kw =
  Map.of_alist_exn
    (module String)
    [
      ("fn", FUNCTION);
      ("let", LET);
      ("true", TRUE);
      ("false", FALSE);
      ("if", IF);
      ("else", ELSE);
      ("return", RETURN);
    ]

type t = { token : token_type; literal : string }
[@@deriving show, eq, ord, sexp]

let lookup_ident s = match Base.Map.find kw s with Some t -> t | None -> IDENT
let make token ch = { token; literal = String.of_char ch }
let illegal ch = { token = ILLEGAL; literal = String.of_char ch }
let eof () = { token = EOF; literal = "" }
