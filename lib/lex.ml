open Base

type t =
  { input : string
  ; input_len : int
  ; mutable pos : int
  ; mutable read_pos : int
  ; mutable ch : char
  }
[@@deriving show]

let read_char lex : unit =
  if lex.read_pos >= String.length lex.input
  then lex.ch <- Char.of_int_exn 0
  else lex.ch <- lex.input.[lex.read_pos];
  lex.pos <- lex.read_pos;
  lex.read_pos <- lex.read_pos + 1
;;

let make input : t =
  if String.is_empty input
  then { input; input_len = 0; pos = 0; read_pos = 0; ch = Char.of_int_exn 0 }
  else { input; input_len = String.length input; pos = 0; read_pos = 1; ch = input.[0] }
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false
;;

let skip_whitespace lex : unit =
  let is_whitespace = function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false
  in
  while is_whitespace lex.ch do
    read_char lex
  done
;;

let peek lex : char =
  if lex.read_pos >= lex.input_len then Char.of_int_exn 0 else lex.input.[lex.read_pos]
;;

let read_number lex : string =
  let position = lex.pos in
  while is_digit (peek lex) do
    read_char lex
  done;
  String.sub lex.input ~pos:position ~len:(lex.pos - position + 1)
;;

let read_ident lex : string =
  let position = lex.pos in
  while is_letter (peek lex) do
    read_char lex
  done;
  String.sub lex.input ~pos:position ~len:(lex.pos - position + 1)
;;

let tokenize_ident lex : Token.t =
  let literal = read_ident lex in
  let token = Token.lookup_ident literal in
  { literal; token }
;;

let tokenize_eq lex : Token.t =
  let ch = lex.ch in
  read_char lex;
  String.{ literal = of_char ch ^ of_char lex.ch; token = EQ }
;;

let tokenize_neq lex : Token.t =
  let ch = lex.ch in
  read_char lex;
  String.{ literal = of_char ch ^ of_char lex.ch; token = NEQ }
;;

let next_token lex : Token.t =
  let next_char_is = fun c -> Char.(peek lex = c) in
  skip_whitespace lex;
  let token =
    match lex.ch with
    | '=' when next_char_is '=' -> tokenize_eq lex
    | '!' when next_char_is '=' -> tokenize_neq lex
    | '=' -> Token.make ASSIGN lex.ch
    | '+' -> Token.make PLUS lex.ch
    | '-' -> Token.make MINUS lex.ch
    | '!' -> Token.make BANG lex.ch
    | '/' -> Token.make SLASH lex.ch
    | '*' -> Token.make STAR lex.ch
    | '<' -> Token.make LT lex.ch
    | '>' -> Token.make GT lex.ch
    | ';' -> Token.make SEMICOLON lex.ch
    | ',' -> Token.make COMMA lex.ch
    | '{' -> Token.make LBRACE lex.ch
    | '}' -> Token.make RBRACE lex.ch
    | '(' -> Token.make LPAREN lex.ch
    | ')' -> Token.make RPAREN lex.ch
    | c when Char.to_int c = 0 -> Token.eof ()
    | c when is_digit c -> { token = INT; literal = read_number lex }
    | c when is_letter c -> tokenize_ident lex
    | c -> Token.illegal c
  in
  read_char lex;
  token
;;

let tokenize str : Token.t list =
  let lex = make str in
  let rec loop tokens =
    match next_token lex with
    | { token = EOF; _ } as tok -> tok :: tokens
    | { token = ILLEGAL; _ } as tok -> tok :: tokens
    | tok -> loop (tok :: tokens)
  in
  loop [] |> List.rev
;;

let%test_unit "lex output" =
  let input = "let five = 5;" in
  let expected : Token.t list =
    [ { token = LET; literal = "let" }
    ; { token = IDENT; literal = "five" }
    ; { token = ASSIGN; literal = "=" }
    ; { token = INT; literal = "5" }
    ; { token = SEMICOLON; literal = ";" }
    ; { token = EOF; literal = "" }
    ]
  in
  [%test_eq: Token.t list] expected (tokenize input)
;;

let%test_unit "lex output 2" =
  let open Token in
  let input =
    {|
let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
|}
  in
  let expected =
    [ { token = LET; literal = "let" }
    ; { token = IDENT; literal = "five" }
    ; { token = ASSIGN; literal = "=" }
    ; { token = INT; literal = "5" }
    ; { token = SEMICOLON; literal = ";" }
    ; { token = LET; literal = "let" }
    ; { token = IDENT; literal = "ten" }
    ; { token = ASSIGN; literal = "=" }
    ; { token = INT; literal = "10" }
    ; { token = SEMICOLON; literal = ";" }
    ; { token = LET; literal = "let" }
    ; { token = IDENT; literal = "add" }
    ; { token = ASSIGN; literal = "=" }
    ; { token = FUNCTION; literal = "fn" }
    ; { token = LPAREN; literal = "(" }
    ; { token = IDENT; literal = "x" }
    ; { token = COMMA; literal = "," }
    ; { token = IDENT; literal = "y" }
    ; { token = RPAREN; literal = ")" }
    ; { token = LBRACE; literal = "{" }
    ; { token = IDENT; literal = "x" }
    ; { token = PLUS; literal = "+" }
    ; { token = IDENT; literal = "y" }
    ; { token = SEMICOLON; literal = ";" }
    ; { token = RBRACE; literal = "}" }
    ; { token = SEMICOLON; literal = ";" }
    ; { token = LET; literal = "let" }
    ; { token = IDENT; literal = "result" }
    ; { token = ASSIGN; literal = "=" }
    ; { token = IDENT; literal = "add" }
    ; { token = LPAREN; literal = "(" }
    ; { token = IDENT; literal = "five" }
    ; { token = COMMA; literal = "," }
    ; { token = IDENT; literal = "ten" }
    ; { token = RPAREN; literal = ")" }
    ; { token = SEMICOLON; literal = ";" }
    ; { token = EOF; literal = "" }
    ]
  in
  [%test_eq: Token.t list] expected (tokenize input)
;;

let%test_unit "lex output full" =
  let open Token in
  let input =
    {|
let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
	return true;
} else {
	return false;
}

10 == 10;
10 != 9;
|}
  in
  let expected : Token.t list =
    [ { token = Token.LET; literal = "let" }
    ; { token = Token.IDENT; literal = "five" }
    ; { token = Token.ASSIGN; literal = "=" }
    ; { token = Token.INT; literal = "5" }
    ; { token = Token.SEMICOLON; literal = ";" }
    ; { token = Token.LET; literal = "let" }
    ; { token = Token.IDENT; literal = "ten" }
    ; { token = Token.ASSIGN; literal = "=" }
    ; { token = Token.INT; literal = "10" }
    ; { token = Token.SEMICOLON; literal = ";" }
    ; { token = Token.LET; literal = "let" }
    ; { token = Token.IDENT; literal = "add" }
    ; { token = Token.ASSIGN; literal = "=" }
    ; { token = Token.FUNCTION; literal = "fn" }
    ; { token = Token.LPAREN; literal = "(" }
    ; { token = Token.IDENT; literal = "x" }
    ; { token = Token.COMMA; literal = "," }
    ; { token = Token.IDENT; literal = "y" }
    ; { token = Token.RPAREN; literal = ")" }
    ; { token = Token.LBRACE; literal = "{" }
    ; { token = Token.IDENT; literal = "x" }
    ; { token = Token.PLUS; literal = "+" }
    ; { token = Token.IDENT; literal = "y" }
    ; { token = Token.SEMICOLON; literal = ";" }
    ; { token = Token.RBRACE; literal = "}" }
    ; { token = Token.SEMICOLON; literal = ";" }
    ; { token = Token.LET; literal = "let" }
    ; { token = Token.IDENT; literal = "result" }
    ; { token = Token.ASSIGN; literal = "=" }
    ; { token = Token.IDENT; literal = "add" }
    ; { token = Token.LPAREN; literal = "(" }
    ; { token = Token.IDENT; literal = "five" }
    ; { token = Token.COMMA; literal = "," }
    ; { token = Token.IDENT; literal = "ten" }
    ; { token = Token.RPAREN; literal = ")" }
    ; { token = Token.SEMICOLON; literal = ";" }
    ; { token = Token.BANG; literal = "!" }
    ; { token = Token.MINUS; literal = "-" }
    ; { token = Token.SLASH; literal = "/" }
    ; { token = Token.STAR; literal = "*" }
    ; { token = Token.INT; literal = "5" }
    ; { token = Token.SEMICOLON; literal = ";" }
    ; { token = Token.INT; literal = "5" }
    ; { token = Token.LT; literal = "<" }
    ; { token = Token.INT; literal = "10" }
    ; { token = Token.GT; literal = ">" }
    ; { token = Token.INT; literal = "5" }
    ; { token = Token.SEMICOLON; literal = ";" }
    ; { token = Token.IF; literal = "if" }
    ; { token = Token.LPAREN; literal = "(" }
    ; { token = Token.INT; literal = "5" }
    ; { token = Token.LT; literal = "<" }
    ; { token = Token.INT; literal = "10" }
    ; { token = Token.RPAREN; literal = ")" }
    ; { token = Token.LBRACE; literal = "{" }
    ; { token = Token.RETURN; literal = "return" }
    ; { token = Token.TRUE; literal = "true" }
    ; { token = Token.SEMICOLON; literal = ";" }
    ; { token = Token.RBRACE; literal = "}" }
    ; { token = Token.ELSE; literal = "else" }
    ; { token = Token.LBRACE; literal = "{" }
    ; { token = Token.RETURN; literal = "return" }
    ; { token = Token.FALSE; literal = "false" }
    ; { token = Token.SEMICOLON; literal = ";" }
    ; { token = Token.RBRACE; literal = "}" }
    ; { token = Token.INT; literal = "10" }
    ; { token = Token.EQ; literal = "==" }
    ; { token = Token.INT; literal = "10" }
    ; { token = Token.SEMICOLON; literal = ";" }
    ; { token = Token.INT; literal = "10" }
    ; { token = Token.NEQ; literal = "!=" }
    ; { token = Token.INT; literal = "9" }
    ; { token = Token.SEMICOLON; literal = ";" }
    ; { token = Token.EOF; literal = "" }
    ]
  in
  [%test_eq: Token.t list] expected (tokenize input)
;;
