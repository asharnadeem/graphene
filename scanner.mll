{ open Parser }

let digit = ['0' - '9']
let digits = digit+

rule token = parse
(* ignored *)
  [' ' '\t' '\r' '\n']  { token lexbuf }
  | "//"                  { linecomment lexbuf }
  | "/*"                  { multicomment lexbuf }
(* blocking *)  
  | '('                   { LPAREN }
  | ')'                   { RPAREN }
  | '{'                   { LBRACE }
  | '}'                   { RBRACE }
  | '['                   { LSQUARE }
  | ']'                   { RSQUARE }
  | ';'                   { SEMI }
  | ':'                   { COLON }
  | ','                   { COMMA }
(* operators *)
  | '+'                   { PLUS }
  | '-'                   { MINUS }
  | '*'                   { TIMES }
  | '/'                   { DIVIDE }
  | '='                   { ASSIGN }
  | "=="                  { EQ }
  | "!="                  { NEQ }
  | '.'                   { DOT }
(* potential issue with using these (<, >) for node/graph/lists/tuples? *)
  | '<'                   { LT }
  | '>'                   { GT }
  | "<="                  { LEQ }
  | ">="                  { GEQ }
  | "&&"                  { AND }
  | "||"                  { OR }
  | "!"                   { NOT }
(* graph tokens *)
  | "~>>"                 { DIR1 }
  | "~~"                  { UNDIR1 }
(* keywords *)
  | "void"                { VOID }
  | "int"                 { INT }
  | "float"               { FLOAT }
  | "bool"                { BOOL }
  | "string"              { STRING }
  | "graph"               { GRAPH }
  | "node"                { NODE }
  | "list"                { LIST }
  | "tuple"               { TUPLE }
  | "if"                  { IF }
  | "else"                { ELSE }
  | "for"                 { FOR }
  | "while"               { WHILE }
  | "return"              { RETURN }
  | "continue"            { CONTINUE }
  | "break"               { BREAK }
(* literals *)
  | "true"                { BLIT(true) }
  | "false"               { BLIT(false) }
  | '\"' ([^'\"']* as str) '\"' { SLIT(str) }
  | digits as lxm { LITERAL(int_of_string lxm) }
  | (('0'|['1'-'9']['0'-'9']*) '.' ['0'-'9']+) as num { DLIT(num) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }

  and linecomment = parse
    '\n'                  { token lexbuf }
  | _                     { linecomment lexbuf }

  and longcomment = parse
    "*/"                  { token lexbuf }
  | _                     { longcomment lexbuf }