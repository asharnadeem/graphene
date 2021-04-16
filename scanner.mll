{ open Parser }

let digit = ['0' - '9']
let digits = digit+

rule token = parse
  | eof                   { EOF }
(* ignored *)
  | [' ' '\t' '\r' '\n']  { token lexbuf }
  | "//"                  { linecomment lexbuf }
  | "/*"                  { longcomment lexbuf }
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
  | '%'                   { MOD }
  | "=="                  { EQ }
  | '.'                   { DOT }
  | '<'                   { LT }
  | '>'                   { GT }
  | "<="                  { LEQ }
  | ">="                  { GEQ }
  | "!="                  { NEQ }
  | "&&"                  { AND }
  | "||"                  { OR }
  | "!"                   { NOT }
(* graph tokens *)
  | "~>>"                 { DIREDGE }
  | ">>"                  { DGT }
  | "~~"                  { UNDIREDGE }
  | "~"                   { TILDE }

(* keywords *)
  | "void"                { VOID }
  | "int"                 { INT }
  | "float"               { FLOAT }
  | "string"              { STRING }
  | "graph"               { GRAPH }
  | "node"                { NODE }
  | "edge"                { EDGE }
  | "list"                { LIST }
  | "if"                  { IF }
  | "else"                { ELSE }
  | "for"                 { FOR }
  | "foreach"             { FOREACH }
  | "while"               { WHILE }
  | "return"              { RETURN }
  | "continue"            { CONTINUE }
  | "break"               { BREAK }

(* list functions
  |"push_back"           { PUSH_BACK }*)
(* literals *)
  | '\"' ([^'\"']* as str) '\"' { SLIT(str) }
  | digits as num { LITERAL(int_of_string num) }
  | (('0'|['1'-'9']['0'-'9']*) '.' ['0'-'9']+) as num { FLIT(num) }
  
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as str { ID(str) }

  and linecomment = parse
    '\n'                  { token lexbuf }
  | _                     { linecomment lexbuf }

  and longcomment = parse
    "*/"                  { token lexbuf }
  | _                     { longcomment lexbuf }
