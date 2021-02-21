{ open Parser }

rule token = parse
(* ignored *)
  [' ' '\t' '\r' '\n']  { token lexbuf }
  | "//"                  { linecomment lexbuf }
  | "/*"                  { multicomment lexbuf }
(* blocking *)  
  | '('                   { OPENPAREN }
  | ')'                   { CLOSEPAREN }
  | '{'                   { OPENBRACE }
  | '}'                   { CLOSEBRACE }
  | '['                   { OPENSQUARE }
  | ']'                   { CLOSESQAURE }
  | ';'                   { SEMICOLON }
  | ':'                   { COLON }
  | ','                   { COMMA }
(* operators *)
  | '='                   { ASSIGN }
  | '!'                   { NEGATE }
  | '+'                   { PLUS }
  | '-'                   { MINUS }
  | '*'                   { TIMES }
  | '/'                   { DIVIDE }
  | "=="                  { EQUALS }
  | '.'                   { DOT }
  (* potential issue with using these (<, >) for node/graph/lists/tuples? *)
  | '<'                   { OPENANGLE }
  | '>'                   { CLOSEANGLE }
  | "<="                  { LESSEQ }
  | ">="                  { GREATEREQ }
  | "||"                  { OR }
  | "&&"                  { AND }
  | '~'                   { TILDE }
  | ">>"                  { DIRECTION }
(* keywords *)
  | "void"                { VOID }
  | "int"                 { INT }
  | "double"              { DOUBLE }
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
  | "true"                { BOOLLIT(true) }
  | "false"               { BOOLLIT(false) }
  | '\"' ([^'\"']* as str) '\"' { STRINGLIT(str) }
  | ('0'|['1'-'9']['0'-'9']*) as num  { INTLIT(num) }
  | (('0'|['1'-'9']['0'-'9']*) '.' ['0'-'9']+) as num { DOUBLELIT(num) }
  (* this is assuming variable names must start with lowercase *)
  | (['a'-'z']['a'-'z' 'A'-'Z' '_']*) as name { ID(name) } 

  and linecomment = parse
    '\n'                  { token lexbuf }
  | _                     { linecomment lexbuf }

  and longcomment = parse
    "*/"                  { token lexbuf }
  | _                     { longcomment lexbuf }
