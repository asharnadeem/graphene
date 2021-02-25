/* Ocamlyacc parser for Graphene */
/* TODO: rules for accessing methods, are we going to support objects?
         graph operators */

%{ open Ast %}

%token SEMI COLON LPAREN RPAREN LBRACE RBRACE LSQUARE RSQUARE COMMA PLUS MINUS TIMES DIVIDE MOD ASSIGN DOT
%token NOT EQ NEQ LT LEQ GT GEQ AND OR TILDE DIREDGE UNDIREDGE
%token RETURN BREAK CONTINUE IF ELSE FOR FOREACH WHILE INT FLOAT STRING GRAPH NODE LIST TUPLE VOID
%token <int> LITERAL
%token <float> FLIT
%token <string> SLIT
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%nonassoc ELSE
%nonassoc NOELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT

%%

program:
  decls EOF { $1 }

decls:
    /* nothing */ {[], []}
  | decls vdecl { (($2 :: fst $1), snd $1) }
  | decls fdecl { (fst $1, ($2 :: snd $1)) }

fdecl:
  typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
    { {typ = $1; 
      fname = $2;
      formals = $4;
      locals = $7;
      body = $8;
    } }

formals_opt:
    /* nothing */ { [] }
  | formal_list { $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1}

typ: 
    INT    { Int }
  | FLOAT  { Float }
  | STRING { String }
  | GRAPH LT typ GT  { Graph($3) }
  | NODE  LT typ GT { Node($3) }
  | LIST  LT typ GT { List($3) }
  | TUPLE LT GT { Tuple }
  | VOID   { Void }

vdecl_list:
    /* nothing */  { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
    typ ID SEMI { ($1, $2) }
    | typ ID LBRACE expr COMMA expr RBRACE SEMI { NodeLit($1, $4, $6) }

stmt_list:
    /* nothing */   { [] }
  | stmt_list stmt  { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | RETURN expr_opt SEMI { Return $2 }
  | CONTINUE SEMI { Continue }
  | BREAK SEMI { Break }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt { For($3, $5, $7, $9) }
  | FOREACH LPAREN typ ID COLON expr RPAREN stmt { For($3, $4, $6, $8) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr { $1 }

expr: 
    literal { $1 }
  | expr PLUS expr { Binop($1, Add, $3) }
  | expr MINUS expr { Binop($1, Sub, $3) }
  | expr TIMES expr { Binop($1, Mul, $3) }
  | expr DIVIDE expr { Binop($1, Div, $3) }
  | expr MOD expr { Binop($1, Mod, $3) }
  | expr EQ expr { Binop($1, Eq, $3) }
  | expr LT expr { Binop($1, Less, $3) }
  | expr GT expr { Binop($1, Greater, $3) }
  | expr LEQ expr { Binop($1, Leq, $3) }
  | expr GEQ expr { Binop($1, Geq, $3) }
  | expr AND expr { Binop($1, And, $3) }
  | expr OR expr { Binop($1, Or, $3) }
  | MINUS expr %prec NOT { Unop(Neg, $2) }
  | NOT expr { Unop(Not, $2) }
  | ID ASSIGN expr { Assign($1, $3) }
  | ID LPAREN args_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }
  | ID UNDIREDGE ID { UndirEdge($1, $3) }
  | ID TILDE LPAREN literal RPAREN TILDE ID { UndirEdgeCustom($1, $4, $7) }
  | ID DIREDGE ID { DirEdge($1, $3) }
  | ID TILDE LPAREN literal RPAREN DIREDGE ID { DirEdgeCustom($1, $4, $7) }
  | ID DOT ID LPAREN args_opt RPAREN { Method($1, $3) }
  | ID LSQUARE expr RSQUARE { Index($1, $3) }
  | LSQUARE args_opt RSQUARE { ArrayLit($2) }
  /* | LPAREN edge_list RPAREN { GraphLit($2) } Design Choice: If we need this, we can add it back*/

literal:
    ID { Id($1) }
  | LITERAL { Literal($1) }
  | FLIT { Float($1) }
  | SLIT { String($1) }

args_opt:
    /* nothing */ { [] }
  | args_list COMMA expr { $3 :: $1 }

args_list:
    expr { [$1] }
  | args_list COMMA expr { $3 :: $1 }

/* Left out due to design choice
edge_list:
    edge_lit { [$1] }
  | edge_list COMMA edge_lit { $3 :: $1 }

edge_lit:
  LBRACE expr COMMA expr RBRACE { EdgeLit($2, $4) } */
