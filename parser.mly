/* HERE Ocamlyacc parser for Graphene */

%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LSQUARE RSQUARE COMMA SEMI COLON DOT
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN NOT EQ NEQ LT LEQ GT GEQ AND OR
%token TILDE DIREDGE UNDIREDGE DGT
%token RETURN BREAK CONTINUE IF ELSE FOR FOREACH WHILE 
%token INT FLOAT STRING GRAPH NODE EDGE LIST VOID
%token PUSH_BACK
%token <int> LITERAL
%token <string> FLIT
%token <string> SLIT
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT
%right PUSH_BACK

%%

program:
  decls EOF { $1 }

decls:
    /* nothing */ {[], []}
  | decls vdecl { (($2 :: fst $1), snd $1) }
  | decls fdecl { (fst $1, ($2 :: snd $1)) }

fdecl:
  typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
    { {typ = $1; 
      fname = $2;
      formals = $4;
      body = List.rev $7;
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
  | EDGE  LT typ GT { Edge($3) }
  | LIST  LT typ GT { List($3) }
  | VOID   { Void }

vdecl:
    typ ID SEMI { ($1, $2) }
/* remove? | typ ID LBRACE expr COMMA expr RBRACE SEMI { NodeLit($1, $4, $6) } */
/*  | typ ID ASSIGN expr SEMI { bind($1, $2, Assign($2, $4)) } 
*/
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
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt 
                                                        { For($3, $5, $7, $9) }
/*  | FOREACH LPAREN typ ID COLON expr RPAREN stmt { Foreach($3, $4, $6, $8) }
*/
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | typ ID ASSIGN expr SEMI { Declare($1, $2, Assign($2, $4)) }
  | typ ID SEMI { Declare($1, $2, Noexpr) }
  

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
  | expr NEQ expr { Binop($1, Neq, $3) }
  | expr LT expr { Binop($1, Less, $3) }
  | expr GT expr { Binop($1, Greater, $3) }
  | expr LEQ expr { Binop($1, Leq, $3) }
  | expr GEQ expr { Binop($1, Geq, $3) }
  | expr AND expr { Binop($1, And, $3) }
  | expr OR expr { Binop($1, Or, $3) }
  | MINUS expr %prec NOT { Unop(Neg, $2) }
  | NOT expr { Unop(Not, $2) }
  | ID ASSIGN expr { Assign($1, $3) }
  | ID DOT ID ASSIGN expr { AssignField($1, $3, $5) }
  | ID LPAREN args_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }
  /* nodeA ~~ nodeB */
  | ID UNDIREDGE ID { UEdge($1, $3) }
  /* nodeA ~(5)~ nodeB */
  | ID TILDE LPAREN literal RPAREN TILDE ID { UEdgeC($1, $4, $7) }
  /* nodeA ~>> nodeB */ 
  | ID DIREDGE ID { DEdge($1, $3) }
  /* nodeA ~(5)>> nodeB */
  | ID TILDE LPAREN literal RPAREN DGT ID { DEdgeC($1, $4, $7) }
  /* node.id */ 
  | ID DOT ID { Access($1, $3) } 
  /* graph.getNode(key) 
  | ID DOT ID LPAREN args_opt RPAREN { Call(Access($1, $3), $5) }*/
  /* queue[3] */
  | ID LSQUARE args_opt RSQUARE { Call("list_index", Id($1) :: $3) }
  /* [1,2,3,4,5] 
  Why?  | LSQUARE args_opt RSQUARE { ListLit($2) } */
  | ID DOT ID LPAREN args_opt RPAREN { Call( $3, Id($1) :: $5 ) }

literal:
    ID { Id($1) }
  | LITERAL { Ilit($1) }
  | FLIT { Flit($1) }
  | SLIT { Slit($1) }

args_opt:
    /* nothing */ { [] }
  | args_list { List.rev $1 }

args_list:
    expr { [$1] }
  | args_list COMMA expr { $3 :: $1 }
