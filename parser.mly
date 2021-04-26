/* Ocamlyacc parser for Graphene 
  Author: Matthew Sanchez */

%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LSQUARE RSQUARE COMMA SEMI COLON DOT
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN NOT EQ NEQ LT LEQ GT GEQ AND OR
%token DEDGE UEDGE 
%token RETURN IF ELSE FOR WHILE PRINT
%token INT FLOAT STRING GRAPH NODE EDGE LIST VOID 
%token PUSHBACK PUSHFRONT POPBACK POPFRONT PEEKBACK PEEKFRONT 
%token CONTAINS CONTAINSID ADDALL ADDNODE ADD 
%token <int> LITERAL
%token <string> FLIT
%token <string> SLIT
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE 
%right ASSIGN LSQUARE RSQUARE
%right DEDGE UEDGE 
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT
%right DOT

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
      formals = List.rev $4;
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
  | NODE  LT typ GT { Node($3) }
  | EDGE  LT typ GT { Edge($3) }
  | LIST  LT typ GT { List($3) }
  | GRAPH LT typ GT  { Graph($3) }
  | VOID   { Void }

vdecl:
    typ ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */   { [] }
  | stmt_list stmt  { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | RETURN expr_opt SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt 
                                                        { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | typ ID ASSIGN expr SEMI { Declare($1, [$2], Assign($2, $4)) }
  | typ id_list SEMI { Declare($1, $2, Noexpr) }

id_list:
    ID                { [ $1 ] }
  | id_list COMMA ID  { $3 :: $1 }
  

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
  | expr DOT ID ASSIGN expr { AssignField($1, $3, $5) }
  | LPAREN expr RPAREN { $2 }
  /* nodeA -> nodeB */
  | expr DEDGE expr { DEdge( $1, $3) }
  /* nodeA ->[1] nodeB */
  | expr DEDGE LSQUARE expr RSQUARE expr { DEdgeC( $1, $4, $6) }
  /* nodeA <-> nodeB */
  | expr UEDGE expr { UEdge ( $1, $3) }
  /* nodeA <-> nodeB */
  | expr UEDGE LSQUARE expr RSQUARE expr { UEdgeC( $1, $4, $6) }
  /* node.id */ 
  | expr DOT ID { Access($1, $3) } 
  /* list[3] */
  | expr LSQUARE expr RSQUARE { Index($1, $3) }
  | ID LPAREN args_opt RPAREN { Call($1, $3) }
  /* built-in "functions" */
  | PRINT LPAREN expr RPAREN { Print($3) }
  | expr DOT ID LPAREN args_opt RPAREN { Call( $3, $1 :: $5 ) }
  | expr DOT PUSHBACK LPAREN expr RPAREN { PushBack($1, $5) }
  | expr DOT PUSHFRONT LPAREN expr RPAREN { PushFront($1, $5) }
  | expr DOT POPBACK LPAREN RPAREN { PopBack($1) }
  | expr DOT POPFRONT LPAREN RPAREN { PopFront($1) }
  | expr DOT PEEKBACK LPAREN RPAREN { PeekBack($1) }
  | expr DOT PEEKFRONT LPAREN RPAREN { PeekFront($1) }
  | expr DOT ADDNODE LPAREN expr RPAREN { AddNode($1, $5) }
  | expr DOT ADD LPAREN expr COMMA expr RPAREN { GAdd($1, $5, $7) }
  | expr DOT CONTAINS LPAREN expr RPAREN { Contains($1, $5) }
  | expr DOT CONTAINSID LPAREN expr RPAREN { ContainsId($1, $5) }
  | expr DOT ADDALL LPAREN args_list RPAREN { AddAll( $1, List.rev $5 )}

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
