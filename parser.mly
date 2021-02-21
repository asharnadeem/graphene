/* Ocamlyacc parser for Graphene */

%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE LSQUARE RSQUARE COMMA PLUS MINUS TIMES DIVIDE ASSIGN
%token NOT EQ NEQ LT LEQ GT GEQ AND OR
%token RETURN IF ELSE FOR WHILE INT BOOL FLOAT STRING GRAPH NODE LIST TUPLE VOID
%token <int> LITERAL
%token <float> FLIT
%token <bool> BLIT
%token <string> ID
%token EOF

%start expr
%type <Ast.program> expr

%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT

%%

expr:
  expr PLUS   expr      { Binop($1, Add, $3) }
| expr MINUS  expr      { Binop($1, Sub, $3) }
| expr TIMES  expr      { Binop($1, Mul, $3) }
| expr DIVIDE expr      { Binop($1, Div, $3) }
