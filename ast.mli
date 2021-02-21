type unop =
    Not 
  | Negative

type binop =
    Add
  | Sub
  | Multiply
  | Divide
  | Equals
  | And
  | Or
  | Greater
  | Less
  | GreaterEqual
  | LessEqual

type ptype = 
    Int
  | String
  | Bool
  | Float
  | Void
  
type wtype =
    Graph of ptype * ptype
  | Node of ptype
  | List of ptype
  | Tuple of ptype list

type provtype =
    Primitive of ptype
  | Wrapper of wtype

type expr =
    IntLit of int
  | StringLit of string
  | BoolLit of bool
  | FloatLit of float
(*  | NodeLit of expr
  | GraphLit of ?
  | ListLit of expr list
  ...
  *) 
  | Unop of expr * expr
  | Binop of expr * op * expr
  | Id of string
  | Assign of string * expr
  | Call of string * expr list
  | Access of string 

type stmnt = 
    Expression of expr
  | Block of stmnt list
  | Return of expr
  | If of expr * stmnt * stmnt
  | For of expr * expr * expr * stmnt
(*  | Foreach of variable * ? * block *)
  | While of expr * stmnt

type vardec = 
    string * ptype
  | string * wtype

type functiondec = {
    name : string;
    formals : variable list;
    body : stmnt;
    returntype : provtype
}


