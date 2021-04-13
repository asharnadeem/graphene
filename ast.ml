type unop =
    Not 
  | Neg

type binop =
    Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | And
  | Or
  | Greater
  | Less
  | Geq
  | Leq
  | Neq

type typ = 
    Int
  | String
  | Float
  | Void
  | Graph of typ 
  | Edge of typ
  | Node of typ
  | List of typ

type expr =
    Ilit of int
  | Slit of string
  | Flit of string 
  | Id of string
  | ListLit of expr list 
  (* | NodeLit of expr
  | GraphLit of ?
  ...
   
  | UEdgeC of string * expr * string
  | DEdge of string * string
  | DEdgeC of string * expr * string
  *)
  | Unop of unop * expr
  | Binop of expr * binop * expr
  | Assign of string * expr
  | AssignField of string * string * expr
  | Call of string * expr list
  | Access of string * string
  (* | Index of expr * expr *)
  | Noexpr
  | UEdge of string * string
  | UEdgeC of string * expr * string
  | DEdge of string * string
  | DEdgeC of string * expr * string

type bind = typ * string

type stmt = 
    Expr of expr
  | Block of stmt list
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  (*| Foreach of typ * string * expr * stmt *)
  | While of expr * stmt
  | Continue
  | Break
  | Declare of typ * string * expr

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : stmt list;
  }

type program = bind list * func_decl list

(* Pretty-printing functions *)

let string_of_unop = function
    Neg -> "-"
  | Not -> "!"

let string_of_binop = function
    Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | And -> "&&"
  | Or -> "||"
  | Greater -> ">"
  | Less -> "<"
  | Geq -> ">="
  | Leq -> "<="
  | Neq -> "!="


let rec string_of_typ = function
    Int -> "int"
  | String -> "string"
  | Float -> "float"
  | Void -> "void"
  | Graph(t) -> "graph<" ^ string_of_typ t ^ ">"
  | Node(t) -> "node<" ^ string_of_typ t ^ ">" 
  | Edge(t) -> "edge<" ^ string_of_typ t ^ ">" 
  | List(t) -> "list<" ^ string_of_typ t ^ ">" 

let rec string_of_expr = function
    Ilit(x) -> string_of_int x
  | Slit(x) -> "\"" ^ x ^ "\""
  | Flit(x) -> x 
  | Id(x) -> x
(*  | UEdge(n1, n2) -> "(" ^ string_of_expr n1 ^ ", " ^ string_of_expr n2 ^ ")"
  | UEdgeC(n1, e, n2) -> "(" ^ string_of_expr n1 ^ ", " ^ string_of_expr e ^
    ", " ^ string_of_expr n2 ^ ")"
  | DEdge(n1, n2) -> "(" ^ string_of_expr n1 ^ " > " ^ string_of_expr n2 ^ ")"
  | DEdgeC(n1, e, n2) -> "(" ^ string_of_expr n1 ^ ", " ^ string_of_expr e ^    
    "> " ^ string_of_expr n2 ^ ")"
*)  
  | Unop(o, e) -> string_of_unop o ^ string_of_expr e
  | Binop(e1, o, e2) -> string_of_expr e1 ^ " " ^ 
    string_of_binop o ^ " " ^ string_of_expr e2
  | Assign(x, e) -> x ^ " = " ^ string_of_expr e
  | AssignField(x, s, e) -> x ^ "." ^ s ^ " = " ^ string_of_expr e
  | Call(f, el) -> 
    f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Access(x, s) -> x ^ "." ^ s
  (* | Index(x, e) -> string_of_expr x ^ "[" ^ string_of_expr e ^ "]" *)
  | Noexpr -> ""
  | UEdge(n1, n2) -> n1 ^ " ~~ " ^ n2 
  | UEdgeC(n1, e, n2) -> n1 ^ " ~(" ^ string_of_expr e ^ ")~ " ^ n2
  | DEdge(n1, n2) -> n1 ^ " ~> " ^ n2
  | DEdgeC(n1, e, n2) -> n1 ^ " ~(" ^ string_of_expr e ^ ")>> " ^ n2
  | ListLit(l) -> "[" ^ String.concat "," (List.map string_of_expr l) ^ "]" 

let rec string_of_stmt = function
    Expr(e) -> string_of_expr e ^ ";\n"
  | Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) -> "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) -> 
      "for (" ^ string_of_expr e1 ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3 ^ ")" ^ string_of_stmt s
(*  | Foreach(t, s, expr, stmt) -> 
*)
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Continue -> "continue;"
  | Break -> "break;"
  | Declare(t, x, Noexpr) -> string_of_typ t ^ " " ^ x ^ ";\n"
  | Declare(t, x, e) -> string_of_typ t ^ " " ^ x ^ "; " ^ string_of_expr e ^ ";\n"


let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl = 
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^
  String.concat ", " (List.map snd fdecl.formals) ^ ")\n{\n" ^
  String.concat ", " (List.map string_of_stmt fdecl.body) ^ "}\n"

let string_of_program (vars, funcs) = 
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)

