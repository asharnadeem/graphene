open Ast

type sexpr = typ * sx
and sx =
    SIlit of int
  | SSlit of string
  | SFlit of string
  | SId of string
  | SListLit of sexpr list 
  | SUnop of unop * sexpr
  | SBinop of sexpr * binop * sexpr
  | SAssign of string * sexpr
  | SAssignField of string * string * sexpr
  | SCall of string * sexpr list
  | SAccess of string * string
  | SIndex of sexpr * sexpr
  | SNoexpr
  | SUEdge of string * string
  | SUEdgeC of string * sexpr * string
  | SDEdge of string * string
  | SDEdgeC of string * sexpr * string
  | SList_Push_Back of sexpr * sexpr

type sstmt = 
    SExpr of sexpr
  | SBlock of sstmt list
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SContinue
  | SBreak 
  | SDeclare of typ * string * sexpr
  
type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    sbody : sstmt list;
  }

type sprogram = bind list * sfunc_decl list

(* Pretty-printing *)

let rec string_of_sexpr (t, e) = 
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SIlit(l) -> string_of_int l
  | SSlit(l) -> "\"" ^ l ^ "\""
  | SFlit(l) -> l
  | SId(s) -> s
  | SListLit(l) -> "[" ^ String.concat "," (List.map string_of_sexpr l) ^ "]"
  | SUnop(o, e) -> string_of_unop o ^ string_of_sexpr e
  | SBinop(e1, o, e2) -> 
      string_of_sexpr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_sexpr e2
  | SAssign(s, e) -> s ^ " = " ^ string_of_sexpr e
  | SAssignField(x, s, e) -> x ^ "." ^ s ^ " = " ^ string_of_sexpr e
  | SCall(f, el) -> 
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SAccess(x, s) -> x ^ "." ^ s
  | SIndex(x, e) -> string_of_sexpr x ^ "[" ^ string_of_sexpr e ^ "]"
  | SList_Push_Back(l, e) -> string_of_sexpr l ^ ".push_back(" ^ string_of_sexpr e ^ ")"
  | SNoexpr -> ""
  | SUEdge(n1, n2) -> n1 ^ " ~~ " ^ n2
  | SUEdgeC(n1, e, n2) -> n1 ^ " ~(" ^ string_of_sexpr e ^ ")~ " ^ n2
  | SDEdge(n1, n2) -> n1 ^ " ~> " ^ n2
  | SDEdgeC(n1, e, n2) -> n1 ^ " ~(" ^ string_of_sexpr e ^ ")>> " ^ n2
  ) ^ ")"
      
let rec string_of_sstmt = function
    SExpr(se) -> string_of_sexpr se ^ ";\n"
  | SBlock(ssl) -> 
    "{\n" ^ String.concat "" (List.map string_of_sstmt ssl) ^ "}\n"
  | SReturn(se) -> "return " ^ string_of_sexpr se ^ ";\n"
  | SIf(se, ss, SBlock([])) -> 
      "if (" ^ string_of_sexpr se ^ ")\n" ^ string_of_sstmt ss
  | SIf(se, ss1, ss2) -> "if (" ^ string_of_sexpr se ^ ")\n" ^ 
      string_of_sstmt ss1 ^ "else\n" ^ string_of_sstmt ss2
  | SFor(se1, se2, se3, ss) -> 
      "for (" ^ string_of_sexpr se1  ^ " ; " ^ string_of_sexpr se2 ^ " ; " ^
      string_of_sexpr se3  ^ ") " ^ string_of_sstmt ss
  | SWhile(se, ss) -> "while (" ^ string_of_sexpr se ^ ") " ^ 
      string_of_sstmt ss
  | SContinue -> "continue;"
  | SBreak -> "break;"
  | SDeclare(t, x, se) -> string_of_typ t ^ " " ^ x ^ "; " ^ 
      string_of_sexpr se ^ ";\n"

let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^ 
  fdecl.sfname ^ "(" ^ 
  String.concat ", " (List.map snd fdecl.sformals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^ "}\n"

let string_of_sprogram (vars, funcs) = 
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)

