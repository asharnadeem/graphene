open Ast

type sexpr = typ * sx
and sx =
    SIlit of int
  | SSlit of string
  | SFlit of string
  | SId of string
  | SUnop of unop * sexpr
  | SBinop of sexpr * binop * sexpr
  | SAssign of string * sexpr
  | SAssignField of sexpr * string * sexpr
  | SCall of string * sexpr list
  | SPrint of sexpr
  | SAccess of sexpr * string
  | SNoexpr
  | SUEdge of sexpr * sexpr
  | SUEdgeC of sexpr * sexpr * sexpr
  | SDEdge of sexpr * sexpr
  | SDEdgeC of sexpr * sexpr * sexpr
  | SIndex of sexpr * sexpr
  | SPushBack of sexpr * sexpr
  | SPushFront of sexpr * sexpr
  | SPopBack of sexpr 
  | SPopFront of sexpr
  | SPeekBack of sexpr 
  | SPeekFront of sexpr
  | SAddNode of sexpr * sexpr
  | SGAdd of sexpr * sexpr * sexpr
  | SContains of sexpr * sexpr
  | SContainsId of sexpr * sexpr

type sstmt = 
    SExpr of sexpr
  | SBlock of sstmt list
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SContinue
  | SBreak 
  | SDeclare of typ * string list * sexpr
  
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
  | SUnop(o, e) -> string_of_unop o ^ string_of_sexpr e
  | SBinop(e1, o, e2) -> 
      string_of_sexpr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_sexpr e2
  | SAssign(s, e) -> s ^ " = " ^ string_of_sexpr e
  | SAssignField(x, s, e) -> string_of_sexpr x ^ "." 
        ^ s ^ " = " ^ string_of_sexpr e
  | SCall(f, el) -> 
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SPrint(e) -> "print(" ^ string_of_sexpr e ^ ")"
  | SAccess(x, s) -> string_of_sexpr x ^ "." ^ s
  | SNoexpr -> ""
  | SUEdge(n1, n2) -> string_of_sexpr n1 ^ " <-> " ^ string_of_sexpr n2
  | SUEdgeC(n1, e, n2) -> string_of_sexpr n1 ^ " <-> [" ^ string_of_sexpr e 
                          ^ "] " ^ string_of_sexpr n2
  | SDEdge(n1, n2) -> string_of_sexpr n1 ^ " -> " ^ string_of_sexpr n2
  | SDEdgeC(n1, e, n2) -> string_of_sexpr n1 ^ " ->[" ^ string_of_sexpr e 
                          ^ "] " ^ string_of_sexpr n2
  | SIndex(l, i) -> string_of_sexpr l ^ "[" ^ string_of_sexpr i ^ "]"
  | SPushBack(l, e) -> string_of_sexpr l ^ ".push_back(" 
                      ^ string_of_sexpr e ^ ")"
  | SPushFront(l, e) -> string_of_sexpr l ^ ".push_front(" 
                      ^ string_of_sexpr e ^ ")"
  | SPopBack(l) -> string_of_sexpr l ^ ".pop_back()"
  | SPopFront(l) -> string_of_sexpr l ^ ".pop_front()"
  | SPeekBack(l) -> string_of_sexpr l ^ ".peek_back()"
  | SPeekFront(l) -> string_of_sexpr l ^ ".peek_front()"
  | SAddNode(g, e) -> string_of_sexpr g ^ ".add_node(" ^ string_of_sexpr e ^")"
  | SGAdd(g, id, v) -> string_of_sexpr g ^ ".add(" 
    ^ string_of_sexpr id ^ ", " ^ string_of_sexpr v ^ ")"
  | SContains(g, n) -> 
    string_of_sexpr g ^ ".contains(" ^ string_of_sexpr n ^ ")"
  | SContainsId(g, i) -> 
    string_of_sexpr g ^ ".contains_id(" ^ string_of_sexpr i ^ ")"

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
  | SDeclare(t, lx, (_,SNoexpr)) -> string_of_typ t ^ " " ^
    (List.fold_left (fun l s  -> s ^ "; " ^ l) "" lx ) ^ "\n"
  | SDeclare(t, ([x] as l), e) when (List.length l) = 1 -> string_of_typ t ^ " " ^ x ^ "; " 
        ^ string_of_sexpr e ^ ";\n"
  | SDeclare(_) -> "PARSING ERROR\n"
  (* | SDeclare(t, x, se) -> string_of_typ t ^ " " ^ x ^ "; " ^ 
      string_of_sexpr se ^ ";\n" *)

let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^ 
  fdecl.sfname ^ "(" ^ 
  String.concat ", " (List.map snd fdecl.sformals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^ "}\n"

let string_of_sprogram (vars, funcs) = 
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)

