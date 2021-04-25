(* Semantic checking *)

open Ast
open Sast

module StringMap = Map.Make(String) 

let valid_element_type = function
       	(Void,_) -> raise(Failure("error: list of type void not allowed"))
	| _ -> ()
  
let check_type (ex, ty) = 
	if ex = ty then () 
	else raise (Failure ("error: "))

let get_type(t, _) = t
let first_element (myList) = match myList with
 [] -> Void
| first_e1 :: _ -> get_type(first_e1)

(* Check global variables, then each function *)
let check (globals, functions) =

  (* Check global variables *)
  (* No void bindings or duplicate names *)
  let check_binds (kind : string) (binds: bind list) = 
    List.iter (function
        (Void, b) -> raise (Failure ("error: illegal void " ^ kind ^
        " " ^ b))
      | _ -> ()) binds;
  
    let rec dups = function
        [] -> ()
      | ((_,n1) :: (_,n2) :: _) when n1 = n2 -> raise(Failure 
          ("error: variable previously declared: " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in check_binds "global" globals;

  (* built-in decls go here (none at the moment) *)
  let built_in_decls = 
    let add_bind map (ft, name, t) = StringMap.add name {
      typ = ft;
      fname = name;
      formals = [t];
      body = [] } map
      in List.fold_left add_bind StringMap.empty [   ]
    
  
  in
  (* adds unique function names to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "error: function previously declared" ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname
    in match fd with
        _ when StringMap.mem n built_in_decls -> make_err built_in_err
      | _ when StringMap.mem n map -> make_err dup_err
      | _ -> StringMap.add n fd map
  in
  (* Collects built-in and declared functions *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in 

  (* Finds a function in the table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("error: function never declared: " ^ s))
  in
  
  (* Require main function for all programs *) 
  let _ = find_func "main" in 
  
  let check_function func = 
    (* No duplicates in formals *)
    check_binds "formal" func.formals;
    
    let check_assign lvaluet rvaluet err = 
      if lvaluet = rvaluet then lvaluet else raise (Failure (err ^ ""))
    in

    (* Since we aren't using locals, find declarations in fcn *)
    let rec concat_statements locals = function
        [] -> locals
      | h::t -> concat_statements (match h with
                  Declare(t, x, Noexpr) -> 
                    List.fold_left (fun l s -> (t, s)::l) locals x
                | Declare(t, ([x] as l), _) when List.length l = 1
                    -> (t, x) :: locals
                    (* Declare(t, x, e) -> ignore e; (t, x) :: locals *)
                | _ -> locals) t
    in
    let fulldecls = globals @ func.formals @ concat_statements [] func.body 
    in  (List.iter (fun (t, _) -> match t with 
                  Node(it) -> (match it with
                      List(_) | Node(_) | Graph(_) | Edge(_) | Void ->
                        raise (Failure ("error: nodes cannot wrap "
                              ^ string_of_typ it))
                    | _ -> ())
                | Edge(it) -> (match it with
                      List(_) | Node(_) | Graph(_) | Edge(_) | Void ->
                        raise (Failure ("error: edges cannot wrap "
                              ^ string_of_typ it))
                    | _ -> ())
                | Graph(it) -> (match it with
                      List(_) | Node(_) | Edge(_) | Graph(_) | Void ->
                        raise (Failure ("error: graphs cannot wrap "
                              ^ string_of_typ it))
                    | _ -> ())
                | List(Void) -> raise (Failure 
                  "error: lists cannot wrap void")
                | _ -> ())
                   fulldecls);
    (* Local symbol table for function *)
    let symbols = List.fold_left (fun m (t, x) -> StringMap.add x t m) 
      StringMap.empty (fulldecls)
    in

    (* Gets symbols from table *)
    let type_of_identifier s = 
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("error: variable never declared: " ^ s))
    in 

    (* Checks expressions *)
    let rec expr = 
      function
        Ilit l -> (Int, SIlit l)
      | Slit l -> (String, SSlit l)
      | Flit l -> (Float, SFlit l)
      | Id s -> (type_of_identifier s, SId s) 
      | Unop(o, e) as ex -> 
          let (t, e') = expr e in
          let ty = match o with 
            Neg when t = Int || t = Float -> t
          | Not when t = Int -> Int
          | _ -> raise (Failure ("error: illegal unary operator " ^
                                 string_of_unop o ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(o, (t, e')))
      | Binop(e1, o, e2) as ex ->
          let (t1, e1') = expr e1 and (t2, e2') = expr e2 in
          let same = t1 = t2 in
          let ty = match o with
            Add | Sub | Mul | Div | Mod when same && t1 = Int -> Int
          | Add | Sub | Mul | Div | Mod when same && t1 = Float -> Float
          | Eq                  when same && t1 = Int -> Int
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int || t1 = Float) -> Int
          | And | Or when same && t1 = Int -> Int
          | _ -> raise (Failure("error: illegal binary operator" ^ 
                        string_of_typ t1 ^ " " ^ string_of_binop o ^ " " ^
                        string_of_typ t2 ^ " in " ^ string_of_expr ex))
          in (ty, SBinop((t1,e1'), o, (t2, e2')))
      | Assign(x, e) as ex -> 
          let lt = type_of_identifier x
          and (rt, e') = expr e in
          let err = "error: illegal HERE assignment " ^ string_of_typ lt ^ " = " ^
                    string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign(x, (rt, e')))
      | AssignField(x, s, e) as ex -> let sx = expr x in 
            let (lt, it) = (match sx with
              (Node(t), _) -> t, List(Int)
            | _ -> raise (Failure ("error: cannot access this type")))
          and (rt, e') = expr e in
          let err = "error1111: illegal assignment " ^ string_of_typ lt ^ " = "
                  ^ string_of_typ rt ^ " in " ^ string_of_expr ex
          in (match s with 
              "val" -> 
                (check_assign lt rt err, SAssignField(sx, s, (rt, e')))
            | "id" ->
                (check_assign Int rt err, SAssignField(sx, s, (rt, e')))
            | "edges" -> 
                (check_assign it rt err, SAssignField(sx, s, (rt, e')))
            | _ -> raise (Failure (err)))
      | Call(f, el) as call -> 
          let fd = find_func f in
          let param_length = List.length fd.formals in
          if List.length el != param_length then
            raise (Failure ("error: expecting " ^ string_of_int param_length ^
                           " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e =
            let (et, e') = expr e in
            let err = "error: illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in  
          let args = List.map2 check_call fd.formals el
          in (fd.typ, SCall(f, args))
      | Print(e) -> let e' = expr e in (match e' with
            (Void, _) -> raise (Failure "print used on void")
          | _ -> (Void, SPrint(e')))
      | Access(x, s) -> let sx = expr x in (match sx with
            (Node(tn), _) -> (match s with 
                "val" -> (tn, SAccess(sx, s))
              | "id"  -> (Int, SAccess(sx, s))
              | "edges" -> (List(Edge(tn)), SAccess(sx, s))
              | _ -> raise (Failure ("error: invalid node field " ^ s)))
          | (List(_), _) -> (match s with 
                "size" -> (Int, SAccess(sx, s))
              | _ -> raise (Failure ("error: invalid list field " ^ s)))
          | (Graph(tg), _) -> (match s with 
                "size" -> (Int, SAccess(sx, s))
              | "nodes" -> (List(Node(tg)), SAccess(sx, s))
              | "root" -> (Node(tg), SAccess(sx, s))
              | _ -> raise (Failure ("error: invalid graph field " ^ s)))
          | (Edge(te), _) -> (match s with 
                "weight" -> (te, SAccess(sx, s))
              | "to" -> (Node(te), SAccess(sx, s))
              | "t" -> (Int, SAccess(sx, s))
              | _ -> raise (Failure ("error: invalid edge field " ^ s)))
          | _ -> raise 
              (Failure ("error: this type does not have this field: " ^ s)))
      | Noexpr -> (Void, SNoexpr)
      | UEdge(n1, n2) -> let sn1 = expr n1 and
                             sn2 = expr n2 in (match (sn1, sn2) with 
            ((Node(a), _),  (Node(b), _)) when a = b -> 
                    (Node(a), SUEdge(sn1, sn2))
          | _ -> raise (Failure ("error: UEdge fail")))

     | UEdgeC(n1, e, n2) -> let sn1 = expr n1 and
                                sn2 = expr n2 in (match (sn1, sn2) with
            ((Node(a), _),  (Node(b), _)) when a = b -> 
                    (Node(a), SUEdgeC(sn1, expr e, sn2))
          | _ -> raise (Failure ("error: UEdgeC fail")))

      | DEdge(n1, n2) -> let sn1 = expr n1 and
                             sn2 = expr n2 in (match (sn1, sn2) with
            ((Node(a), _),  (Node(b), _)) when a = b -> 
                    (Node(a), SDEdge(sn1, sn2))
          | _ -> raise (Failure ("error: DEdge fail")))

      | DEdgeC(n1, e, n2) -> let sn1 = expr n1 and
                                  sn2 = expr n2 in (match (sn1, sn2) with
            ((Node(a), _),  (Node(b), _)) when a = b -> 
              (Node(a), SDEdgeC(sn1, expr e, sn2))
          | _ -> raise (Failure ("error: DEdgeC fail")))
      | Index(l, i) -> let l' = expr l and 
                               i' = expr i in (match (l', i') with
            ((List(t),_), (Int, _)) -> (t, SIndex(l', i'))
          | ((Graph(t),_), (Int, _)) -> (Node(t), SIndex(l', i'))
          | (_, (Int,_)) -> raise (Failure 
          ("error: cannot index non-list/graph " ^ string_of_expr l))
          | ((List(_),_), _) -> raise (Failure 
          ("error: cannot index with non-int " ^ string_of_expr i))
          | _ -> raise (Failure ("error: invalid index of " 
          ^ string_of_expr l ^ " with " ^ string_of_expr i)))    
      | PushBack(l, e) -> let l' = expr l 
                        and e' = expr e in (match (l', e') with
          ((List(tl),_), (te, _)) when tl = te -> 
                                  (List(tl), SPushBack(l', e'))
        | ((List(tl),_), _) -> raise (Failure ("error: cannot push " 
        ^ string_of_expr e ^ " to list of type " ^ string_of_typ tl))
        | _ -> raise (Failure ("error: push_back on non-list " 
          ^ string_of_expr e)))
      | PushFront(l, e) -> let l' = expr l 
                          and e' = expr e in (match (l', e') with
          ((List(tl),_), (te, _)) when tl = te -> 
                                  (List(tl), SPushFront(l', e'))
        | ((List(tl),_), _) -> raise (Failure ("error: cannot push " 
        ^ string_of_expr e ^ " to list of type " ^ string_of_typ tl))
        | _ -> raise (Failure ("error: push_back on non-list " 
        ^ string_of_expr e)))
      | PopBack(l) -> let l' = expr l in (match l' with
            (List(tl),_) -> (tl, SPopBack(l'))
          | _ -> raise (Failure ("error: pop_back on non-list " 
          ^ string_of_expr l)))
      | PopFront(l) -> let l' = expr l in (match l' with
            (List(tl),_) -> (tl, SPopFront(l'))
          | _ -> raise (Failure ("error: pop_front on non-list " 
          ^ string_of_expr l)))
      | AddNode(g, e) -> let g' = expr g  and e' = expr e 
        in (match (g', e') with 
          ((Graph(tg), _), (Node(tn), _)) ->  
            if tg = tn then  
            (Void, SAddNode(g', e'))
            else raise (Failure ("error: graph and node types do not match,"
            ^ string_of_typ tg ^ " vs " ^ string_of_typ tn))
        | ((Graph(_), _), _) -> 
            raise (Failure "error: cannot add non-node to graph")
        | (_, (Node(_), _)) ->
            raise (Failure "error: cannot add to non-graph")
        | (_, _) -> raise (Failure "error: misuse of add_node"))
      
                          

    in

    let check_bool_expr e =
      let (t', e') = expr e
      and err = "error: expected int expression in " ^ string_of_expr e
      in if t' != Int then raise (Failure err) else (t', e')
    in 

    (* Check statements *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | Block sl -> 
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _ -> raise (Failure ("Nothing may follow a return"))
            | Block sl :: ss -> check_stmt_list (sl @ ss)
            | s :: ss -> check_stmt s :: check_stmt_list ss
            | [] -> []
          in SBlock(check_stmt_list sl)
      | Return e -> let (t, e') = expr e in
          if t = func.typ then SReturn (t, e')
          else raise (Failure 
              ("error: expected return to be of type: " ^ string_of_typ t))
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, s) -> 
          SFor(expr e1, expr e2, expr e3, check_stmt s)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Continue -> SContinue
      | Break -> SBreak
      | Declare(t, x, Noexpr) -> SDeclare(t, x, (Void, SNoexpr))
      | Declare(t, l, e) as d when List.length l = 1 
          -> let (t', v) = expr e in
          ignore v;
          if t' = t then SDeclare(t, l, (t', v))
          else raise (Failure ("error: assignment does not match value in " ^ 
                string_of_stmt d ))
      | Declare(_) -> raise (Failure ("error: invalid declaration"))
    in
    (* check_function finally complete *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = func.formals;
      sbody = match check_stmt (Block func.body) with
        SBlock(sl) -> sl
        | _ -> raise (Failure ("internal error"))
    }
  in (globals, List.map check_function functions) 
