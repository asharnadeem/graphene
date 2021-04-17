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
      | ((_,n1) :: (_,n2) :: _) when n1 = n2 -> raise(Failure ("error: variable previously declared: " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in check_binds "global" globals;

  let check_list_binds (binds : sexpr list) =
      List.iter valid_element_type binds;

      let rec check_type = function
          [] -> ()
      | ((t1,_) :: (t2,_) :: _) when t1 != t2 ->
              raise (Failure ("error: list elements of different types"))
      | _ :: t -> check_type t
      in check_type (List.sort (fun (a,_) (b,_) -> compare a b) binds);
      
      (*first_element(binds)*)
  in 

  (* Check functions *)
  let built_in_decls_one = 
    let add_bind map (name, t) = StringMap.add name {
      typ = Void;
      fname = name;
      formals = [(t, "x")];
      body = [] } map
      in List.fold_left add_bind StringMap.empty [ 
        ("print", Int);
        ("printf", Float);
     ]

  in let built_in_decls = 
    let add_bind2 map (ft, name, t1, t2) = StringMap.add name {
      typ = ft;
      fname = name;
      formals = [(t1, "x"); (t2, "y")];
      body = [] } map
      in List.fold_left add_bind2 built_in_decls_one [ 
        (Int, "list_index", List(Int), Int;);
        (Int, "list_push_back", List(Int), Int);
        (Int, "list_push_front", List(Int), Int);
        (Void, "graph_add_node", Graph(Node(Int)), Node(Int));
     ]
    
  
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
                    Declare(t, x, e) -> ignore e; (t, x) :: locals
                  | _ -> locals) t
    in
    (* Local symbol table for function *)
    let symbols = List.fold_left (fun m (t, x) -> StringMap.add x t m) 
      StringMap.empty (globals @ func.formals @ concat_statements [] func.body)
    in

    (* Gets symbols from table *)
    let type_of_identifier s = 
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("error: variable never declared: " ^ s))
    in 

    (* Checks expressions *)
    let rec expr = 
      let check_list m =
          let (t, _) = expr m in
          match t with
          List(_) -> ()
          |_ -> raise (Failure ("error: expected different list type: " ^ string_of_typ t)) in
      let check_list_type m =
				let (t, _) = expr m in
				match t with
				 List(ty) -> ty
				|_ -> raise (Failure ("error: " ^ string_of_typ t)) in
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
          (* Might be easier just to add Bools... *)
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
          let err = "error: illegal assignment " ^ string_of_typ lt ^ " = " ^
                    string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign(x, (rt, e')))
      | AssignField(x, s, e) as ex -> let (lt, it) = (match type_of_identifier x with
              Node(t) -> t, List(Int)
            | _ -> raise (Failure ("error: cannot access this type")))
          and (rt, e') = expr e in
          let err = "error: illegal assignment " ^ string_of_typ lt ^ " = "
                  ^ string_of_typ rt ^ " in " ^ string_of_expr ex
          in (match s with 
              "val" -> 
                (check_assign lt rt err, SAssignField(x, s, (rt, e')))
            | "id" ->
                (check_assign Int rt err, SAssignField(x, s, (rt, e')))
            | "edges" -> 
                (check_assign it rt err, SAssignField(x, s, (rt, e')))
            | _ -> raise (Failure (err)))
      (* split generalized function into specific function *)
      | Call("push_back", ([Id(l) ; e] as el)) as call -> let sub_func = (match type_of_identifier l with
            List(Int) -> "list_push_back"
          | _ -> raise (Failure ("error: push_back not a function of type: " ^ string_of_typ (type_of_identifier l)))) in
          let fd = find_func sub_func in
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
          in (fd.typ, SCall(sub_func, args))
      | Call("push_front", ([Id(l) ; e] as el)) as call -> let sub_func = (match type_of_identifier l with
            List(Int) -> "list_push_front"
          | _ -> raise (Failure ("error: push_front not a function of type: " ^ string_of_typ (type_of_identifier l)))) in
          let fd = find_func sub_func in
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
          in (fd.typ, SCall(sub_func, args))
      | Call("add_node", ([Id(l) ; e] as el)) as call -> let sub_func = (match type_of_identifier l with
            Graph(Node(Int)) -> "graph_add_node"
          | _ -> raise (Failure ("error: add_node not a function of type: " ^ string_of_typ (type_of_identifier l)))) in
          let fd = find_func sub_func in
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
          in (fd.typ, SCall(sub_func, args))
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
      | Access(x, s) -> let t = type_of_identifier x in (match t with
            Node(tn) -> (match s with 
                "val" -> (tn, SAccess(x, s))
              | "id"  -> (Int, SAccess(x, s))
              | "edges" -> (List(Edge(Int)), SAccess(x, s))
              | _ -> raise (Failure ("error: invalid field")))
          | _ -> raise (Failure ("error: this type does not have this field: " ^ s)))
      | Noexpr -> (Void, SNoexpr)
      | UEdge(n1, n2) -> (match (type_of_identifier n1, type_of_identifier n2)
          with 
            (Node(a), Node(b)) when a = b -> (Edge(Node(a)), SUEdge(n1, n2))
          | _ -> raise (Failure ("error: UEdge fail")))

     | UEdgeC(n1, e, n2) -> (match (type_of_identifier n1, type_of_identifier n2)
                         with 
                           (Node(a), Node(b)) when a = b -> (Edge(Node(a)), SUEdgeC(n1, expr e, n2))
                         | _ -> raise (Failure ("error: UEdgeC fail")))

      | DEdge(n1, n2) -> (match (type_of_identifier n1, type_of_identifier n2)
                         with 
                           (Node(a), Node(b)) when a = b -> (Edge(Node(a)), SDEdge(n1, n2))
                         | _ -> raise (Failure ("error: DEdge fail")))

      | DEdgeC(n1, e, n2) -> (match (type_of_identifier n1, type_of_identifier n2)
                         with 
                           (Node(a), Node(b)) when a = b -> 
                              (Edge(Node(a)), SDEdgeC(n1, expr e, n2))
                         | _ -> raise (Failure ("error: DEdgeC fail")))
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
        (*    | Return _ :: _ -> raise (Failure ("Nothing may follow a return"))
         *)   | Block sl :: ss -> check_stmt_list (sl @ ss)
            | s :: ss -> check_stmt s :: check_stmt_list ss
            | [] -> []
          in SBlock(check_stmt_list sl)
      | Return e -> let (t, e') = expr e in
          if t = func.typ then SReturn (t, e')
          else raise (Failure ("error: expected return to be of type: " ^ string_of_typ t))
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, s) -> 
          SFor(expr e1, expr e2, expr e3, check_stmt s)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Continue -> SContinue
      | Break -> SBreak
      | Declare(t, x, Noexpr) -> SDeclare(t, x, (Void, SNoexpr))
      | Declare(t, x, e) as d -> let (t', v) = expr e in
          ignore v;
          if t' = t then SDeclare(t, x, expr e)
          else raise (Failure ("error: assignment does not match value in " ^ 
                string_of_stmt d ))
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
