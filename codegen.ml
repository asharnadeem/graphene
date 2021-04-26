(* Generates LLVM IR from graphene input code (in the form of sast)
  Authors: Matthew Sanchez & Ashar Nadeem *)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let get_type(t, _) = t

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in

  let llmem_graph = L.MemoryBuffer.of_file "graphene.bc" in
  let llm_graph = Llvm_bitreader.parse_bitcode context llmem_graph in
  
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Graphene" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context 
  and i8_t       = L.i8_type     context
  and string_t   = L.pointer_type (L.i8_type context)
  and float_t    = L.double_type context
  and void_t     = L.void_type   context 
  and void_ptr_t = L.pointer_type (L.i8_type context)
  and lst_t      = L.pointer_type 
    (match L.type_by_name llm_graph "struct.list" with
      None -> raise (Failure "error: missing implementation for struct list")
    | Some t -> t)
  and node_t  = L.pointer_type 
    (match L.type_by_name llm_graph "struct.node" with
      None -> raise (Failure "error: missing implementation for struct node")
    | Some t -> t)
  and edge_t  = L.pointer_type 
    (match L.type_by_name llm_graph "struct.edge" with
      None -> raise (Failure "error: missing implementation for struct edge")
    | Some t -> t)
  and graph_t      = L.pointer_type 
    (match L.type_by_name llm_graph "struct.graph" with
      None -> raise (Failure "error: missing implementation for struct graph")
    | Some t -> t)
  in
  

   (* Return the LLVM type for a Graphene type *)
  let ltype_of_typ = function
      A.Int      -> i32_t
    | A.String   -> string_t
    | A.Float    -> float_t
    | A.Void     -> void_t
    | A.Node _   -> node_t
    | A.List _   -> lst_t
    | A.Edge _   -> edge_t
    | A.Graph _  -> graph_t
  in


  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) = 
      let init = match t with
          A.Float -> L.const_float (ltype_of_typ t) 0.0
        | A.Int -> L.const_int (ltype_of_typ t) 0
        | _ -> raise (Failure 
          "error: this type cannot be declared globally")
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_f : L.llvalue = 
      L.declare_function "printf" printf_t the_module in
      
  let list_init_t : L.lltype = 
      L.function_type lst_t [| |] in
  let list_init_f : L.llvalue =
      L.declare_function "list_init" list_init_t the_module in

  let node_init_t : L.lltype = 
      L.function_type node_t [| |] in
  let node_init_f : L.llvalue =
      L.declare_function "node_init" node_init_t the_module in

  let edge_init_t : L.lltype = 
      L.function_type edge_t [| void_ptr_t ; node_t; i32_t |] in
  let edge_init_f : L.llvalue = 
      L.declare_function "edge_init" edge_init_t the_module in

  let graph_init_t : L.lltype = 
      L.function_type graph_t [| |] in
  let graph_init_f : L.llvalue =
      L.declare_function "graph_init" graph_init_t the_module in

  let list_index_t : L.lltype =
      L.function_type void_ptr_t [|lst_t ; i32_t|] in  
  let list_index_f : L.llvalue =
      L.declare_function "list_index" list_index_t the_module in

  let list_push_back_t : L.lltype = 
      L.function_type i32_t [| lst_t ; void_ptr_t |] in
  let list_push_back_f : L.llvalue =
      L.declare_function "list_push_back" list_push_back_t the_module in

  let list_push_front_t : L.lltype = 
      L.function_type i32_t [| lst_t ; void_ptr_t |] in
  let list_push_front_f : L.llvalue =
      L.declare_function "list_push_front" list_push_front_t the_module in

  let list_pop_back_t : L.lltype = 
      L.function_type void_ptr_t [| lst_t |] in
  let list_pop_back_f : L.llvalue =
      L.declare_function "list_pop_back" list_pop_back_t the_module in

  let list_pop_front_t : L.lltype = 
      L.function_type void_ptr_t [| lst_t |] in
  let list_pop_front_f : L.llvalue =
      L.declare_function "list_pop_front" list_pop_front_t the_module in

  let list_peek_back_t : L.lltype = 
      L.function_type void_ptr_t [| lst_t |] in
  let list_peek_back_f : L.llvalue =
      L.declare_function "list_peek_back" list_peek_back_t the_module in

  let list_peek_front_t : L.lltype = 
      L.function_type void_ptr_t [| lst_t |] in
  let list_peek_front_f : L.llvalue =
      L.declare_function "list_peek_front" list_peek_front_t the_module in

  let graph_add_node_t : L.lltype = 
      L.function_type i32_t [| graph_t ; node_t |] in
  let graph_add_node_f : L.llvalue =
      L.declare_function "graph_add_node" graph_add_node_t the_module in

  let graph_add_t : L.lltype = 
      L.function_type node_t [| graph_t; i32_t; void_ptr_t |] in
  let graph_add_f : L.llvalue = 
      L.declare_function "graph_add" graph_add_t the_module in

  let graph_get_node_t : L.lltype = 
      L.function_type node_t [| graph_t ; i32_t |] in
  let graph_get_node_f : L.llvalue =
      L.declare_function "graph_get_node" graph_get_node_t the_module in
  
  let graph_contains_node_t : L.lltype = 
      L.function_type i32_t [| graph_t; node_t |] in
  let graph_contains_node_f : L.llvalue =
      L.declare_function "graph_contains_node" graph_contains_node_t the_module
  in 
  let graph_contains_id_t : L.lltype = 
      L.function_type i32_t [| graph_t; i32_t |] in
  let graph_contains_id_f : L.llvalue =
      L.declare_function "graph_contains_id" graph_contains_id_t the_module in

  (* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
     let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
      let function_decl m fdecl =
        let name = fdecl.sfname
        and formal_types = 
    Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
        in let ftype = 
          L.function_type (ltype_of_typ fdecl.styp) formal_types in
        StringMap.add name (L.define_function name ftype the_module, fdecl) m 
        in List.fold_left function_decl StringMap.empty functions in
    
    (* Fill in the body of the given function *)
    let build_function_body fdecl =
      let (the_function, _) = StringMap.find fdecl.sfname function_decls in
      let builder = L.builder_at_end context (L.entry_block the_function) in
  
      let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
      and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder 
      and str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in
  
      (* Construct the function's "locals": formal arguments and locally
         declared variables.  Allocate each on the stack, initialize their
         value, if appropriate, and remember their values in the "locals" map *)
      let local_vars =
        let add_formal m (t, n) p = 
          L.set_value_name n p;

      let local = L.build_alloca (ltype_of_typ t) n builder in
      ignore (L.build_store p local builder);
      StringMap.add n local m 
  
        (* Allocate space for any locally declared variables and add the
         * resulting registers to our map *)
        and add_local m (t, n) = 
    let local_var = L.build_alloca (ltype_of_typ t) n builder
    in StringMap.add n local_var m 
        in
  
        let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
            (Array.to_list (L.params the_function)) in
        let rec gather_locals locals = function
            [] -> locals
          | h::t -> gather_locals (match h with
              SDeclare(t, s, (_, Sast.SNoexpr)) -> 
      List.fold_left (fun locs sl -> (t, sl) :: locs) locals s
            | SDeclare(t, ([s] as l), _) when List.length l = 1 
                          -> (t, s) :: locals
            | _ -> locals) t
        in
        let complete_locals = gather_locals [] fdecl.sbody
        in
        List.fold_left add_local formals complete_locals 
      in
  
      (* Return the value for a variable or formal argument.
         Check local names first, then global names *)
      let lookup n = try StringMap.find n local_vars
                     with Not_found -> StringMap.find n global_vars
      in 
    
  
      (* Construct code for an expression; return its value *)
      let rec expr builder ((_, e) : sexpr) = match e with
        SIlit i     -> L.const_int i32_t i
      | SFlit l     -> L.const_float_of_string float_t l
      | SSlit s     -> L.build_global_stringptr (s ^ "\x00") "str" builder
      | SNoexpr     -> L.const_int i32_t 0
      | SId s       -> L.build_load (lookup s) s builder
      | SAssign (s, e) -> let e' = expr builder e in
                          ignore(L.build_store e' (lookup s) builder); e'
      | SAssignField (x, s, e) -> let e' = expr builder e and
                                      x' = expr builder x in  
        (match x with 
          (A.Node(t), _)  ->   
          (match s with
            "id" ->         
            let p = L.build_struct_gep x' 0 "struct.ptr" builder in
          ignore(L.build_store e' p builder); e'
          | "val" -> 
          let str_ptr = L.build_struct_gep x' 1 "struct.ptr" builder in
          let void_ptr = L.build_load str_ptr "void.ptr" builder in
          let cast = 
            L.build_bitcast void_ptr (L.pointer_type (ltype_of_typ t))
            "val" builder in 
          ignore (L.build_store e' cast builder); e'
          | "edges" -> 
            let str_ptr = L.build_struct_gep x' 2 "struct.ptr" builder in
            let edge_ptr = L.build_load str_ptr "edge.ptr" builder in
            ignore(L.build_store e' edge_ptr builder); e'
          | _ -> raise (Failure 
                "this should never happen, field error missed in semant"))
        | _ -> raise (Failure "internal error on assignfield"))
      | SBinop ((A.Float,_ ) as e1, op, e2) ->
        let e1' = expr builder e1
        and e2' = expr builder e2 in
        (match op with 
            A.Add     -> L.build_fadd
          | A.Sub     -> L.build_fsub
          | A.Mul     -> L.build_fmul
          | A.Div     -> L.build_fdiv 
          | A.Mod     -> L.build_frem
          | A.Eq      -> L.build_fcmp L.Fcmp.Oeq
          | A.Neq     -> L.build_fcmp L.Fcmp.One
          | A.Less    -> L.build_fcmp L.Fcmp.Olt
          | A.Leq     -> L.build_fcmp L.Fcmp.Ole
          | A.Greater -> L.build_fcmp L.Fcmp.Ogt
          | A.Geq     -> L.build_fcmp L.Fcmp.Oge
          | A.And | A.Or ->
              raise (Failure 
                "internal error: semant should have rejected and/or on float")
          ) e1' e2' "tmp" builder
      | SBinop ((A.Int,_ ) as e1, op, e2) ->
          let e1' = expr builder e1
          and e2' = expr builder e2 in
          (match op with
            A.Add     -> L.build_add
          | A.Sub     -> L.build_sub
          | A.Mul     -> L.build_mul
          | A.Div     -> L.build_sdiv
          | A.Mod     -> L.build_srem
          | A.And     -> L.build_and
          | A.Or      -> L.build_or
          | A.Eq      -> L.build_icmp L.Icmp.Eq
          | A.Neq     -> L.build_icmp L.Icmp.Ne
          | A.Less    -> L.build_icmp L.Icmp.Slt
          | A.Leq     -> L.build_icmp L.Icmp.Sle
          | A.Greater -> L.build_icmp L.Icmp.Sgt
          | A.Geq     -> L.build_icmp L.Icmp.Sge
          ) e1' e2' "tmp" builder
            | SUnop(op, ((t, _) as e)) ->
                let e' = expr builder e in
          (match op with
            A.Neg when t = A.Float -> L.build_fneg 
          | A.Neg                  -> L.build_neg
          | A.Not                  -> L.build_not) e' "tmp" builder
      | SBinop ((ty,_ ) as e1, op, e2) ->
          if op = A.Eq then
            let e1' = expr builder e1
            and e2' = expr builder e2 in
            L.build_icmp L.Icmp.Eq e1' e2' "tmp" builder
          else if op = A.Neq then
            let e1' = expr builder e1
            and e2' = expr builder e2 in
            L.build_icmp L.Icmp.Ne e1' e2' "tmp" builder
          else raise (Failure ("invalid operator " ^ A.string_of_binop op ^
            " on " ^ A.string_of_typ ty))
      | SIndex ((lt, _) as s, e) -> (match lt with
          A.List(t) -> (match t with
            A.Int | A.Float ->
          let ptr = (L.build_call list_index_f 
            [|expr builder s; expr builder e |] "list_index" builder)
            and ty = (L.pointer_type (ltype_of_typ t)) in 
          let cast = L.build_bitcast ptr ty "cast" builder in 
          L.build_load cast "val" builder
          | _ ->  let ptr = L.build_call list_index_f 
            [|expr builder s; expr builder e |] "list_index" builder
            and ty =  (ltype_of_typ t) in
             L.build_bitcast ptr ty "cast" builder)
        | A.Graph(_)-> 
          L.build_call graph_get_node_f 
          [| expr builder s; expr builder e |] "graph_get_node" builder
        | _ -> raise (Failure 
          ("this should never happen, error in semant")) )
      | SCall (f, args) ->
           let (fdef, fdecl) = StringMap.find f function_decls in
     let llargs = List.rev (List.map (expr builder) (List.rev args)) in
     let result = (match fdecl.styp with 
            A.Void -> ""
          | _ -> f ^ "_result") in
           L.build_call fdef (Array.of_list llargs) result builder
      | SPrint(e) -> (match e with
          (A.Int, _) -> 
            L.build_call printf_f [| int_format_str ; (expr builder e) |]
            "printf" builder
        | (A.Float, _) ->
            L.build_call printf_f [| float_format_str ; (expr builder e) |]
            "printf" builder
        | (A.String, _) ->
            L.build_call printf_f [| str_format_str ; (expr builder e) |]
            "printf" builder
        | (A.Node(t), _) -> 
          let id = expr builder ((A.Int), (SAccess(e, "id")))
          in let nval = (expr builder (t, SAccess(e, "val")))
          in let edges = (expr builder 
            (A.Int, SAccess((A.List(A.Edge(t)), SAccess(e, "edges")), "size")))
           in  let str_mod  = 
          (match t with
            A.Int -> "d"
          | A.Float -> "f"
          | A.String -> "s"
          | _ -> raise (Failure "internal error on node") )
          in let format_str = 
            L.build_global_stringptr 
            ("id: %d, val: %" ^ str_mod ^ ", edges: %d\n") "fmt" builder in
            L.build_call printf_f 
            [| format_str; id; nval; edges|] 
            "printf" builder
        | _ -> raise (Failure "internal error on print") )
      | SAccess (s, x) -> let s' = expr builder s in 
          (match s with 
          (A.Node(t), _) ->
            (match x with
              "id" -> 
              (let p = L.build_struct_gep s' 0 "struct.ptr" builder in
            L.build_load p ("struct.val." ^ x) builder)
            | "val" -> 
            (let str_ptr = L.build_struct_gep s' 1 "struct.ptr" builder in
            let void_ptr = L.build_load str_ptr "void.ptr" builder in
            let ty = ltype_of_typ t in
            let cast = 
              L.build_bitcast void_ptr (L.pointer_type ty) "cast" builder in
            L.build_load cast ("struct.val." ^ x ^ ".value") builder)
            | "edges" -> 
              (let str_ptr = L.build_struct_gep s' 2 "struct.ptr" builder in
              L.build_load str_ptr ("struct.val." ^ x ) builder) 
            | _ -> raise (Failure 
                ("this should never happen, error in semant")))
        | (A.List(_), _) -> 
            (match x with
              "size" -> (let p = L.build_struct_gep s' 0 "struct.ptr" builder 
              in L.build_load p ("struct.val." ^ x) builder)
            | _ -> raise 
                (Failure "this should never happen, error in semant")
              )
        | (A.Graph(_), _) -> let mem = (match x with
            | "nodes" -> 0
            | "root" -> 1
            | _ -> raise (Failure "intdfasdfasernal error in accessing")) in 
          let p = L.build_struct_gep s' mem "struct.ptr" builder 
          in L.build_load p ("struct.val." ^ x) builder
        | (A.Edge(t), _) -> (match x with 
              "weight" -> 
               (let str_ptr = L.build_struct_gep s' 0 "struct.ptr" builder in
            let void_ptr = L.build_load str_ptr "void.ptr" builder in
            let ty = ltype_of_typ t in
            let cast = 
              L.build_bitcast void_ptr (L.pointer_type ty) "cast" builder in
            L.build_load cast ("struct.val." ^ x ^ ".value") builder)
            | "dest" -> 
                let str_ptr = L.build_struct_gep s' 1 "struct.ptr" builder
                in 
                  L.build_load str_ptr ("struct.val." ^ x) builder
            | "t" -> 
                let str_ptr = L.build_struct_gep s' 2 "struct.ptr" builder
                in L.build_load str_ptr ("struct.val." ^ x) builder
            | _ -> raise (Failure "internal error in accessing"))
        | _ -> raise (Failure "internal error")
          )
      | SDEdge (n1, n2) -> let n1p = expr builder n1 and
                               n2p = expr builder n2 in 
        let atr = 
        (match n1 with
          (A.Node(A.String), _) -> 
            ((L.build_global_stringptr ("\x00") "str" builder), A.String)
        | (A.Node(A.Int), _) -> 
            ((L.const_int i32_t 0), A.Int)
        | (A.Node(A.Float), _ )->  
            ((L.const_float float_t 0.0), A.Float)
        | _ -> raise (Failure "internal error"))
        in
        (* allocate space for and store weight value *)
        let ptr = L.build_malloc (ltype_of_typ (snd atr)) "element" builder in
        ignore (L.build_store (fst atr) ptr builder); 
        (* cast to void * *)
        let cast = L.build_bitcast ptr void_ptr_t "cast" builder in
        (* call edge init function, returns pointer to edge *)
         let fedgep = L.build_call edge_init_f
          [|cast; n2p; L.const_int i32_t 1 |] 
          "edge_init" builder and
            bedgep = L.build_call edge_init_f
          [|cast; n1p; L.const_int i32_t 0|] 
          "edge_init" builder in  
          (* get edge lists for nodes *)
        let n1el = L.build_struct_gep n1p 2 "struct.ptr" builder and
            n2el = L.build_struct_gep n2p 2 "struct.ptr" builder in 
        let n1ell = L.build_load n1el "temp" builder and
            n2ell = L.build_load n2el "temp" builder in
            (* cast edge ptrs to void * *)
        let cast1 = L.build_bitcast fedgep void_ptr_t 
            "cast" builder and
            cast2 = L.build_bitcast bedgep void_ptr_t
            "cast" builder in
            (* push edge ptrs to node edge lists *)
        ignore (L.build_call list_push_back_f 
            [| n1ell; cast1|] "edge_push" builder);
        ignore (L.build_call list_push_back_f
            [| n2ell; cast2|] "edge_push" builder);
            expr builder n1;
      | SDEdgeC (n1, e, n2) -> let e' = expr builder e in 
          let n1p = expr builder n1 and
              n2p = expr builder n2 in 
          let typ = 
        (match n1 with
          (A.Node(A.String), _) -> (A.String)
        | (A.Node(A.Int), _) ->  (A.Int)
        | (A.Node(A.Float), _ )->  (A.Float)
        | _ -> raise (Failure "internal error"))
        in
        let ptr = L.build_malloc (ltype_of_typ (typ)) "element" builder in
        ignore (L.build_store e' ptr builder); 
        let cast = L.build_bitcast ptr void_ptr_t "cast" builder in
         let fedgep = L.build_call edge_init_f
          [|cast; n2p; L.const_int i32_t 1 |] 
          "edge_init" builder and
            bedgep = L.build_call edge_init_f
          [|cast; n1p; L.const_int i32_t 0|] 
          "edge_init" builder in  
        let n1el = L.build_struct_gep n1p 2 "struct.ptr" builder and
            n2el = L.build_struct_gep n2p 2 "struct.ptr" builder in 
        let n1ell = L.build_load n1el "temp" builder and
            n2ell = L.build_load n2el "temp" builder in
        let cast1 = L.build_bitcast fedgep void_ptr_t 
            "cast" builder and
            cast2 = L.build_bitcast bedgep void_ptr_t
            "cast" builder in
        ignore (L.build_call list_push_back_f 
            [| n1ell; cast1|] "edge_push" builder);
        ignore (L.build_call list_push_back_f
            [| n2ell; cast2|] "edge_push" builder);
            expr builder n1;
      | SUEdge (n1, n2) -> let n1p = expr builder n1 and
                               n2p = expr builder n2 in 
        let atr = 
        (match n1 with
          (A.Node(A.String), _) -> 
            ((L.build_global_stringptr ("\x00") "str" builder), A.String)
        | (A.Node(A.Int), _) -> 
            ((L.const_int i32_t 0), A.Int)
        | (A.Node(A.Float), _ )->  
            ((L.const_float float_t 0.0), A.Float)
        | _ -> raise (Failure "internal error"))
        in
        let ptr = L.build_malloc (ltype_of_typ (snd atr)) "element" builder in
        ignore (L.build_store (fst atr) ptr builder); 
        let cast = L.build_bitcast ptr void_ptr_t "cast" builder in
         let fedgep = L.build_call edge_init_f
          [|cast; n2p; L.const_int i32_t 1 |] 
          "edge_init" builder and
            bedgep = L.build_call edge_init_f
          [|cast; n1p; L.const_int i32_t 1|] 
          "edge_init" builder in  
        let n1el = L.build_struct_gep n1p 2 "struct.ptr" builder and
            n2el = L.build_struct_gep n2p 2 "struct.ptr" builder in 
        let n1ell = L.build_load n1el "temp" builder and
            n2ell = L.build_load n2el "temp" builder in
        let cast1 = L.build_bitcast fedgep void_ptr_t 
            "cast" builder and
            cast2 = L.build_bitcast bedgep void_ptr_t
            "cast" builder in
        ignore (L.build_call list_push_back_f 
            [| n1ell; cast1|] "edge_push" builder);
        ignore (L.build_call list_push_back_f
            [| n2ell; cast2|] "edge_push" builder);
            expr builder n1;
      | SUEdgeC (n1, e, n2) -> let e' = expr builder e in 
          let n1p = expr builder n1 and
              n2p = expr builder n2 in 
          let typ = 
        (match n1 with
          (A.Node(A.String), _) -> (A.String)
        | (A.Node(A.Int), _) ->  (A.Int)
        | (A.Node(A.Float), _ )->  (A.Float)
        | _ -> raise (Failure "internal error"))
        in
        let ptr = L.build_malloc (ltype_of_typ (typ)) "element" builder in
        ignore (L.build_store e' ptr builder); 
        let cast = L.build_bitcast ptr void_ptr_t "cast" builder in
         let fedgep = L.build_call edge_init_f
          [|cast; n2p; L.const_int i32_t 1 |] 
          "edge_init" builder and
            bedgep = L.build_call edge_init_f
          [|cast; n1p; L.const_int i32_t 1|] 
          "edge_init" builder in  
        let n1el = L.build_struct_gep n1p 2 "struct.ptr" builder and
            n2el = L.build_struct_gep n2p 2 "struct.ptr" builder in 
        let n1ell = L.build_load n1el "temp" builder and
            n2ell = L.build_load n2el "temp" builder in
        let cast1 = L.build_bitcast fedgep void_ptr_t 
            "cast" builder and
            cast2 = L.build_bitcast bedgep void_ptr_t
            "cast" builder in
        ignore (L.build_call list_push_back_f 
            [| n1ell; cast1|] "edge_push" builder);
        ignore (L.build_call list_push_back_f
            [| n2ell; cast2|] "edge_push" builder);
            expr builder n1;
      | SPushBack(s, e) -> (match (s, e) with 
           ((A.List(t), l), _) -> let e' = expr builder e in
           (match t with 
              A.Int | A.Float ->
        let ptr = L.build_malloc (ltype_of_typ t) "element" builder in
        ignore (L.build_store e' ptr builder); 
        let cast = L.build_bitcast ptr void_ptr_t "cast" builder in
          ignore (L.build_call list_push_back_f 
          [| expr builder s; cast|] "list_push_back" builder);
          expr builder (A.List(t),l) 
            | _ -> let cast = L.build_bitcast e' void_ptr_t "cast" builder in
          ignore (L.build_call list_push_back_f 
          [| expr builder s; cast|] "list_push_back" builder);
          expr builder s)          
            
          | _ -> raise (Failure ("internal error on pushback"))  )
      | SPushFront(s, e) -> (match (s, e) with 
           ((A.List(t), l), _) -> let e' = expr builder e in
           (match t with 
              A.Int | A.Float ->
        let ptr = L.build_malloc (ltype_of_typ t) "element" builder in
        ignore (L.build_store e' ptr builder); 
        let cast = L.build_bitcast ptr void_ptr_t "cast" builder in
          ignore (L.build_call list_push_front_f 
          [| expr builder s; cast|] "list_push_front" builder);
          expr builder (A.List(t),l) 
            | _ -> let cast = L.build_bitcast e' void_ptr_t "cast" builder in
          ignore (L.build_call list_push_front_f 
          [| expr builder s; cast|] "list_push_front" builder);
          expr builder s)          
            
          | _ -> raise (Failure ("internal error on pushback"))  )
      | SPopBack(s) -> (match s with
            (A.List(t), _)  ->
           (match t with 
              A.Int | A.Float -> 
              (let ptr = (L.build_call list_pop_back_f 
            [| expr builder s|] "list_pop_back" builder)
            and ty = (L.pointer_type (ltype_of_typ t)) in         
            let cast = L.build_bitcast ptr ty "cast" builder in 
            L.build_load cast "val" builder)

            | _ ->  (let ptr = (L.build_call list_pop_back_f 
            [| expr builder s |] "list_pop_back" builder)
            and ty = (ltype_of_typ t) in 
            L.build_bitcast ptr ty "cast" builder)
            )
          | _ -> raise (Failure "internal error on pop_back") )
      | SPopFront(s) -> (match s with
            (A.List(t), _)  ->
           (match t with 
              A.Int | A.Float -> 
              (let ptr = (L.build_call list_pop_front_f 
            [| expr builder s|] "list_pop_front" builder)
            and ty = (L.pointer_type (ltype_of_typ t)) in         
            let cast = L.build_bitcast ptr ty "cast" builder in 
            L.build_load cast "val" builder)

            | _ ->  (let ptr = (L.build_call list_pop_front_f 
            [| expr builder s |] "list_pop_front" builder)
            and ty = (ltype_of_typ t) in 
            L.build_bitcast ptr ty "cast" builder)
            )
          | _ -> raise (Failure "internal error on pop_back") )
      | SPeekBack(s) -> (match s with
            (A.List(t), _)  ->
           (match t with 
              A.Int | A.Float  -> 
              (let ptr = (L.build_call list_peek_back_f 
            [| expr builder s|] "list_peek_back" builder)
            and ty = (L.pointer_type (ltype_of_typ t)) in         
            let cast = L.build_bitcast ptr ty "cast" builder in 
            L.build_load cast "val" builder)

            | _ ->  (let ptr = (L.build_call list_peek_back_f 
            [| expr builder s |] "list_peek_back" builder)
            and ty = (ltype_of_typ t) in 
            L.build_bitcast ptr ty "cast" builder)
            )
          | _ -> raise (Failure "internal error on peek_front") )
      | SPeekFront(s) -> (match s with
            (A.List(t), _)  ->
           (match t with 
              A.Int | A.Float -> 
              (let ptr = (L.build_call list_peek_front_f 
            [| expr builder s|] "list_peek_front" builder)
            and ty = (L.pointer_type (ltype_of_typ t)) in         
            let cast = L.build_bitcast ptr ty "cast" builder in 
            L.build_load cast "val" builder)

            | _ ->  (let ptr = (L.build_call list_peek_front_f 
            [| expr builder s |] "list_peek_front" builder)
            and ty = (ltype_of_typ t) in 
            L.build_bitcast ptr ty "cast" builder)
            )
          | _ -> raise (Failure "internal error on peek_front") )
      | SAddNode(g, e) ->  L.build_call graph_add_node_f 
          [| expr builder g; expr builder e|] "graph_add_node" builder
      | SGAdd(g, id, v) -> (match g with
          (A.Graph(t), _) -> 
            let nptr = L.build_malloc (ltype_of_typ t) "nodeval" builder in
            ignore(L.build_store (expr builder v) nptr builder);
            let cast = L.build_bitcast nptr void_ptr_t "cast" builder in
            L.build_call graph_add_f
            [| expr builder g; expr builder id; cast |] "graph_add" builder
        | _ -> raise (Failure "internal error on add"))
      | SContains(g, n) -> L.build_call graph_contains_node_f  
          [|expr builder g; expr builder n|] "graph_contains_node" builder
      | SContainsId(g, id) -> L.build_call graph_contains_id_f  
          [|expr builder g; expr builder id|] "graph_contains_id" builder
      | SAddAll(s, el) -> let s' = expr builder s in (match s with 
          (A.List(t),_) -> (match t with
              A.Int | A.Float -> let addfun e = ( 
          let e' = expr builder e in
          let ptr = L.build_malloc (ltype_of_typ t) "element" builder in
          ignore (L.build_store e' ptr builder); 
          let cast = L.build_bitcast ptr void_ptr_t "cast" builder in
          ignore(L.build_call list_push_back_f 
          [| s'; cast|] "list_push_back" builder)) in
          List.iter addfun el;
          s'
            | _ ->  let addfun e = ( 
          let e' = expr builder e in
          let cast = L.build_bitcast e' void_ptr_t "cast" builder in
          ignore(L.build_call list_push_back_f 
          [| s'; cast|] "list_push_back" builder)) in
          List.iter addfun el;
          s')
        | (A.Graph(t), _) -> let l = expr builder 
          (A.List(A.Node(t)), SAccess(s, "edges"))
        in (match t with
              A.Int | A.Float -> let addfun e = ( 
          let e' = expr builder e in
          let ptr = L.build_malloc (ltype_of_typ t) "element" builder in
          ignore (L.build_store e' ptr builder); 
          let cast = L.build_bitcast ptr void_ptr_t "cast" builder in
          ignore(L.build_call list_push_back_f 
          [| l; cast|] "list_push_back" builder)) in
          List.iter addfun el;
          s'
            | _ ->  let addfun e = ( 
          let e' = expr builder e in
          let cast = L.build_bitcast e' void_ptr_t "cast" builder in
          ignore(L.build_call list_push_back_f 
          [| l; cast|] "list_push_back" builder)) in
          List.iter addfun el;
          s')
        | _ -> raise (Failure "internal error on add_all"))
      in
      

    (* LLVM insists each basic block end with exactly one "terminator" 
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
       let add_terminal builder instr =
        match L.block_terminator (L.insertion_block builder) with
          Some _ -> ()
        | None -> ignore (instr builder) in
    
      (* Build the code for the given statement; return the builder for
         the statement's successor (i.e., the next instruction will be built
         after the one generated by this call) *)
  
      let rec stmt builder = function
          SBlock sl -> List.fold_left stmt builder sl
        | SExpr e -> ignore(expr builder e); builder 
        | SReturn e -> ignore(match fdecl.styp with
                                (* Special "return nothing" instr *)
                                A.Void -> L.build_ret_void builder
                                (* Build return statement *)
                              | _ -> L.build_ret (expr builder e) builder );
                       builder
        | SIf (predicate, then_stmt, else_stmt) ->
           let bool_val = let predval = expr builder predicate in 
            (match predicate with 
              (A.Int, SBinop(_)) -> predval
            | (A.Int, _) -> 
           L.build_icmp L.Icmp.Sgt predval (L.const_int i32_t 0) "tmp" builder
            | (A.Float, _) -> 
          L.build_fcmp L.Fcmp.Ogt predval (L.const_float float_t 0.0) 
              "tmp" builder
            | _ -> raise (Failure "internal error on if-pred")) in
           let merge_bb = L.append_block context "merge" the_function in
           let build_br_merge = L.build_br merge_bb in (* partial function *)
           
           let then_bb = L.append_block context "then" the_function in
           add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
           build_br_merge;
           
           let else_bb = L.append_block context "else" the_function in
           add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
           build_br_merge;
           
           ignore(L.build_cond_br bool_val then_bb else_bb builder);
           L.builder_at_end context merge_bb
  
        | SWhile (predicate, body) ->
        let pred_bb = L.append_block context "while" the_function in
        ignore(L.build_br pred_bb builder);
        
        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (stmt (L.builder_at_end context body_bb) body)
        (L.build_br pred_bb);
        
        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val = let predval = expr pred_builder predicate in 
            (match predicate with 
              (A.Int, SBinop(_)) -> predval
            | (A.Int, _) -> 
           L.build_icmp L.Icmp.Sgt predval (L.const_int i32_t 0) 
            "tmp" pred_builder
            | (A.Float, _) -> 
          L.build_fcmp L.Fcmp.Ogt predval (L.const_float float_t 0.0) 
              "tmp" pred_builder
            | _ -> raise (Failure "internal error on if-pred")) in
  
      let merge_bb = L.append_block context "merge" the_function in
      ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
      L.builder_at_end context merge_bb
  
        (* Implement for loops as while loops *)
        | SFor (e1, e2, e3, body) -> stmt builder
        ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
        | SDeclare(A.List(_), ll, _) -> List.iter (fun s -> 
            let ptr = L.build_call list_init_f [| |] "init_list" builder in
            ignore (L.build_store ptr (lookup s) builder)) ll; builder
        | SDeclare(A.Graph(_), lg, _) -> List.iter (fun g -> 
            let ptr = L.build_call graph_init_f [| |] "init_graph" builder in
            ignore (L.build_store ptr (lookup g) builder)) lg ; builder
        | SDeclare(A.Node(t), ln, _) -> List.iter (fun n -> 
            let ptr = L.build_call node_init_f [| |] "init_node" builder in
            ignore (L.build_store ptr (lookup n) builder)  ;
            let val_ptr = 
              L.build_struct_gep ptr 1 "struct.ptr" builder in
            let mptr = L.build_malloc (ltype_of_typ t) "nodeval" builder in
             let cast = L.build_bitcast mptr void_ptr_t "cast" builder in 
            ignore (L.build_store cast val_ptr builder)) ln ; builder 
        | SDeclare(_, _, a) -> ignore(expr builder a); builder
        
      in
  
      (* Build the code for each statement in the function *)
      let builder = stmt builder (SBlock fdecl.sbody) in
  
      (* Add a return if the last block falls off the end *)
      add_terminal builder (match fdecl.styp with
          A.Void -> L.build_ret_void
        | A.Float -> L.build_ret (L.const_float float_t 0.0)
        | t -> L.build_ret (L.const_int (ltype_of_typ t) 0)) 
    in

  List.iter build_function_body functions;
  the_module
