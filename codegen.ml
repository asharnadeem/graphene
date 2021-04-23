module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let get_type(t, _) = t

(* TODO: returning a variable doesn't work?
         need to figure out how to check null ptr for indexing out of bounds 
         other types of edges (basically copy what I did for dedge) 
         graphs
         some way to print edges?
         cleaup
         DONE???? *)

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
        | _ -> L.const_int (ltype_of_typ t) 0
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
      L.function_type edge_t [| i32_t; node_t; i32_t |] in
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

  let list_empty_t : L.lltype =
      L.function_type i32_t [| lst_t |] in  
  let list_empty_f : L.llvalue =
      L.declare_function "list_empty" list_empty_t the_module in

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

  (* let node_set_id_t : L.lltype = 
      L.function_type void_ptr_t [| node_t ; i32_t|] in
  let node_set_id_f : L.llvalue =
      L.declare_function "node_set_id" node_set_id_t the_module in

  let node_set_val_t : L.lltype = 
      L.function_type void_ptr_t [| node_t ; void_ptr_t|] in
  let node_set_val_f : L.llvalue =
      L.declare_function "node_set_val" node_set_val_t the_module in

  let node_get_id_t : L.lltype = 
      L.function_type i32_t [| node_t |] in
  let node_get_id_f : L.llvalue =
      L.declare_function "node_get_id" node_get_id_t the_module in

  let node_get_val_t : L.lltype = 
      L.function_type void_ptr_t [| node_t |] in
  let node_get_val_f : L.llvalue =
      L.declare_function "node_get_val" node_get_val_t the_module in *)

  let graph_add_node_t : L.lltype = 
      L.function_type i32_t [| graph_t ; node_t |] in
  let graph_add_node_f : L.llvalue =
      L.declare_function "graph_add_node" graph_add_node_t the_module in

  let graph_get_node_t : L.lltype = 
      L.function_type node_t [| graph_t ; i32_t |] in
  let graph_get_node_f : L.llvalue =
      L.declare_function "graph_get_node" graph_get_node_t the_module in

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
      and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in
  
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
                        SDeclare(t, s, _) -> (t, s) :: locals
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
      let rec expr builder ((expr_typ, e) : sexpr) = match e with
          SIlit i     -> L.const_int i32_t i
        | SFlit l     -> L.const_float_of_string float_t l
        | SSlit s     -> L.const_float_of_string float_t s 
        | SNoexpr     -> L.const_int i32_t 0
        | SId s       -> L.build_load (lookup s) s builder
        | SAssign (s, e) -> let e' = expr builder e in
                            ignore(L.build_store e' (lookup s) builder); e'
        | SAssignField (x, s, e) -> let e' = expr builder e and
                                        x' = expr builder x in 
           
            (match s with
              "id" -> 
              
              let p = L.build_struct_gep x' 0 "struct.ptr" builder in
            ignore(L.build_store e' p builder); e'
            | "val" -> 
            let val_ptr = L.build_struct_gep x' 1 "struct.ptr" builder in
            let ty = ltype_of_typ expr_typ in
            let cast = 
              L.build_bitcast val_ptr (L.pointer_type ty) "val" builder in 
            ignore (L.build_store e' cast builder); e'
            | "edges" -> 
              
              let p = L.build_struct_gep x' 2 "struct.ptr" builder in
              ignore(L.build_store e' p builder); e'
            | _ -> raise (Failure 
                  "this should never happen, field error missed in semant"))
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
        | SBinop (e1, op, e2) ->
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
      | SCall ("print", [e]) | SCall ("printb", [e]) ->
	  L.build_call printf_f [| int_format_str ; (expr builder e) |]
	    "printf" builder
      | SCall ("printf", [e]) -> 
	  L.build_call printf_f [| float_format_str ; (expr builder e) |]
	    "printf" builder
      | SListIndex ((A.List(t), _) as ls, e) -> 
        let ptr = (L.build_call list_index_f 
          [|expr builder ls; expr builder e |] "list_index" builder)
          and ty = (L.pointer_type (ltype_of_typ t)) in 
              if L.is_null ptr then (
                raise (Failure ("error: trying to index uninitialized list")))
                (* match t with
                A.Int -> L.const_int i32_t 0
              | A.Float -> L.const_float float_t 0.0 
              | _ -> raise (Failure "NULL not implemented for this list index")) *)
          else (let cast = L.build_bitcast ptr ty "cast" builder in 
        L.build_load cast "val" builder)
      | SListIndex(_) -> raise (Failure 
          ("this should never happen, error in semant"))
      | SCall ("list_empty", [ A.List(t), l ]) -> 
            L.build_call list_empty_f 
          [| expr builder (t, l) |] "list_empty" builder
      | SCall ("list_push_back", [(A.List(t),l); e]) -> 
        let e' = expr builder e in
        let ptr = L.build_malloc (ltype_of_typ t) "element" builder in
        ignore (L.build_store e' ptr builder); 
        let cast = L.build_bitcast ptr void_ptr_t "cast" builder in
          L.build_call list_push_back_f 
          [| expr builder (t, l); cast|] "list_push_back" builder
      | SCall ("list_push_front", [(A.List(t),l); e]) -> 
        let e' = expr builder e in
        let ptr = L.build_malloc (ltype_of_typ t) "element" builder in
        ignore (L.build_store e' ptr builder); 
        let cast = L.build_bitcast ptr void_ptr_t "cast" builder in
           L.build_call list_push_front_f 
          [| expr builder (t, l); cast|] "list_push_front" builder
      | SCall ("list_pop_back", [ (A.List(t), l) ]) -> 
        let ptr = (L.build_call list_pop_back_f 
          [| expr builder (A.List(t), l) |] "list_pop_back" builder)
          and ty = (L.pointer_type (ltype_of_typ t)) in 
              if L.is_null ptr then (
                raise (Failure 
                  ("error: trying to pop from uninitialized list")))
                (* match t with
                A.Int -> L.const_int i32_t 0
              | A.Float -> L.const_float float_t 0.0 
              | _ -> raise (Failure "NULL not implemented for this list index")) *)
          else (let cast = L.build_bitcast ptr ty "cast" builder in 
        L.build_load cast "val" builder)
      | SCall ("list_pop_front", [ (A.List(t), l) ]) -> 
        let ptr = (L.build_call list_pop_front_f 
          [| expr builder (A.List(t), l) |] "list_pop_front" builder)
          and ty = (L.pointer_type (ltype_of_typ t)) in 
              if L.is_null ptr then (
                raise (Failure 
                  ("error: trying to pop from uninitialized list") ) )
                (* match t with
                A.Int -> L.const_int i32_t 0
              | A.Float -> L.const_float float_t 0.0 
              | _ -> raise (Failure "NULL not implemented for this list index")) *)
          else (let cast = L.build_bitcast ptr ty "cast" builder in 
        L.build_load cast "val" builder)
      | SCall ("graph_add_node", [(A.Graph(t),l); e]) -> (*let list_type = match t with
          A.List(A.Int) -> L.const_int i32_t 1
        | A.List(A.Float) -> L.const_int i32_t 2
        | _ -> L.const_int (ltype_of_typ t) 0
        in  *)
        let e' = expr builder e in
        let ptr = L.build_malloc (ltype_of_typ t) "node" builder in
        ignore (L.build_store e' ptr builder); 
        let cast = L.build_bitcast ptr node_t "cast" builder in
           L.build_call graph_add_node_f 
          [| expr builder (t, l); cast|] "graph_add_node" builder
      | SCall ("graph_get_node", [(A.Graph(t),l); e]) ->
        let e' = expr builder e in
        let ptr = L.build_malloc (ltype_of_typ t) "element" builder in
        ignore (L.build_store e' ptr builder); 
        (* let cast = L.build_bitcast ptr node_t "cast" builder in *)
           L.build_call graph_get_node_f 
          [| expr builder (t, l)|] "graph_get_node" builder
      | SCall (f, args) ->
           let (fdef, fdecl) = StringMap.find f function_decls in
     let llargs = List.rev (List.map (expr builder) (List.rev args)) in
     let result = (match fdecl.styp with 
            A.Void -> ""
          | _ -> f ^ "_result") in
           L.build_call fdef (Array.of_list llargs) result builder
      | SAccess (s, x) -> let s' = expr builder s in 
          (match x with
            "id" -> 
            (let p = L.build_struct_gep s' 0 "struct.ptr" builder in
          L.build_load p ("struct.val." ^ x) builder)
          | "val" -> 
          (let void_ptr = L.build_struct_gep s' 1 "struct.ptr" builder in
          let ty = ltype_of_typ expr_typ in
          let cast = 
            L.build_bitcast void_ptr (L.pointer_type ty) "cast" builder in
          L.build_load cast ("struct.val." ^ x ^ ".value") builder)
          | "edges" -> 
            (let p = L.build_struct_gep s' 2 "struct.ptr" builder in
            L.build_load p ("struct.val." ^ x ) builder) 
          | _ -> raise (Failure ("this should never happen, error in semant")))
      | SDEdge (n1, n2) -> let n1p = expr builder n1 and
                               n2p = expr builder n2 in 
         let fedgep = L.build_call edge_init_f
          [|L.const_int i32_t 0; n2p; L.const_int i32_t 1 |] 
          "edge_init" builder and
            bedgep = L.build_call edge_init_f
          [|L.const_int i32_t 0; n1p; L.const_int i32_t 0|] 
          "edge_init" builder in  
        let n1el = L.build_struct_gep n1p 2 "struct.ptr" builder and
            n2el = L.build_struct_gep n2p 2 "struct.prt" builder in 
        let n1ell = L.build_load n1el "temp" builder and
            n2ell = L.build_load n2el "temp" builder in
        let cast1 = L.build_bitcast fedgep void_ptr_t 
            "cast" builder and
            cast2 = L.build_bitcast bedgep void_ptr_t
            "cast" builder in
        ignore (L.build_call list_push_back_f 
            [| n1ell; cast1|] "edge_push" builder);
        L.build_call list_push_back_f
            [| n2ell; cast2|] "edge_push" builder;
      | SDEdgeC (n1, e, n2) -> let e' = expr builder e in 
          let n1p = expr builder n1 and
              n2p = expr builder n2 in 
          let fedgep = L.build_call edge_init_f
          [|e'; n2p; L.const_int i32_t 1 |] 
          "edge_init" builder and
              bedgep = L.build_call edge_init_f
          [|e'; n1p; L.const_int i32_t 0|] 
          "edge_init" builder in  
          let n1el = L.build_struct_gep n1p 2 "struct.ptr" builder and
            n2el = L.build_struct_gep n2p 2 "struct.prt" builder in 
          let n1ell = L.build_load n1el "temp" builder and
            n2ell = L.build_load n2el "temp" builder in
          let cast1 = L.build_bitcast fedgep void_ptr_t 
            "cast" builder and
            cast2 = L.build_bitcast bedgep void_ptr_t
            "cast" builder in
        ignore (L.build_call list_push_back_f 
            [| n1ell; cast1|] "edge_push" builder);
        L.build_call list_push_back_f
            [| n2ell; cast2|] "edge_push" builder;
      
      | SUEdge (n1, n2) -> let n1p = expr builder n1 and
                               n2p = expr builder n2 in 
          let fedgep = L.build_call edge_init_f
          [|L.const_int i32_t 0; n2p; L.const_int i32_t 1 |] 
          "edge_init" builder and
              bedgep = L.build_call edge_init_f
          [|L.const_int i32_t 0; n1p; L.const_int i32_t 1|] 
          "edge_init" builder in  
          let n1el = L.build_struct_gep n1p 2 "struct.ptr" builder and
            n2el = L.build_struct_gep n2p 2 "struct.prt" builder in 
          let n1ell = L.build_load n1el "temp" builder and
            n2ell = L.build_load n2el "temp" builder in
          let cast1 = L.build_bitcast fedgep void_ptr_t 
            "cast" builder and
            cast2 = L.build_bitcast bedgep void_ptr_t
            "cast" builder in
        ignore (L.build_call list_push_back_f 
            [| n1ell; cast1|] "edge_push" builder);
        L.build_call list_push_back_f
            [| n2ell; cast2|] "edge_push" builder;
      | SUEdgeC (n1, e, n2) -> let e' = expr builder e in 
          let n1p = expr builder n1 and
              n2p = expr builder n2 in 
          let fedgep = L.build_call edge_init_f
          [|e'; n2p; L.const_int i32_t 1 |] 
          "edge_init" builder and
              bedgep = L.build_call edge_init_f
          [|e'; n1p; L.const_int i32_t 1|] 
          "edge_init" builder in  
          let n1el = L.build_struct_gep n1p 2 "struct.ptr" builder and
            n2el = L.build_struct_gep n2p 2 "struct.prt" builder in 
          let n1ell = L.build_load n1el "temp" builder and
            n2ell = L.build_load n2el "temp" builder in
          let cast1 = L.build_bitcast fedgep void_ptr_t 
            "cast" builder and
            cast2 = L.build_bitcast bedgep void_ptr_t
            "cast" builder in
        ignore (L.build_call list_push_back_f 
            [| n1ell; cast1|] "edge_push" builder);
        L.build_call list_push_back_f
            [| n2ell; cast2|] "edge_push" builder;
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
           let bool_val = expr builder predicate in
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
        let bool_val = expr pred_builder predicate in
  
      let merge_bb = L.append_block context "merge" the_function in
      ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
      L.builder_at_end context merge_bb
  
        (* Implement for loops as while loops *)
        | SFor (e1, e2, e3, body) -> stmt builder
        ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
        | SDeclare(A.List(_), s, _) -> 
            let ptr = L.build_call list_init_f [| |] "init_list" builder in
            ignore (L.build_store ptr (lookup s) builder); builder
        | SDeclare(A.Graph(_), s, _) -> 
            let ptr = L.build_call graph_init_f [| |] "init_graph" builder in
            ignore (L.build_store ptr (lookup s) builder); builder
        | SDeclare(A.Node(_), s, _) ->
          (* INIT EDGELIST HERE *)
            let ptr = L.build_call node_init_f [| |] "init_node" builder in
            ignore (L.build_store ptr (lookup s) builder); builder 
        | SDeclare(_, _, a) -> ignore(expr builder a); builder
        | SContinue | SBreak -> raise (Failure ("error: continue/break"))
        
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
