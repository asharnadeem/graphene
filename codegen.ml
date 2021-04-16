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
   (*(match L.type_by_name llm_graph "struct.node_int" with
      None -> raise (Failure "Missing implementation for struct node_int")
    | Some t -> t) *)
  and node_t  = L.pointer_type (match L.type_by_name llm_graph "struct.node" with
      None -> raise (Failure "Missing implementation for struct node")
    | Some t -> t)
  and lst_t      = L.pointer_type (match L.type_by_name llm_graph "struct.list" with
      None -> raise (Failure "Missing implementation for struct list")
    | Some t -> t)
  in
  

   (* Return the LLVM type for a Graphene type *)
  let rec ltype_of_typ = function
      A.Int     -> i32_t
    | A.String  -> string_t
    | A.Float   -> float_t
    | A.Void    -> void_t
    | A.Node _  -> node_t
    | A.List _  -> lst_t
  in

  (* let struct_map = 
    let struct_decl map sdecl =
      let name = sdecl.name
      and member_types = Array.of_list (List.map 
          (fun (t, _) -> ltype_of_typ t) sdecl.members) in 
      let stype = L.struct_type context member_types in
      StringMap.add name (stype, sdecl.members) map in
    List.fold_left struct_decl StringMap.empty built_in_structs
  in *)



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

  let printbig_t : L.lltype =
      L.function_type i32_t [| i32_t |] in
  let printbig_f : L.llvalue =
      L.declare_function "printbig" printbig_t the_module in

  let list_init_t : L.lltype = 
      L.function_type lst_t [| |] in
  let list_init_f : L.llvalue =
      L.declare_function "list_init" list_init_t the_module in

  let node_init_t : L.lltype = 
      L.function_type node_t [| |] in
  let node_init_f : L.llvalue =
      L.declare_function "node_init" node_init_t the_module in

  let node_idset_t : L.lltype = 
      L.function_type void_t [| node_t ; i32_t|] in
  let node_idset_f : L.llvalue =
      L.declare_function "node_idset" node_idset_t the_module in

  let node_valset_t : L.lltype = 
      L.function_type void_t [| node_t ; void_ptr_t|] in
  let node_valset_f : L.llvalue =
      L.declare_function "node_valset" node_valset_t the_module in

  let list_push_back_t : L.lltype = 
      L.function_type i32_t [|lst_t ; i32_t; i32_t|] in
  let list_push_back_f : L.llvalue =
      L.declare_function "list_push_back" list_push_back_t the_module in

  let list_index_t : L.lltype =
      L.function_type i32_t [|lst_t ; i32_t; i32_t|] in  
  let list_index_f : L.llvalue =
      L.declare_function "list_index" list_index_t the_module in
   (* let make_list_t = L.function_type lst_t [||] in
  let make_list_func = L.declare_function "make_list" make_list_t the_module in

  let list_index_t = L.function_type i32_t [| lst_t; void_ptr_t |] in
  let list_index_func = L.declare_function "index" list_index_t the_module in

  let list_push_back_t = L.function_type i32_t [| lst_t; void_ptr_t |] in
  let list_push_back_func = L.declare_function "push_back" list_push_back_t the_module in

  let list_push_back_int_t = L.function_type i32_t [| lst_t; i32_t |] in
  let list_push_back_int_func = L.declare_function "push_back_int" list_push_back_int_t the_module in

  let list_push_back_float_t = L.function_type float_t [| lst_t; float_t |] in
  let list_push_back_float_func = L.declare_function "push_back_int" list_push_back_int_t the_module in  *)

  (* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
     let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
      let function_decl m fdecl =
        let name = fdecl.sfname
        and formal_types = 
    Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
        in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
        StringMap.add name (L.define_function name ftype the_module, fdecl) m in
      List.fold_left function_decl StringMap.empty functions in
    
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
        (* | SListLit l -> let rec list_fill lst = (function
        [] -> lst
        | sx :: rest ->
        let (t, _) = sx in 
        let data = (match t with
          A.List _ -> expr builder sx 
          | _ -> let data = L.build_malloc (ltype_of_typ t) "data" builder in
            let llvm =  expr builder sx 
            in ignore(L.build_store llvm data builder); data)
        in let data = L.build_bitcast data void_ptr_t "data" builder in
          ignore(L.build_call list_push_back_func [| lst; data |] "list_push_back" builder); list_fill lst rest) in
        let m = L.build_call make_list_func [||] "make_list" builder in
        list_fill m l *)
        (* | SIndex(l, e) ->
          let ltype = ltype_of_typ fdecl.styp in
          let lst = expr builder l in
          let index = expr builder e in
          let data = L.build_call list_index_func [| lst; index |] "index" builder in
            (match fdecl.styp with 
            A.Int -> L.build_bitcast data ltype "data" builder
            | _ -> let data = L.build_bitcast data (L.pointer_type ltype) "data" builder in
              L.build_load data "data" builder) *)

        (* | SList_Push_Back(l, e) -> let r = (match get_type(e) with
			    A.Int -> let l' = expr builder l and e' = expr builder e in
				  L.build_call list_push_back_int_func [|l'; e'|] "push_back_int" builder;
			    | A.Float -> let l' = expr builder l and e' = expr builder e in
				  L.build_call list_push_back_float_func [|l'; e'|] "push_back_float" builder;
          | _ -> raise(Failure("not valid list type"))) in
          r *)
        | SAssignField (x, s, e) -> let e' = expr builder e in 
            (match s with
              "id" -> 
              let str = L.build_load (lookup x) "struct" builder in
              let p = L.build_struct_gep str 0 "struct.ptr" builder in
            ignore(L.build_store e' p builder); e'
            | "val" -> let str = L.build_load (lookup x) "struct" builder in
            let val_ptr = L.build_struct_gep str 1 "struct.ptr" builder in
            let ty = ltype_of_typ expr_typ in
            let cast = L.build_bitcast val_ptr (L.pointer_type ty) "val" builder in 
            ignore (L.build_store e' cast builder); e'
            | _ -> raise (Failure "this should never happen, field error missed in semant"))
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
          raise (Failure "internal error: semant should have rejected and/or on float")
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
      | SCall ("printbig", [e]) ->
	  L.build_call printbig_f [| (expr builder e) |] "printbig" builder
      | SCall ("printf", [e]) -> 
	  L.build_call printf_f [| float_format_str ; (expr builder e) |]
	    "printf" builder
      | SCall ("list_push_back", [(t,l); e]) -> let list_type = match t with
          A.List(A.Int) -> L.const_int i32_t 1
        | A.List(A.Float) -> L.const_int i32_t 2
        | _ -> L.const_int (ltype_of_typ t) 0
        in 



      (* let l_ptr = expr builder l in
let e_val = expr builder e in
let n = idtostring l in
let l_type = getListType (lookup_types n) in ( match l_type with
let pqptr = expr builder p in let e_val = expr builder e in ignore (L.build_call pushPQ_f pqptr
[| pqptr; e_val |] "" builder);
A.String -> ignore(L.build_call addList_f [| l_ptr; e_val |] "" builder); l_ptr
| _ ->
let d_ltyp = L.type_of e_val in
let d_ptr = L.build_malloc d_ltyp "tmp" builder in ignore(L.build_store e_val d_ptr builder);
let void_e_ptr = L.build_bitcast d_ptr (L.pointer_type i8_t) "ptr" builder in
ignore (L.build_call addList_f [| l_ptr ; void_e_ptr |] "" l_ptr
) *)


    L.build_call list_push_back_f [| expr builder (t, l); expr builder e; list_type |] "list_push_back" builder
      | SCall ("list_index", [(t, l); e]) -> let list_type = match t with
          A.List(A.Int) -> L.const_int i32_t 1
        | A.List(A.Float) -> L.const_int i32_t 2
        | _ -> L.const_int i32_t 0 in
    L.build_call list_index_f [|expr builder (t, l); expr builder e; list_type|] "list_index" builder
      | SCall (f, args) ->
           let (fdef, fdecl) = StringMap.find f function_decls in
     let llargs = List.rev (List.map (expr builder) (List.rev args)) in
     let result = (match fdecl.styp with 
                          A.Void -> ""
                        | _ -> f ^ "_result") in
           L.build_call fdef (Array.of_list llargs) result builder
      | SAccess (s, x) -> 

      (* let rec get_idx n lst i = match lst with 
            [] -> raise (Failure("CODEGEN: id " ^ m ^ 
                          " is not a member of struct " ^ A.string_of_expr s))
          | h::t -> if (h = n) then i else get_idx n t (i+1)
          in let idx = (get_idx m (List.map (fun (_,nm,_,_) -> nm) members) 0) in
let ptr = L.build_struct_gep location idx ("struct.ptr") builder in L.build_load ptr ("struct.val."^m) builder
       *)

          (match x with
            "id" -> (let str = L.build_load (lookup s) "struct" builder in 
            let p = L.build_struct_gep str 0 "struct.ptr" builder in
          L.build_load p ("struct.val." ^ x) builder)
          | "val" -> let str = L.build_load (lookup s) "struct" builder in 
          (let void_ptr = L.build_struct_gep str 1 "struct.ptr" builder in
          let ty = ltype_of_typ expr_typ in
          let cast = L.build_bitcast void_ptr (L.pointer_type ty) "cast" builder in
          L.build_load cast ("struct.val." ^ x ^ ".value") builder))
      in
      



      (* let v_pointer = L.build_call getData_f [| e_val |]
"getData" builder in
let l_dtyp = ltype_of_typ n_type in
let d_ptr = L.build_bitcast v_pointer (L.pointer_type l_dtyp) "d_ptr" builder in
(L.build_load d_ptr "d_ptr" builder) *)
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
        | SDeclare(A.List(t), s, _) -> 
            let ptr = L.build_call list_init_f [| |] "init_list" builder in
            ignore (L.build_store ptr (lookup s) builder); builder
        | SDeclare(A.Node(t), s, _) ->
            let ptr = L.build_call node_init_f [| |] "init_node" builder in
            ignore (L.build_store ptr (lookup s) builder); builder 
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
