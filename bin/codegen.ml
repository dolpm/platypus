module L = Llvm
module A = Ast
open Sast
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

(* Translates SAST into LLVM module or throws error *)
let translate (things, pipes) ownership_map m_external =
  let context = L.global_context () in

  let i32_t = L.i32_type context
  and i8_t = L.i8_type context
  and i1_t = L.i1_type context
  and float_t = L.double_type context
  and unit_t = L.void_type context
  and string_t = L.pointer_type (L.i8_type context)
  and the_module = L.create_module context "Platypus" in

  let vector_t =
    match L.type_by_name m_external "struct.Vector" with
    | None -> raise (Failure "Vector struct not defined")
    | Some x -> x
  in

  let thing_types = ref StringMap.empty in

  (* Convert Platypus types to LLVM types *)
  (* val ltype_of_typ : defined_type -> lltype *)
  let rec ltype_of_typ t =
    match t with
    | A.Int -> i32_t
    | A.Float -> float_t
    | A.Bool -> i1_t
    | A.Tuple ts ->
        L.pointer_type
          (L.struct_type context (Array.of_list (List.map ltype_of_typ ts)))
    | A.Unit -> unit_t
    | A.Char -> i8_t
    | A.String | A.Str -> string_t
    | A.Box t -> L.pointer_type (ltype_of_typ t)
    | A.Borrow (t, _) -> L.pointer_type (ltype_of_typ t)
    | A.MutBorrow (t, _) -> L.pointer_type (ltype_of_typ t)
    | A.Generic -> L.pointer_type i8_t
    | A.Vector _ -> L.pointer_type vector_t
    | A.Ident s ->
        if StringMap.mem s !thing_types then
          L.pointer_type (StringMap.find s !thing_types)
        else raise (Failure ("unrecognized thing type " ^ s))
    | t ->
        raise
          (Failure ("Cannot convert type" ^ A.string_of_typ t ^ "to LLVM IR"))
  in

  let _ =
    List.iter
      (fun tdecl ->
        let as_struct_type = L.named_struct_type context tdecl.stname in
        let children_types =
          List.map
            (fun (_is_mut, typ, _name) -> ltype_of_typ typ)
            tdecl.selements
        in
        let _ =
          L.struct_set_body as_struct_type (Array.of_list children_types) false
        in
        thing_types := StringMap.add tdecl.stname as_struct_type !thing_types)
      (List.rev things)
  in

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |]
  in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module
  in

  let vector_new_t = L.function_type (ltype_of_typ (A.Vector A.Generic)) [||] in
  let vector_new_func =
    L.declare_function "Vector_new" vector_new_t the_module
  in

  let vector_push_t =
    L.function_type unit_t
      [| L.pointer_type vector_t; L.pointer_type (ltype_of_typ A.Generic) |]
  in
  let vector_push_func =
    L.declare_function "Vector_push" vector_push_t the_module
  in

  let vector_pop_t = L.function_type unit_t [| L.pointer_type vector_t |] in
  let vector_pop_func =
    L.declare_function "Vector_pop" vector_pop_t the_module
  in

  let vector_get_t =
    L.function_type
      (L.pointer_type (ltype_of_typ A.Generic))
      [| L.pointer_type vector_t; i32_t |]
  in
  let vector_get_func =
    L.declare_function "Vector_get" vector_get_t the_module
  in

  let vector_free_t = L.function_type unit_t [| L.pointer_type vector_t |] in
  let vector_free_func =
    L.declare_function "Vector_free" vector_free_t the_module
  in

  let str_new_t = L.function_type string_t [| string_t |] in
  let str_new_func = L.declare_function "Str_new" str_new_t the_module in

  let str_concat_t = L.function_type string_t [| string_t; string_t |] in
  let str_concat_func =
    L.declare_function "Str_concat" str_concat_t the_module
  in

  let str_push_t = L.function_type unit_t [| string_t; i8_t |] in
  let str_push_func = L.declare_function "Str_push" str_push_t the_module in

  let str_compare_t = L.function_type i1_t [| string_t; string_t |] in
  let str_compare_func =
    L.declare_function "Str_compare" str_compare_t the_module
  in

  (* Define all pipes declarations *)
  let pipe_decls : (L.llvalue * s_pipe_declaration) StringMap.t =
    let pipe_decl m pdecl =
      let name = pdecl.sname
      and formal_types =
        Array.of_list
          (List.map (fun (_, t, _) -> ltype_of_typ t) pdecl.sformals)
      in
      let ptype =
        L.function_type (ltype_of_typ pdecl.sreturn_type) formal_types
      in
      StringMap.add name (L.define_function name ptype the_module, pdecl) m
    in
    List.fold_left pipe_decl StringMap.empty pipes
  in

  (* Fill the body of a pipe by with local and formal bindings *)
  let build_pipe_body pdecl =
    let the_pipe, _ = StringMap.find pdecl.sname pipe_decls in
    let builder = L.builder_at_end context (L.entry_block the_pipe) in

    let newline_str = L.build_global_stringptr "%s\n" "fmt_nl" builder
    and int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in

    let variables =
      let add_formal m (_is_mut, t, n) p =
        let () = L.set_value_name n p in
        let local = L.build_alloca (ltype_of_typ t) n builder in
        let _ = L.build_store p local builder in
        StringMap.add n (local, t) m
      in
      (* Create appropriate llvalues for each function formal, i.e. create a value and alloc call *)
      ref
        (List.fold_left2 add_formal StringMap.empty pdecl.sformals
           (Array.to_list (L.params the_pipe)))
    in

    let add_local_variable (_is_mut, t, n) e' =
      let local = L.build_alloca (ltype_of_typ t) n builder in
      let _ = L.build_store e' local builder in
      variables := StringMap.add n (local, t) !variables
    in

    let rec expr (builder : L.llbuilder) ((_, e) : s_expr) : L.llvalue =
      match e with
      | SIntLiteral i -> L.const_int i32_t i
      | SFloatLiteral f -> L.const_float_of_string float_t f
      | SBoolLiteral b -> L.const_int i1_t (if b then 1 else 0)
      | SCharLiteral c -> L.const_int i8_t (int_of_char c)
      | SUnitLiteral -> L.const_null unit_t
      | SStringLiteral s -> L.build_global_stringptr s "strptr" builder
      | SThingValue (t_name, children) ->
          let ttyp = StringMap.find t_name !thing_types in

          let ptr = L.build_alloca ttyp (t_name ^ "_ptr") builder in

          (* get llv's of elems *)
          let elems = List.map (fun (_c_name, e) -> expr builder e) children in

          (* create struct with elems *)
          (* let struct_v = L.const_named_struct ttyp (Array.of_list elems) in *)
          let _ =
            List.fold_left
              (fun idx elem ->
                let ep =
                  L.build_struct_gep ptr idx
                    (t_name ^ "." ^ string_of_int idx)
                    builder
                in
                let _ = L.build_store elem ep builder in
                idx + 1)
              0 elems
          in

          ptr
      | SThingAccess (t, instance_of_t, access_list) ->
          let rec find_elem_index n lst =
            match lst with
            | [] -> raise (Failure "Not Found")
            | (_, _, h) :: t -> if n = h then 0 else 1 + find_elem_index n t
          in

          let _typ_of_elem, casted_gep =
            List.fold_left
              (fun ((thing_typ : A.defined_type), instance) elem_to_match ->
                let thing_name =
                  match thing_typ with
                  | Ident t -> t
                  | _ -> raise (Failure "panic! not possible")
                in

                let loaded_instance =
                  L.build_load instance "instance_of_struct" builder
                in

                let elem_types =
                  (List.find (fun t -> t.stname = thing_name) things).selements
                in
                let idx = find_elem_index elem_to_match elem_types in
                let _, elem_typ, _ =
                  List.find (fun (_, _, n) -> n = elem_to_match) elem_types
                in

                let gepped =
                  L.build_struct_gep loaded_instance idx "gep_on_instance"
                    builder
                in

                let casted_gep =
                  L.build_bitcast gepped
                    (L.pointer_type (ltype_of_typ elem_typ))
                    "casted_gep" builder
                in

                (elem_typ, casted_gep))
              (Ident t, fst (StringMap.find instance_of_t !variables))
              access_list
          in

          casted_gep
      | SBinop (e1, op, e2) -> (
          let t, _ = e1 and e1' = expr builder e1 and e2' = expr builder e2 in
          match t with
          | A.Int ->
              (match op with
              | A.Add -> L.build_add
              | A.Sub -> L.build_sub
              | A.Mult -> L.build_mul
              | A.Div -> L.build_sdiv
              | A.Lt -> L.build_icmp L.Icmp.Slt
              | A.Leq -> L.build_icmp L.Icmp.Sle
              | A.Gt -> L.build_icmp L.Icmp.Sgt
              | A.Geq -> L.build_icmp L.Icmp.Sge
              | A.Equal -> L.build_icmp L.Icmp.Eq
              | A.Neq -> L.build_icmp L.Icmp.Ne
              | _ -> raise (Failure "this operation is not supported"))
                e1' e2' "tmp" builder
          | A.Float ->
              (match op with
              | A.Add -> L.build_fadd
              | A.Sub -> L.build_fsub
              | A.Mult -> L.build_fmul
              | A.Div -> L.build_fdiv
              | A.Lt -> L.build_fcmp L.Fcmp.Olt
              | A.Leq -> L.build_fcmp L.Fcmp.Ole
              | A.Gt -> L.build_fcmp L.Fcmp.Ogt
              | A.Geq -> L.build_fcmp L.Fcmp.Oge
              | A.Equal -> L.build_fcmp L.Fcmp.Oeq
              | A.Neq -> L.build_fcmp L.Fcmp.One
              | _ -> raise (Failure "this operation is not supported"))
                e1' e2' "tmp" builder
          | A.Bool ->
              (match op with
              | A.And -> L.build_and
              | A.Or -> L.build_or
              | A.Equal -> L.build_icmp L.Icmp.Eq
              | A.Neq -> L.build_icmp L.Icmp.Ne
              | _ -> raise (Failure "this operation is not supported"))
                e1' e2' "tmp" builder
          | String | Str -> (
              (* todo implement strings *)
              match op with
              | A.Equal ->
                  let result =
                    L.build_call str_compare_func
                      [| expr builder e1; expr builder e2 |]
                      "tmp" builder
                  in
                  L.build_icmp L.Icmp.Eq result (L.const_int i1_t 0) "tmp2"
                    builder
              | A.Neq ->
                  let result =
                    L.build_call str_compare_func
                      [| expr builder e1; expr builder e2 |]
                      "tmp" builder
                  in
                  L.build_icmp L.Icmp.Eq result (L.const_int i1_t 1) "tmp2"
                    builder
              | A.Concat ->
                  L.build_call str_concat_func
                    [| expr builder e1; expr builder e2 |]
                    "concatted_string" builder
              | _ -> raise (Failure "this operation is not supported"))
          | Char ->
              (match op with
              | A.Equal -> L.build_icmp L.Icmp.Eq
              | A.Neq -> L.build_icmp L.Icmp.Ne
              | _ -> raise (Failure "this operation is not supported"))
                e1' e2' "tmp" builder
          | _ -> raise (Failure "this operation is not supported"))
      | SUnop (op, (t, e)) -> (
          match op with
          | Deref ->
              let load_value = expr builder (t, e) in
              L.build_load load_value "derefed_value" builder
          | Ref | MutRef -> (
              match t with
              | Vector _ | Box _ -> expr builder (t, e)
              | _ ->
                  (* get a reference to the existing value *)
                  let v_name =
                    match e with
                    | SIdent n -> n
                    | _ -> raise (Failure "can't reference a non-ident!")
                  in

                  let reffed_value = fst (StringMap.find v_name !variables) in

                  reffed_value)
          | Neg ->
              let load_value = expr builder (t, e) in
              L.build_neg load_value "negated_value" builder
          | Not ->
              let load_value = expr builder (t, e) in
              L.build_not load_value "boolean_negated_value" builder)
      | SPipeIn ("Printnl", [ (t, sx) ]) -> (
          let arg = expr builder (t, sx) in
          match t with
          | Borrow (t, _) | MutBorrow (t, _) -> (
              let loaded_arg = L.build_load arg "printnl arg" builder in
              match t with
              | Int | Bool ->
                  L.build_call printf_func
                    [| int_format_str; loaded_arg |]
                    "printf" builder
              | Float ->
                  L.build_call printf_func
                    [| float_format_str; loaded_arg |]
                    "printf" builder
              | String | Str ->
                  L.build_call printf_func
                    [| newline_str; loaded_arg |]
                    "printf" builder
              | _ -> raise (Failure "panic! invalid arg type!"))
          | Int | Bool ->
              L.build_call printf_func [| int_format_str; arg |] "printf"
                builder
          | Float ->
              L.build_call printf_func
                [| float_format_str; arg |]
                "printf" builder
          | String ->
              L.build_call printf_func [| newline_str; arg |] "printf" builder
          | _ -> raise (Failure "panic! invalid printnl arg type!"))
      | SPipeIn ("Box_new", [ ((t, _e) as value) ]) ->
          (* evaluate the expression*)
          let e' = expr builder value in

          (* malloc the type *)
          let malloc_of_t =
            L.build_malloc (ltype_of_typ t) "malloc_of_t" builder
          in

          (* store the evaluated value in the malloc *)
          let _ = L.build_store e' malloc_of_t builder in

          malloc_of_t
      | SPipeIn ("Box_unbox", [ box ]) | SPipeIn ("Box_unbox_mut", [ box ]) ->
          expr builder box
      | SPipeIn ("Vector_new", []) ->
          L.build_call vector_new_func [||] "Vector_new" builder
      | SPipeIn ("Vector_push", [ vector; ((t, _e) as value) ]) ->
          (* Check if value is on heap or stack; if on stack, create void ptr of it before passing into alloc *)
          let e' = expr builder value in
          let elem_arg =
            match t with
            | A.Box _ -> e'
            | A.Vector _ ->
                let ptr_casted_to_void =
                  L.build_bitcast e' (L.pointer_type i8_t) "ptr_casted_to_void"
                    builder
                in
                let ref_of_ptr =
                  L.build_alloca (L.pointer_type i8_t) "ref_of_ptr" builder
                in
                let _ = L.build_store ptr_casted_to_void ref_of_ptr builder in

                ref_of_ptr
            | _ ->
                let malloc_of_t =
                  L.build_malloc (ltype_of_typ t) "malloc_of_t" builder
                in
                let _ = L.build_store e' malloc_of_t builder in

                let malloc_casted_to_void =
                  L.build_bitcast malloc_of_t (L.pointer_type i8_t)
                    "malloc_casted_to_void" builder
                in

                let ref_of_malloc =
                  L.build_alloca (L.pointer_type i8_t) "ref_of_malloc" builder
                in
                let _ =
                  L.build_store malloc_casted_to_void ref_of_malloc builder
                in

                ref_of_malloc
          in

          L.build_call vector_push_func
            [| expr builder vector; elem_arg |]
            "" builder
      | SPipeIn ("Vector_pop", [ vector ]) ->
          L.build_call vector_pop_func [| expr builder vector |] "" builder
      | SPipeIn ("Vector_get", [ (vt, v); index ]) ->
          let fetched_item =
            L.build_call vector_get_func
              [| expr builder (vt, v); expr builder index |]
              "vector_item" builder
          in
          let inner_type =
            match vt with
            | Borrow (Vector t, _) -> t
            | _ -> raise (Failure "panic!")
          in
          L.build_bitcast fetched_item
            (L.pointer_type (ltype_of_typ inner_type))
            "vector_item_as_type" builder
      | SPipeIn ("Vector_get_mut", [ (vt, v); index ]) ->
          let fetched_item =
            L.build_call vector_get_func
              [| expr builder (vt, v); expr builder index |]
              "vector_item" builder
          in
          let inner_type =
            match vt with
            | MutBorrow (Vector t, _) -> t
            | _ -> raise (Failure "panic!")
          in
          L.build_bitcast fetched_item
            (L.pointer_type (ltype_of_typ inner_type))
            "vector_item_as_type" builder
      | SPipeIn ("Str_new", [ str ]) ->
          L.build_call str_new_func
            [| expr builder str |]
            "mallocd_string" builder
      | SPipeIn ("Str_push", [ str; c ]) ->
          L.build_call str_push_func
            [|
              L.build_load (expr builder str) "loaded_str" builder;
              expr builder c;
            |]
            "" builder
      | SPipeIn (pname, args) ->
          let pdef, pdecl = StringMap.find pname pipe_decls in
          let llargs = List.rev (List.map (expr builder) (List.rev args)) in
          let result =
            match pdecl.sreturn_type with
            | A.Unit -> ""
            | _ -> pname ^ "_result"
          in
          L.build_call pdef (Array.of_list llargs) result builder
      | SIdent name ->
          L.build_load
            (fst (StringMap.find name !variables))
            (name ^ "_loaded") builder
      (* Dummy add instruction *)
      | _ -> L.build_add (L.const_int i32_t 0) (L.const_int i32_t 0) "" builder
    in

    let add_terminal (builder : L.llbuilder) instr : unit =
      match L.block_terminator (L.insertion_block builder) with
      | Some _ -> ()
      | None -> ignore (instr builder)
    in

    let return_value = ref None in

    let free (typ : A.defined_type) (llvalue : L.llvalue)
        (builder : L.llbuilder) =
      let rec free_inner typ llvalue builder (is_root : bool) =
        match typ with
        | A.Vector typ_inner ->
            let v_struct_llv = L.build_load llvalue "v_struct" builder in
            (* if inner type is on the heap, free the values individually *)
            (* before we free this value *)
            let builder =
              match typ_inner with
              | A.Vector _ | A.Box _ ->
                  let v_len_llv =
                    L.build_load
                      (L.build_struct_gep v_struct_llv 0 "v_len" builder)
                      "stored_v_len" builder
                  in

                  let cur_item = L.build_alloca i32_t "cur_item" builder in
                  let _ =
                    L.build_store (L.const_int i32_t 0) cur_item builder
                  in

                  let pred_bb = L.append_block context "free_while" the_pipe in
                  let _ = L.build_br pred_bb builder in

                  let body_bb =
                    L.append_block context "free_while_body" the_pipe
                  in
                  let body_builder = L.builder_at_end context body_bb in

                  (* fetch the item from the vector *)
                  let fetched_item =
                    L.build_call vector_get_func
                      [| v_struct_llv; L.build_load cur_item "x" body_builder |]
                      "vector_item" body_builder
                  in

                  (* cast the value to its type *)
                  let casted_value =
                    L.build_bitcast fetched_item (ltype_of_typ typ_inner)
                      "vector_item_as_type" body_builder
                  in

                  let ptr_to_inner =
                    L.build_alloca (ltype_of_typ typ_inner) "ptr_to_inner"
                      body_builder
                  in

                  let _ =
                    L.build_store casted_value ptr_to_inner body_builder
                  in

                  (* call free on this casted value *)
                  let body_builder' =
                    free_inner typ_inner ptr_to_inner body_builder false
                  in

                  (* increment iterator *)
                  let add =
                    L.build_add
                      (L.build_load cur_item "x" body_builder)
                      (L.const_int i32_t 1) "add" body_builder
                  in
                  let _ = L.build_store add cur_item body_builder in

                  let () = add_terminal body_builder' (L.build_br pred_bb) in

                  let pred_builder = L.builder_at_end context pred_bb in
                  let cmp_as_bool =
                    L.build_icmp L.Icmp.Slt
                      (L.build_load cur_item "x" pred_builder)
                      v_len_llv "free_loop_cond" pred_builder
                  in

                  let merge_bb = L.append_block context "free_merge" the_pipe in
                  let _ =
                    L.build_cond_br cmp_as_bool body_bb merge_bb pred_builder
                  in
                  L.builder_at_end context merge_bb
              | _ -> builder
            in

            (* free the vector contents by calling the helper *)
            let _ =
              L.build_call vector_free_func [| v_struct_llv |] "" builder
            in

            (* free the pointer itself, only needed at top level, otherwise we'll get a double-free *)
            let _ =
              if is_root then
                let _ = L.build_free v_struct_llv builder in
                ()
              else ()
            in
            builder
        | A.Box typ_inner ->
            (* load the malloc *)
            let box = L.build_load llvalue "box_malloc_to_free" builder in
            let builder' =
              match typ_inner with
              (* recursively free box internals *)
              | A.Vector _ | A.Box _ ->
                  (* store inner value in ptr on stack *)
                  let inner_ptr =
                    L.build_alloca (ltype_of_typ typ_inner) "box_inner_ptr"
                      builder
                  in

                  (* load the value inside of the malloc (i.e., boxed thing) *)
                  let _ =
                    L.build_store
                      (L.build_load box "box_malloc_inner" builder)
                      inner_ptr builder
                  in

                  let builder' = free_inner typ_inner inner_ptr builder true in
                  builder'
              | _ -> builder
            in

            let _ = L.build_free box builder' in

            builder'
        | A.Str ->
            (* load the malloc *)
            let str = L.build_load llvalue "str_malloc_to_free" builder in
            let _ = L.build_free str builder in
            builder
        | Ident thing_name ->
            let loaded_instance =
              L.build_load llvalue "instance_of_struct" builder
            in
            let _, builder' =
              List.fold_left
                (fun (idx, builder) (_, elem_typ, _) ->
                  let gepped =
                    L.build_struct_gep loaded_instance idx "gep_on_instance"
                      builder
                  in
                  let casted_gep =
                    L.build_bitcast gepped
                      (L.pointer_type (ltype_of_typ elem_typ))
                      "casted_gep" builder
                  in
                  let builder' = free_inner elem_typ casted_gep builder true in
                  (idx + 1, builder'))
                (0, builder)
                (List.find (fun t -> t.stname = thing_name) things).selements
            in
            builder'
        | _ -> builder
      in
      free_inner typ llvalue builder true
    in

    let rec stmt s dangling_owns builder =
      match s with
      | SBlock (sl, sblock_id) ->
          let block_deallocs =
            if StringMap.mem sblock_id ownership_map then
              StringMap.find sblock_id ownership_map
            else []
          in

          let dangling_owns' =
            StringSet.union dangling_owns (StringSet.of_list block_deallocs)
          in

          (* build inner stmts *)
          let builder' =
            List.fold_left
              (fun builder' s ->
                match !return_value with
                | None -> stmt s dangling_owns' builder'
                | Some _ -> builder')
              builder sl
          in

          let builder' = ref builder' in

          let _ =
            List.fold_left
              (fun is_done stmt ->
                let v_names =
                  StringSet.of_list
                    (List.map fst (StringMap.bindings !variables))
                in
                match stmt with
                | SPipeOut _ ->
                    let _ =
                      StringSet.iter
                        (fun n ->
                          let v, t = StringMap.find n !variables in
                          builder' := free t v !builder')
                        (StringSet.inter v_names dangling_owns')
                    in
                    true
                | _ -> is_done)
              false sl
          in

          (* If we've seen a return, build it now (after freeing*)
          let _ =
            match !return_value with
            | Some e ->
                let _ =
                  match pdecl.sreturn_type with
                  | A.Unit -> L.build_ret_void !builder'
                  | _ -> L.build_ret (expr builder e) !builder'
                in
                return_value := None
            | None ->
                List.iter
                  (fun n ->
                    let v, t = StringMap.find n !variables in
                    builder' := free t v !builder')
                  block_deallocs
          in

          !builder'
      | SExpr e ->
          let _ = expr builder e in
          builder
      | SPipeOut e ->
          let _ = return_value := Some e in
          builder
      | SAssign (is_mut, t, name, e) ->
          let e' = expr builder e in
          let _ = add_local_variable (is_mut, t, name) e' in
          builder
      | SReAssign (is_mutborrow, name, e) ->
          let e' = expr builder e in
          let llv = fst (StringMap.find name !variables) in

          (* TODO: FREE OLD VALUE *)

          (* if mutborrow, deref the ptr to update *)
          let _ =
            if is_mutborrow then
              let derefed = L.build_load llv "derefed_mutborrow" builder in
              let _ = L.build_store e' derefed builder in
              ()
            else
              let _ = L.build_store e' llv builder in
              ()
          in
          builder
      | SWhile (pred, body, s_returns) ->
          let pred_bb = L.append_block context "while" the_pipe in
          let _ = L.build_br pred_bb builder in

          let body_bb = L.append_block context "while_body" the_pipe in
          let while_builder =
            stmt body dangling_owns (L.builder_at_end context body_bb)
          in

          let _ =
            if not s_returns then
              add_terminal while_builder (L.build_br pred_bb)
            else ()
          in

          let pred_builder = L.builder_at_end context pred_bb in
          let bool_val = expr pred_builder pred in

          if not s_returns then
            let merge_bb = L.append_block context "merge" the_pipe in
            let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
            L.builder_at_end context merge_bb
          else
            let builder = L.builder_at_end context pred_bb in
            let _ = L.build_br body_bb builder in
            L.builder_at_end context body_bb
      | SLoop (e1, e2, i, e3, body, s_returns) ->
          let assn = SAssign (true, A.Int, i, e1) in
          let pred = (A.Bool, SBinop ((A.Int, SIdent i), Leq, e2)) in
          let step =
            SReAssign (false, i, (A.Int, SBinop ((A.Int, SIdent i), Add, e3)))
          in

          (* TODO: figure out the sblock_id's s.t. the loop owns i *)
          stmt
            (SBlock
               ( [
                   assn;
                   SWhile
                     ( pred,
                       SBlock
                         ((if s_returns then [ body ] else [ body; step ]), "-1"),
                       s_returns );
                 ],
                 "-1" ))
            dangling_owns builder
      | SIf (pred, s1, s2, s1_returns, s2_returns) ->
          let then_bb = L.append_block context "then" the_pipe in
          let then_builder =
            stmt s1 dangling_owns (L.builder_at_end context then_bb)
          in

          let else_bb = L.append_block context "else" the_pipe in
          let else_builder =
            stmt s2 dangling_owns (L.builder_at_end context else_bb)
          in

          let _ = L.build_cond_br (expr builder pred) then_bb else_bb builder in

          if (not s1_returns) || not s2_returns then
            let merge_bb = L.append_block context "merge" the_pipe in
            let branch_instr = L.build_br merge_bb in

            let () =
              if not s1_returns then add_terminal then_builder branch_instr
              else ()
            in

            let () =
              if not s2_returns then add_terminal else_builder branch_instr
              else ()
            in

            L.builder_at_end context merge_bb
          else builder
    in

    let _builder = stmt (SBlock (pdecl.sbody, "-1")) StringSet.empty builder in
    ()
  in
  let _ = List.iter build_pipe_body pipes in
  the_module
