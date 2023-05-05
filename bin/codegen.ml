(* Dylan M. | Ronit S. | Tony H. | Rodrigo C. *)
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
          (L.packed_struct_type context
             (Array.of_list (List.map ltype_of_typ ts)))
    | A.Unit -> unit_t
    | A.Char -> i8_t
    | A.String | A.Str -> string_t
    | A.Box t | A.Borrow (t, _) | A.MutBorrow (t, _) ->
        L.pointer_type (ltype_of_typ t)
    | A.Generic -> L.pointer_type i8_t
    | A.Vector _ -> L.pointer_type vector_t
    | A.Ident s ->
        if StringMap.mem s !thing_types then
          L.pointer_type (StringMap.find s !thing_types)
        else raise (Failure ("unrecognized thing type " ^ s))
  in

  (* create user-defined "thing" types *)
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

  let noop_t = Llvm.function_type unit_t [||] in
  let noop_func = L.declare_function "llvm.donothing" noop_t the_module in

  let stacksave_t = Llvm.function_type (L.pointer_type i8_t) [||] in
  let stacksave_func =
    Llvm.declare_function "llvm.stacksave" stacksave_t the_module
  in

  let stackrestore_t = Llvm.function_type unit_t [| L.pointer_type i8_t |] in
  let stackrestore_func =
    Llvm.declare_function "llvm.stackrestore" stackrestore_t the_module
  in

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |]
  in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module
  in

  let sprintf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t; L.pointer_type i8_t |]
  in
  let sprintf_func : L.llvalue =
    L.declare_function "sprintf" sprintf_t the_module
  in

  let memcpy_t : L.lltype =
    L.function_type unit_t
      [| L.pointer_type i8_t; L.pointer_type i8_t; L.i64_type context |]
  in
  let _memcpy_func : L.llvalue =
    L.declare_function "memcpy" memcpy_t the_module
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
    L.function_type (ltype_of_typ A.Generic)
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

  let str_clone_t = L.function_type string_t [| string_t |] in
  let str_clone_func = L.declare_function "Str_clone" str_clone_t the_module in

  let rng_init_t = L.function_type unit_t [| i32_t |] in
  let rng_init_func = L.declare_function "Rng_init" rng_init_t the_module in

  let rng_generate_t = L.function_type i32_t [| i32_t; i32_t |] in
  let rng_generate_func =
    L.declare_function "Rng_generate" rng_generate_t the_module
  in

  let panic_t = L.function_type unit_t [| i32_t |] in
  let panic_func = L.declare_function "exit" panic_t the_module in

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

    let nonewline_str = L.build_global_stringptr "%s" "fmt_str_nnl" builder
    and nonewline_int_format_str =
      L.build_global_stringptr "%d" "fmt_int_nnl" builder
    and nonewline_char_format_str =
      L.build_global_stringptr "%c" "fmt_char_nnl" builder
    and nonewline_float_format_str =
      L.build_global_stringptr "%g" "fmt_float_nnl" builder
    and newline_str = L.build_global_stringptr "%s\n" "fmt_str_nl" builder
    and empty_str = L.build_global_stringptr "" "empty_str" builder in

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

    let add_local_variable (_is_mut, t, n) e' builder =
      let local = L.build_alloca (ltype_of_typ t) n builder in
      let _ = L.build_store e' local builder in
      variables := StringMap.add n (local, t) !variables
    in

    let add_terminal (builder : L.llbuilder) instr : unit =
      match L.block_terminator (L.insertion_block builder) with
      | Some _ -> ()
      | None -> ignore (instr builder)
    in

    let clone (typ : A.defined_type) (llvalue : L.llvalue)
        (builder : L.llbuilder) =
      let rec clone_inner typ llvalue builder (_is_root : bool) =
        match typ with
        | A.Vector inner_typ ->
            (* Vector we'll push the cloned items into *)
            let new_vector_generic =
              L.build_call vector_new_func [||] "Vector_new" builder
            in
            let new_vector_casted =
              L.build_bitcast new_vector_generic (ltype_of_typ typ)
                "new_vector_casted" builder
            in

            let cloned_vector =
              let vector_length =
                L.build_load
                  (L.build_struct_gep llvalue 0 "v_len_ptr" builder)
                  "vector_length" builder
              in

              (* Index of our for loop *)
              let index = L.build_alloca i32_t "cur_index" builder in
              let _ = L.build_store (L.const_int i32_t 0) index builder in

              (* start while loop *)
              let pred_bb = L.append_block context "clone_while" the_pipe in
              let _ = L.build_br pred_bb builder in

              let body_bb =
                L.append_block context "clone_while_body" the_pipe
              in
              let _ = L.position_at_end body_bb builder in

              (* save stackptr before building body *)
              let stackptr_prebody =
                L.build_call stacksave_func [||] "stackptr_prebody" builder
              in

              (* get item from vector *)
              let fetched_item =
                L.build_call vector_get_func
                  [| llvalue; L.build_load index "i" builder |]
                  "vector_item" builder
              in

              let casted_value =
                L.build_bitcast fetched_item
                  (L.pointer_type (ltype_of_typ inner_typ))
                  "vector_item_casted" builder
              in

              let malloc =
                L.build_malloc (ltype_of_typ inner_typ) "cloned_value" builder
              in

              let cloned_child =
                match inner_typ with
                | A.Vector _ | A.Box _ | A.Str ->
                    clone_inner inner_typ
                      (L.build_load casted_value "loaded_diaper" builder)
                      builder false
                | _ -> L.build_load casted_value "loaded_value" builder
              in

              let _ = L.build_store cloned_child malloc builder in

              let ptr_of_cloned_child =
                L.build_alloca (ltype_of_typ A.Generic) "ptr_of_cloned_child"
                  builder
              in

              let _ =
                L.build_store
                  (L.build_bitcast malloc (ltype_of_typ A.Generic)
                     "casted_to_push" builder)
                  ptr_of_cloned_child builder
              in

              let _ =
                L.build_call vector_push_func
                  [| new_vector_casted; ptr_of_cloned_child |]
                  "" builder
              in

              (* increment iterator *)
              let add =
                L.build_add
                  (L.build_load index "x" builder)
                  (L.const_int i32_t 1) "add" builder
              in
              let _ = L.build_store add index builder in

              let _ =
                L.build_call stackrestore_func [| stackptr_prebody |] "" builder
              in
              let () = add_terminal builder (L.build_br pred_bb) in

              let _ = L.position_at_end pred_bb builder in
              let cmp_as_bool =
                L.build_icmp L.Icmp.Slt
                  (L.build_load index "x" builder)
                  vector_length "clone_loop_cond" builder
              in

              let merge_bb = L.append_block context "clone_merge" the_pipe in
              let _ = L.build_cond_br cmp_as_bool body_bb merge_bb builder in

              let _ = L.position_at_end merge_bb builder in

              new_vector_casted
            in

            cloned_vector
        | A.Box typ_inner ->
            let cloned_child =
              match typ_inner with
              (* recursively clone box internals *)
              | A.Vector _ | A.Box _ | A.Str ->
                  (* get the inner value from the box and recurse on it *)
                  let cloned_box =
                    clone_inner typ_inner
                      (L.build_load llvalue "box_malloc_inner" builder)
                      builder true
                  in
                  cloned_box
              | _ -> L.build_load llvalue "loaded_inner" builder
            in

            (* malloc the new value *)
            let malloc_of_box =
              L.build_malloc (ltype_of_typ typ_inner) "cloned_value" builder
            in

            let _ = L.build_store cloned_child malloc_of_box builder in

            malloc_of_box
        | A.Ident thing_name ->
            let new_instance =
              L.build_alloca
                (StringMap.find thing_name !thing_types)
                "new_thing_instance" builder
            in

            let _ =
              List.fold_left
                (fun idx (_, elem_typ, _) ->
                  let gepped =
                    L.build_struct_gep llvalue idx "gep_on_instance" builder
                  in

                  let casted_gep =
                    L.build_bitcast gepped
                      (L.pointer_type (ltype_of_typ elem_typ))
                      "casted_gep" builder
                  in

                  let loaded_cast = L.build_load casted_gep "tmp" builder in

                  let cloned_value =
                    match elem_typ with
                    | A.Vector _ | A.Box _ | A.Str | A.Ident _ ->
                        clone_inner elem_typ loaded_cast builder true
                    | _ -> loaded_cast
                  in

                  let new_gepped =
                    L.build_struct_gep new_instance idx "gep_on_new_instance"
                      builder
                  in

                  let _ = L.build_store cloned_value new_gepped builder in

                  idx + 1)
                0 (List.find (fun t -> t.stname = thing_name) things).selements
            in
            new_instance
        | A.Tuple typs ->
            let built_types = List.map (fun t -> ltype_of_typ t) typs in
            (* create the packed struct type and allocate space for it *)
            let types_as_packed =
              L.packed_struct_type context (Array.of_list built_types)
            in

            let ptr = L.build_alloca types_as_packed "tuple_ptr" builder in
            (* access and store the values in the struct *)
            let _ =
              List.fold_left
                (fun idx t ->
                  let old_ep =
                    L.build_struct_gep llvalue idx
                      ("tuple-gep." ^ string_of_int idx)
                      builder
                  in

                  let casted_gep =
                    L.build_bitcast old_ep
                      (L.pointer_type (ltype_of_typ t))
                      "casted_gep" builder
                  in

                  let loaded_cast = L.build_load casted_gep "tmp" builder in

                  let cloned_value =
                    match t with
                    | A.Vector _ | A.Box _ | A.Str | A.Ident _ ->
                        clone_inner t loaded_cast builder true
                    | _ -> loaded_cast
                  in

                  let new_ep =
                    L.build_struct_gep ptr idx
                      ("tuple-gep." ^ string_of_int idx)
                      builder
                  in

                  let _ = L.build_store cloned_value new_ep builder in
                  idx + 1)
                0 typs
            in

            ptr
        | A.Str ->
            L.build_call str_clone_func [| llvalue |] "cloned_string" builder
        | _ ->
            (* Store value into pointer *)
            let clone_ptr =
              L.build_alloca (ltype_of_typ typ) "clone_ptr" builder
            in
            let _ = L.build_store llvalue clone_ptr builder in
            (* Load ptr *)
            L.build_load clone_ptr "clone_val" builder
      in
      clone_inner typ llvalue builder true
    in

    let free (typ : A.defined_type) (llvalue : L.llvalue)
        (builder : L.llbuilder) =
      let rec free_inner typ llvalue builder =
        match typ with
        | A.Vector typ_inner ->
            let v_struct_llv = L.build_load llvalue "v_struct" builder in
            (* if inner type is on the heap, free the values individually *)
            (* before we free this value *)
            let builder =
              match typ_inner with
              (* Make sure str should be here *)
              | A.Vector _ | A.Box _ | A.Str ->
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
                  (* save stackptr before building body *)
                  let stackptr_prebody =
                    L.build_call stacksave_func [||] "stackptr_prebody"
                      body_builder
                  in

                  (* fetch the item from the vector *)
                  let fetched_item =
                    L.build_call vector_get_func
                      [| v_struct_llv; L.build_load cur_item "x" body_builder |]
                      "vector_item" body_builder
                  in

                  (* cast the value to its type *)
                  let ptr_to_inner =
                    L.build_bitcast fetched_item
                      (L.pointer_type (ltype_of_typ typ_inner))
                      "vector_item_as_type" body_builder
                  in

                  (* call free on this casted value *)
                  let body_builder' =
                    free_inner typ_inner ptr_to_inner body_builder
                  in

                  (* increment iterator *)
                  let add =
                    L.build_add
                      (L.build_load cur_item "x" body_builder)
                      (L.const_int i32_t 1) "add" body_builder
                  in
                  let _ = L.build_store add cur_item body_builder in

                  let _ =
                    L.build_call stackrestore_func [| stackptr_prebody |] ""
                      body_builder
                  in
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

                  let builder' = free_inner typ_inner inner_ptr builder in
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
                  let builder' = free_inner elem_typ casted_gep builder in
                  (idx + 1, builder'))
                (0, builder)
                (List.find (fun t -> t.stname = thing_name) things).selements
            in
            builder'
        | Tuple inner_types ->
            let loaded_instance =
              L.build_load llvalue "instance_of_tuple" builder
            in
            let _, builder' =
              List.fold_left
                (fun (idx, builder) elem_typ ->
                  let gepped =
                    L.build_struct_gep loaded_instance idx "gep_on_instance"
                      builder
                  in
                  let casted_gep =
                    L.build_bitcast gepped
                      (L.pointer_type (ltype_of_typ elem_typ))
                      "casted_gep" builder
                  in
                  let builder' = free_inner elem_typ casted_gep builder in
                  (idx + 1, builder'))
                (0, builder) inner_types
            in
            builder'
        | _ -> builder
      in
      free_inner typ llvalue builder
    in

    let rec print_helper (t : A.defined_type) arg builder =
      match t with
      | Borrow (t, _) | MutBorrow (t, _) -> (
          let loaded_arg = L.build_load arg "print_arg" builder in
          match t with
          | Int | Bool | Float | String -> print_helper t loaded_arg builder
          | Str ->
              L.build_call printf_func
                [| nonewline_str; loaded_arg |]
                "printf" builder
          | _ -> raise (Failure "panic! invalid print arg type!"))
      | Int ->
          L.build_call printf_func
            [| nonewline_int_format_str; arg |]
            "printf" builder
      | Bool ->
          let pred =
            L.build_icmp L.Icmp.Eq (L.const_int i1_t 1) arg "tmp" builder
          in
          let then_bb = L.append_block context "then" the_pipe in
          let else_bb = L.append_block context "else" the_pipe in
          let merge_bb = L.append_block context "merge" the_pipe in

          let bool_string = L.build_alloca string_t "the_boiler_room" builder in

          let _ = L.build_cond_br pred then_bb else_bb builder in
          let branch_instr = L.build_br merge_bb in

          (* do then block *)
          let _ = L.position_at_end then_bb builder in
          let true_global =
            L.build_global_stringptr "true" "true_str" builder
          in
          let _ = L.build_store true_global bool_string builder in
          let _ = add_terminal builder branch_instr in

          (* do else block *)
          let _ = L.position_at_end else_bb builder in
          let false_global =
            L.build_global_stringptr "false" "false_str" builder
          in
          let _ = L.build_store false_global bool_string builder in
          let _ = add_terminal builder branch_instr in

          let _ = L.position_at_end merge_bb builder in

          L.build_call printf_func
            [| L.build_load bool_string "smorgasbord" builder; arg |]
            "printf" builder
      | Float ->
          L.build_call printf_func
            [| nonewline_float_format_str; arg |]
            "printf" builder
      | String ->
          L.build_call printf_func [| nonewline_str; arg |] "printf" builder
      | _ -> raise (Failure "panic! invalid print arg type!")
    in

    let rec expr (builder : L.llbuilder) ((t, e) : s_expr) : L.llvalue =
      match e with
      | SIntLiteral i -> L.const_int i32_t i
      | SFloatLiteral f -> L.const_float_of_string float_t f
      | SBoolLiteral b -> L.const_int i1_t (if b then 1 else 0)
      | SCharLiteral c -> L.const_int i8_t (int_of_char c)
      | SUnitLiteral -> L.const_null i1_t
      | SStringLiteral s -> L.build_global_stringptr s "strptr" builder
      | STupleValue exprs ->
          (* build exprs, get thier types *)
          let built_exprs = List.map (fun e -> expr builder e) exprs in
          let built_types =
            List.map (fun e_llv -> L.type_of e_llv) built_exprs
          in
          (* create the packed struct type and allocate space for it *)
          let types_as_packed =
            L.packed_struct_type context (Array.of_list built_types)
          in
          let ptr = L.build_alloca types_as_packed "tuple_ptr" builder in
          (* access and store the values in the struct *)
          let _ =
            List.fold_left
              (fun idx e_llv ->
                let ep =
                  L.build_struct_gep ptr idx
                    ("tuple-gep." ^ string_of_int idx)
                    builder
                in
                let _ = L.build_store e_llv ep builder in
                idx + 1)
              0 built_exprs
          in
          ptr
      | STupleIndex (e, idx) ->
          let instance = expr builder e in

          let loaded_instance =
            L.build_load instance "instance_of_struct" builder
          in
          let gepped =
            L.build_struct_gep loaded_instance idx "gep_on_instance" builder
          in

          let casted_gep =
            L.build_bitcast gepped (ltype_of_typ t) "casted_gep" builder
          in
          casted_gep
      | SThingValue (t_name, children) ->
          let ttyp = StringMap.find t_name !thing_types in
          let ptr = L.build_alloca ttyp (t_name ^ "_ptr") builder in
          (* get llv's of elems *)
          let elems = List.map (fun (_c_name, e) -> expr builder e) children in
          (* create struct with elems *)
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
      | SThingAccess (thing_name, instance_of_t, elem_to_access) ->
          let rec find_elem_index n lst =
            match lst with
            | [] -> raise (Failure "Not Found")
            | (_, _, h) :: t -> if n = h then 0 else 1 + find_elem_index n t
          in

          let loaded_instance =
            L.build_load
              (expr builder instance_of_t)
              "instance_of_struct" builder
          in

          let elem_types =
            (List.find (fun t -> t.stname = thing_name) things).selements
          in
          let idx = find_elem_index elem_to_access elem_types in
          let _, elem_typ, _ =
            List.find (fun (_, _, n) -> n = elem_to_access) elem_types
          in

          let gepped =
            L.build_struct_gep loaded_instance idx "gep_on_instance" builder
          in

          let casted_gep =
            L.build_bitcast gepped
              (L.pointer_type (ltype_of_typ elem_typ))
              "casted_gep" builder
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
              match e with
              | SIdent n -> fst (StringMap.find n !variables)
              | SThingAccess _ | STupleIndex _ -> expr builder (t, e)
              | _ ->
                  raise
                    (Failure
                       "can only do references on idents, thing accesses, and \
                        tuple indexing!"))
          | Neg ->
              let load_value = expr builder (t, e) in
              L.build_neg load_value "negated_value" builder
          | Not ->
              let load_value = expr builder (t, e) in
              L.build_not load_value "boolean_negated_value" builder
          | Clone ->
              let load_value = expr builder (t, e) in
              clone t load_value builder)
      | SPipeIn ("Panic", [ msg ]) ->
          (* build instr to print the messgae *)
          let _ =
            L.build_call printf_func
              [|
                nonewline_str;
                L.build_global_stringptr "Panic! " "panic_strptr" builder;
              |]
              "printf" builder
          in
          let _ = expr builder (Unit, SPipeIn ("Printnl", [ msg ])) in
          L.build_call panic_func [| L.const_int i32_t 1 |] "" builder
      | SPipeIn ("Print", [ (t, sx) ]) ->
          let arg = expr builder (t, sx) in
          print_helper t arg builder
      | SPipeIn ("Printnl", [ (t, sx) ]) ->
          let arg = expr builder (t, sx) in
          let _ = print_helper t arg builder in
          L.build_call printf_func [| newline_str; empty_str |] "printf" builder
      | SPipeIn ("Rng_init", [ seed ]) ->
          L.build_call rng_init_func [| expr builder seed |] "" builder
      | SPipeIn ("Rng_generate", [ min; max ]) ->
          L.build_call rng_generate_func
            [| expr builder min; expr builder max |]
            "rng_generate" builder
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
          L.build_load (expr builder box) "unboxed_box" builder
      | SPipeIn ("Vector_new", []) ->
          L.build_call vector_new_func [||] "Vector_new" builder
      | SPipeIn ("Vector_length", [ vector ]) ->
          let loaded_vec =
            L.build_load (expr builder vector) "loaded_vec" builder
          in
          L.build_load
            (L.build_struct_gep loaded_vec 0 "v_len_ptr" builder)
            "vector_length" builder
      | SPipeIn ("Vector_push", [ vector; ((t, _e) as value) ]) ->
          let e' = expr builder value in

          let elem_arg =
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
            let _ = L.build_store malloc_casted_to_void ref_of_malloc builder in

            ref_of_malloc
          in

          let loaded_vec =
            L.build_load (expr builder vector) "loaded_vec" builder
          in

          L.build_call vector_push_func [| loaded_vec; elem_arg |] "" builder
      | SPipeIn ("Vector_pop", [ vector ]) ->
          let loaded_vec =
            L.build_load (expr builder vector) "loaded_vec" builder
          in
          L.build_call vector_pop_func [| loaded_vec |] "" builder
      | SPipeIn ("Vector_get", [ ((vt, _v) as vector); index ]) ->
          let loaded_vec =
            L.build_load (expr builder vector) "loaded_vec" builder
          in

          let fetched_item =
            L.build_call vector_get_func
              [| loaded_vec; expr builder index |]
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
      | SPipeIn ("Vector_update", [ ((vt, _v) as vector); index; updated_value ])
        ->
          let loaded_vec =
            L.build_load (expr builder vector) "loaded_vec" builder
          in

          let fetched_item =
            L.build_call vector_get_func
              [| loaded_vec; expr builder index |]
              "vector_item" builder
          in

          let inner_type =
            match vt with
            | MutBorrow (Vector t, _) -> t
            | _ -> raise (Failure "panic!")
          in

          let casted_item =
            L.build_bitcast fetched_item
              (L.pointer_type (ltype_of_typ inner_type))
              "vector_item_as_type" builder
          in

          (* free over-written value if on the heap *)
          let _ = free inner_type casted_item builder in

          L.build_store (expr builder updated_value) casted_item builder
      | SPipeIn ("Vector_get_mut", [ ((vt, _v) as vector); index ]) ->
          let loaded_vec =
            L.build_load (expr builder vector) "loaded_vec" builder
          in
          let fetched_item =
            L.build_call vector_get_func
              [| loaded_vec; expr builder index |]
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
              expr builder str;
              expr builder c;
            |]
            "" builder
      | SPipeIn ("Int", [ (t, e) ]) -> (
          let e' = expr builder (t, e) in
          match t with
          | Float -> L.build_fptosi e' i32_t "casted_to_int" builder
          | _ -> L.build_intcast e' i32_t "casted_to_int" builder)
      | SPipeIn ("Float", [ (t, e) ]) -> (
          let e' = expr builder (t, e) in
          match t with
          | Float -> e'
          | _ -> L.build_sitofp e' float_t "casted_to_float" builder)
      | SPipeIn ("Str", [ (t, e) ]) ->
          (* alloca a i8 array of length 24 *)
          (* https://stackoverflow.com/questions/1701055/what-is-the-maximum-length-in-chars-needed-to-represent-any-double-value *)
          let e' = expr builder (t, e) in
          let dest_array =
            L.build_array_alloca i8_t (L.const_int i32_t 24) "dog_thresher"
              builder
          in
          let dest_ptr =
            L.build_gep dest_array
              [| L.const_int i32_t 0 |]
              "tongue_of_steel" builder
          in
          (* match on t to sprintf with correct format string into array *)
          let ptr_for_str =
            match t with
            (* TODO print char as char instead of ASCII and bool as 'true' or 'false' instead of 1 or 0 *)
            (* do we need to build a fucking if statement here?! for bool to string I mean. *)
            | Bool ->
                let pred =
                  L.build_icmp L.Icmp.Eq (L.const_int i1_t 1) e' "tmp" builder
                in
                let then_bb = L.append_block context "then" the_pipe in
                let else_bb = L.append_block context "else" the_pipe in
                let merge_bb = L.append_block context "merge" the_pipe in

                let bool_string =
                  L.build_alloca string_t "the_boiler_room" builder
                in

                let _ = L.build_cond_br pred then_bb else_bb builder in
                let branch_instr = L.build_br merge_bb in

                (* do then block *)
                let _ = L.position_at_end then_bb builder in
                let true_global =
                  L.build_global_stringptr "true" "true_str" builder
                in
                let _ = L.build_store true_global bool_string builder in
                let _ = add_terminal builder branch_instr in

                (* do else block *)
                let _ = L.position_at_end else_bb builder in
                let false_global =
                  L.build_global_stringptr "false" "false_str" builder
                in
                let _ = L.build_store false_global bool_string builder in
                let _ = add_terminal builder branch_instr in

                let _ = L.position_at_end merge_bb builder in

                L.build_load bool_string "head_of_house" builder
            | Int ->
                let _ =
                  L.build_call sprintf_func
                    [| dest_ptr; nonewline_int_format_str; e' |]
                    "sprintf" builder
                in
                dest_ptr
            | Char ->
                let _ =
                  L.build_call sprintf_func
                    [| dest_ptr; nonewline_char_format_str; e' |]
                    "sprintf" builder
                in
                dest_ptr
            | Float ->
                let _ =
                  L.build_call sprintf_func
                    [| dest_ptr; nonewline_float_format_str; e' |]
                    "sprintf" builder
                in
                dest_ptr
            | _ ->
                raise
                  (Failure
                     ("unexpected type " ^ A.string_of_typ t
                    ^ " in Str cast pipe"))
          in
          (* call Str_new, passing in i8 array *)
          L.build_call str_new_func [| ptr_for_str |] "wrath_of_george" builder
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
      (* noop *)
      | _ -> Llvm.build_call noop_func [||] "" builder
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

          let builder' = ref builder in

          let ret_expr =
            List.fold_left
              (fun return_expr s ->
                let v_names =
                  StringSet.of_list
                    (List.map fst (StringMap.bindings !variables))
                in
                match s with
                | SPipeOut ((_t, e) as ret_expr) ->
                    (* Build return value register *)
                    let ret_value =
                      match ret_expr with
                      | A.Unit, SUnitLiteral -> None
                      | A.Unit, _ ->
                          (* build the unit expr *)
                          let _ = expr !builder' ret_expr in
                          None
                      | _ -> Some (expr !builder' ret_expr)
                    in
                    let _ =
                      StringSet.iter
                        (fun n ->
                          let v, t = StringMap.find n !variables in
                          builder' := free t v !builder')
                        (* if we return a value that would otherwise *)
                        (* need to be freed, make sure it doesn't get freed *)
                        (match e with
                        | SIdent n ->
                            StringSet.remove n
                              (StringSet.inter v_names dangling_owns')
                        | _ -> StringSet.inter v_names dangling_owns')
                    in
                    let _ =
                      match ret_value with
                      | None -> L.build_ret_void !builder'
                      | Some e -> L.build_ret e !builder'
                    in
                    Some ret_expr
                | s ->
                    let _ =
                      match return_expr with
                      | None -> builder' := stmt s dangling_owns' !builder'
                      | _ -> ()
                    in
                    return_expr)
              None sl
          in

          (* if no ret expression was found in block, free owned vars *)
          let _ =
            match ret_expr with
            | None ->
                List.iter
                  (fun n ->
                    let v, t = StringMap.find n !variables in
                    builder' := free t v !builder')
                  block_deallocs
            | Some _ -> ()
          in

          !builder'
      | SExpr e ->
          let _ = expr builder e in
          builder
      | SPipeOut _e ->
          (* let _ = return_value := Some e in *)
          builder
      | SAssign (is_mut, t, name, e) ->
          let e' = expr builder e in
          let _ = add_local_variable (is_mut, t, name) e' builder in
          builder
      | SReAssign (is_mutborrow, name, e) ->
          let e' = expr builder e in
          let llv, typ = StringMap.find name !variables in

          (* free over-written value *)
          let _ = free typ llv builder in

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
          let body_builder = L.builder_at_end context body_bb in

          (* save stackptr before building body *)
          let stackptr_prebody =
            L.build_call stacksave_func [||] "stackptr_prebody" body_builder
          in

          let while_builder = stmt body dangling_owns body_builder in

          let _ =
            if not s_returns then
              let _ =
                L.build_call stackrestore_func [| stackptr_prebody |] ""
                  while_builder
              in
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

          let _ =
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

              L.position_at_end merge_bb builder
          in
          builder
    in

    let _builder = stmt (SBlock (pdecl.sbody, "-1")) StringSet.empty builder in
    ()
  in
  let _ = List.iter build_pipe_body pipes in
  the_module
