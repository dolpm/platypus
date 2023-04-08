module L = Llvm
module A = Ast
open Sast
module StringMap = Map.Make (String)

(* Translates SAST into LLVM module or throws error *)
let translate (things, pipes) ownership_map the_module =
  let context = L.global_context () in

  let i32_t = L.i32_type context
  and i8_t = L.i8_type context
  and i1_t = L.i1_type context
  and float_t = L.double_type context
  and unit_t = L.void_type context
  and string_t = L.pointer_type (L.i8_type context) in

  let vector_t =
    match L.type_by_name the_module "struct.Vector" with
    | None -> raise (Failure "Vector struct not defined")
    | Some x -> x
  in

  (* Convert Platypus types to LLVM types *)
  (* val ltype_of_typ : defined_type -> lltype *)
  let rec ltype_of_typ = function
    | A.Int -> i32_t
    | A.Float -> float_t
    | A.Bool -> i1_t
    | A.Tuple ts ->
        L.pointer_type
          (L.struct_type context (Array.of_list (List.map ltype_of_typ ts)))
    | A.Unit -> unit_t
    | A.Char -> i8_t
    | A.String -> string_t
    | A.Box t -> L.pointer_type (ltype_of_typ t)
    | A.Borrow (t, _) -> L.pointer_type (ltype_of_typ t)
    | A.MutBorrow (t, _) -> L.pointer_type (ltype_of_typ t)
    | A.Generic -> L.pointer_type i8_t
    | A.Vector _ -> L.pointer_type vector_t
    | A.Ident _ -> string_t
    | t ->
        raise
          (Failure ("Cannot convert type" ^ A.string_of_typ t ^ "to LLVM IR"))
  in

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |]
  in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module
  in

  let vector_alloc_t =
    L.function_type (ltype_of_typ (A.Vector A.Generic)) [||]
  in
  let vector_alloc_func =
    L.declare_function "Vector_alloc" vector_alloc_t the_module
  in

  (*
  let vector_length_t = L.function_type i32_t [| L.pointer_type vector_t |] in
  let _vector_length_func =
    L.declare_function "Vector_length" vector_length_t the_module
  in
  *)
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

  (* Generating code for things. A stringmap of llvalues, where each llvalue is an initialized const_struct global variablle*)
  let _thing_decls : L.llvalue StringMap.t =
    let thing_decl m tdecl =
      let name = tdecl.stname in
      let init =
        let init_ele t =
          match t with
          | A.Int | A.Float | A.Bool | A.Char | A.Unit ->
              L.const_null (ltype_of_typ t)
          | A.Generic | A.Option _ ->
              raise
                (Failure
                   ("Cannot convert type" ^ A.string_of_typ t ^ "to LLVM IR"))
          | _ -> L.const_pointer_null (ltype_of_typ t)
        in
        L.const_struct context
          (Array.of_list
             (List.map (fun (_, t, _) -> init_ele t) tdecl.selements))
      in
      StringMap.add name (L.define_global name init the_module) m
    in
    List.fold_left thing_decl StringMap.empty things
  in
  (* let init_and_add ele =
     let rec init_ele = match (snd ele) with
       A.Float -> L.const_float (ltype_of_typ t) 0.0
       | _ -> raise (Failure ("TODO"))
     in
     StringMap.add ele_n init_ele *)
  (* in
     List.fold_left init_ele StringMap.empty ((ele_n, ele_t)::eles) *)
  (* and eles_map : L.lltype StringMap.t =
       let map_ele mem_m ele =
         StringMap.add (fst ele) (snd ele) mem_m
       in
       List.fold_left map_ele StringMap.empty (snd tdecl)
     in
     StringMap.add name eles_map m *)
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
        StringMap.add n local m
      in
      (* Create appropriate llvalues for each function formal, i.e. create a value and alloc call *)
      ref
        (List.fold_left2 add_formal StringMap.empty pdecl.sformals
           (Array.to_list (L.params the_pipe)))
    in

    let add_local_variable (_is_mut, t, n) e' =
      let local = L.build_alloca (ltype_of_typ t) n builder in
      let _ = L.build_store e' local builder in
      variables := StringMap.add n local !variables
    in

    (* lookup should be defined inside body? *)
    let rec expr (builder : L.llbuilder) ((_, e) : s_expr) : L.llvalue =
      match e with
      | SIntLiteral i -> L.const_int i32_t i
      (* | SFloatLiteral ->
         | SBoolLiteral ->
         | SCharLiteral -> *)
      | SUnitLiteral -> L.const_null unit_t
      | SStringLiteral s -> L.build_global_stringptr s "str" builder
      | SUnop (op, (t, e)) -> (
          match op with
          | Deref ->
              let load_value = expr builder (t, e) in
              L.build_load load_value "derefed_value" builder
          | Ref | MutRef -> (
              match t with
              | Vector _ | Box _ -> expr builder (t, e)
              | _ ->
                  let store_val = expr builder (t, e) in
                  let ref = L.build_alloca (ltype_of_typ t) "ref" builder in
                  let _ = L.build_store store_val ref builder in
                  ref)
          | _ -> expr builder (t, e))
      (* function call, takes in fn name and a list of inputs *)
      | SPipeIn ("printnl", [ (t, sx) ]) -> (
          match t with
          | Int ->
              L.build_call printf_func
                [| int_format_str; expr builder (t, sx) |]
                "printf" builder
          | Float ->
              L.build_call printf_func
                [| float_format_str; expr builder (t, sx) |]
                "printf" builder
          | _ ->
              L.build_call printf_func
                [| newline_str; expr builder (t, sx) |]
                "printf" builder)
      | SPipeIn ("Vector_alloc", []) ->
          L.build_call vector_alloc_func [||] "Vector_alloc" builder
      | SPipeIn ("Vector_push", [ vector; ((t, _e) as value) ]) ->
          (* Check if value is on heap or stack; if on stack, create void ptr of it before passing into alloc *)
          let e' = expr builder value in
          let elem_arg =
            match t with
            | A.Box _ | A.Vector _ -> e'
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
          L.build_load (StringMap.find name !variables) name builder
      (* Dummy add instruction *)
      | _ -> L.build_add (L.const_int i32_t 0) (L.const_int i32_t 0) "" builder
    in

    let add_terminal (builder : L.llbuilder) instr : unit =
      match L.block_terminator (L.insertion_block builder) with
      | Some _ -> ()
      | None -> ignore (instr builder)
    in

    let ret_instr = ref None in

    let free typ llvalue builder =
      match typ with
      | A.Vector _typ_inner ->
          let loaded_vec = L.build_load llvalue "vec_struct" builder in
          let v = L.build_struct_gep loaded_vec 0 "vector_length" builder in
          let loaded_v = L.build_load v "stored_length" builder in

          (* loop through vector length *)
          (*
             inside the loop body:
             1. get the element at index i
             2. cast it to the type we want
             3. recursively generate instrs to free that value
                (by calling ocaml free)
          *)
          (* free the vector by calling the helper *)
          let _ =
            L.build_call printf_func
              [| int_format_str; loaded_v |]
              "printf" builder
          in

          let _ = print_string "freeing vector" in
          ()
      | A.Box _typ_inner ->
          let _ = L.build_free llvalue builder in
          ()
      | _ -> ()
    in

    let rec stmt s builder =
      match s with
      | SBlock (sl, sblock_id) ->
          let block_deallocs =
            if StringMap.mem sblock_id ownership_map then
              StringMap.find sblock_id ownership_map
            else []
          in

          (* build inner stmts *)
          let builder' =
            List.fold_left (fun builder' s -> stmt s builder') builder sl
          in

          (* move builder before return value to insert free's *)
          (* if a return instr has been built (i.e., we returned) *)
          let builder' =
            match !ret_instr with
            | Some instr -> L.builder_before context instr
            | _ -> builder'
          in

          let _ =
            List.iter
              (fun stmt ->
                match stmt with
                | SAssign (_is_mut, t, name, _e) ->
                    if List.mem name block_deallocs then
                      free t (StringMap.find name !variables) builder'
                | _ -> ())
              sl
          in
          builder'
      | SExpr e ->
          let _ = expr builder e in
          builder
      | SPipeOut e ->
          let _ =
            ret_instr :=
              Some
                (match pdecl.sreturn_type with
                | A.Unit -> L.build_ret_void builder
                | _ -> L.build_ret (expr builder e) builder)
          in
          builder
      | SAssign (is_mut, t, name, e) ->
          let e' = expr builder e in
          let _ = add_local_variable (is_mut, t, name) e' in
          builder
      | SReAssign (name, e) ->
          let e' = expr builder e in
          let llv = StringMap.find name !variables in
          (* TODO: FREE OLD VALUE *)
          let _ = L.build_store e' llv builder in
          builder
      | SWhile (pred, body) ->
          let pred_bb = L.append_block context "while" the_pipe in
          let _ = L.build_br pred_bb builder in

          let body_bb = L.append_block context "while_body" the_pipe in
          let while_builder = stmt body (L.builder_at_end context body_bb) in

          let () = add_terminal while_builder (L.build_br pred_bb) in

          let pred_builder = L.builder_at_end context pred_bb in
          let bool_val = expr pred_builder pred in

          let merge_bb = L.append_block context "merge" the_pipe in
          let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
          L.builder_at_end context merge_bb
      | SLoop (e1, e2, i, e3, body) ->
          let assn = SAssign (true, A.Int, i, e1) in
          let pred = (A.Bool, SBinop ((A.Int, SIdent i), Leq, e2)) in
          let step =
            SReAssign (i, (A.Int, SBinop ((A.Int, SIdent i), Add, e3)))
          in

          (* TODO: figure out the sblock_id's s.t. the loop owns i *)
          stmt
            (SBlock
               ([ assn; SWhile (pred, SBlock ([ body; step ], "-1")) ], "-1"))
            builder
      | SIf (pred, s1, s2) ->
          let merge_bb = L.append_block context "merge" the_pipe in
          let branch_instr = L.build_br merge_bb in

          let then_bb = L.append_block context "then" the_pipe in
          let then_builder = stmt s1 (L.builder_at_end context then_bb) in
          let () = add_terminal then_builder branch_instr in

          let else_bb = L.append_block context "else" the_pipe in
          let else_builder = stmt s2 (L.builder_at_end context else_bb) in
          let () = add_terminal else_builder branch_instr in

          let _ = L.build_cond_br (expr builder pred) then_bb else_bb builder in
          L.builder_at_end context merge_bb
    in

    let _builder = stmt (SBlock (pdecl.sbody, "-1")) builder in

    ()
  in
  let _ = List.iter build_pipe_body pipes in
  the_module
