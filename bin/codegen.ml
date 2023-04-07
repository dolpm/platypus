module L = Llvm
module A = Ast
open Sast
module StringMap = Map.Make (String)

(* Translates SAST into LLVM module or throws error *)
let translate (things, pipes) the_module =
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

    let rec expr (builder : L.llbuilder) ((_, e) : s_expr) : L.llvalue =
      match e with
      | SIntLiteral i -> L.const_int i32_t i
      | SFloatLiteral f -> L.const_float_of_string float_t f
      | SBoolLiteral b -> L.const_int i1_t (if b then 1 else 0)
      | SCharLiteral c -> L.const_int i8_t (int_of_char c)
      | SUnitLiteral -> L.const_null unit_t
      | SStringLiteral s -> L.build_global_stringptr s "str" builder
      | SBinop (e1, op, e2) ->
          let t, _ = e1 and e1' = expr builder e1 and e2' = expr builder e2 in
          (match t with
          | A.Int -> (
              match op with
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
          | A.Float -> (
              match op with
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
          | A.Bool -> (
              match op with
              | A.And -> L.build_and
              | A.Or -> L.build_or
              | A.Equal -> L.build_icmp L.Icmp.Eq
              | A.Neq -> L.build_icmp L.Icmp.Ne
              | _ -> raise (Failure "this operation is not supported"))
          | String -> (
              match op with
              | A.Equal -> L.build_icmp L.Icmp.Eq
              | A.Neq -> L.build_icmp L.Icmp.Ne
              | A.Concat -> raise (Failure "TOOD: implement concat op LLVM IR")
              | _ -> raise (Failure "this operation is not supported"))
          | Char -> (
              match op with
              | A.Equal -> L.build_icmp L.Icmp.Eq
              | A.Neq -> L.build_icmp L.Icmp.Ne
              | _ -> raise (Failure "this operation is not supported"))
          | _ -> raise (Failure "this operation is not supported"))
            e1' e2' "tmp" builder
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
      | SPipeIn ("printnl", [ (t, sx) ]) -> (
          match t with
          | Int | Bool ->
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

    let _add_terminal (builder : L.llbuilder) instr : unit =
      match L.block_terminator (L.insertion_block builder) with
      | Some _ -> ()
      | None -> ignore (instr builder)
    in

    let rec stmt builder = function
      | SBlock (sl, _) -> List.fold_left stmt builder sl
      | SExpr e ->
          let _ = expr builder e in
          builder
      | SPipeOut e ->
          let _ =
            match pdecl.sreturn_type with
            | A.Unit -> L.build_ret_void builder
            | _ -> L.build_ret (expr builder e) builder
          in
          builder
      | SAssign (is_mut, t, name, e) ->
          let e' = expr builder e in
          let _ = add_local_variable (is_mut, t, name) e' in
          builder
      | _ -> builder
    in

    let _builder = stmt builder (SBlock (pdecl.sbody, "-1")) in

    ()
  in
  let _ = List.iter build_pipe_body pipes in
  the_module
