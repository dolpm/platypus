module L = Llvm
module A = Ast

open Sast

module StringMap = Map.Make (String)

(* Translates SAST into LLVM module or throws error *)
let translate (things, pipes) =
  let context = L.global_context () in

  let i32_t = L.i32_type context
  and i8_t = L.i8_type context
  and i1_t = L.i1_type context
  and float_t = L.double_type context
  and unit_t = L.void_type context
  and string_t = L.pointer_type (L.i8_type context)

  and the_module = L.create_module context "Platypus" in

  (* Convert Platypus types to LLVM types *)
  (* val ltype_of_typ : defined_type -> lltype *)
  let rec ltype_of_typ = function
      A.Int -> i32_t
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
    (* | A.Thing (_, eles) ->
        L.pointer_type
          (L.struct_type context
             (Array.of_list (List.map (fun (_, t) -> ltype_of_typ t) eles))) *)
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

  (* Generating code for things. A stringmap of llvalues, where each llvalue is an initialized const_struct global variablle*)
  let _thing_decls : L.llvalue StringMap.t = 
    let thing_decl m tdecl = 
      let name = tdecl.stname in
      let init = 
        let init_ele t = match t with
          A.Int | A.Float | A.Bool | A.Char | A.Unit 
            -> L.const_null (ltype_of_typ t)
        | A.Generic | A.Option _ -> 
            raise
              (Failure ("Cannot convert type" ^ A.string_of_typ t
                ^ "to LLVM IR"))
        | _ -> L.const_pointer_null (ltype_of_typ t)
        in
        L.const_struct context 
          (Array.of_list (List.map (fun (_, t, _) -> init_ele t)
                          tdecl.selements))
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
    StringMap.add ele_n init_ele  *)
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

    let _int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and _float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in

    let _formals =
      let add_formal m (_, t, n) p = 
        let () = L.set_value_name n p in
        let local = L.build_alloca (ltype_of_typ t) n builder in
        let _  = L.build_store p local builder in
      StringMap.add n local m 
      in
      List.fold_left2 add_formal StringMap.empty pdecl.sformals
        (Array.to_list (L.params the_pipe))
    in
    (* lookup should be defined inside body? *)
    let rec expr (builder : L.llbuilder) ((_, e) : s_expr) : L.llvalue = 
      match e with
        SIntLiteral i -> L.const_int i32_t i
      (* | SFloatLiteral ->
      | SBoolLiteral ->
      | SCharLiteral -> *)
      | SUnitLiteral-> L.const_null unit_t 
      | SStringLiteral s -> L.build_global_stringptr s "str" builder
      (* assignable thing value *)
      (* | SThingValue ->
      | STupleValue ->
      | STupleIndex ->
      | SIdent ->
      | SBinop ->
      | SUnop -> *)
      (* function call, takes in fn name and a list of inputs *)
      | SPipeIn ("printnl", [e]) ->
        L.build_call printf_func
          [| expr builder e |]
          "printf" builder
      | SPipeIn (pname, args) ->
        let pdef, pdecl = StringMap.find pname pipe_decls in
        let llargs = List.rev (List.map (expr builder) (List.rev args)) in
        let result = 
          match pdecl.sreturn_type with A.Unit -> "" | _ -> pname ^ "_result"
        in
        L.build_call pdef (Array.of_list llargs) result builder
        (* Dummy add instruction *)
      | _ -> L.build_add (L.const_int i32_t 0) (L.const_int i32_t 0) "" 
                    builder
    in

    let _add_terminal (builder : L.llbuilder) instr : unit = 
      match L.block_terminator (L.insertion_block builder) with
      | Some _ -> ()
      | None -> ignore (instr builder) 
    in
    
    let rec stmt builder = function
      SBlock (sl, _) -> List.fold_left stmt builder sl
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
      | _ -> builder
    in

    let _builder = stmt builder (SBlock (pdecl.sbody, [])) in

    ()
  in
  let _ = List.iter build_pipe_body pipes in
  the_module