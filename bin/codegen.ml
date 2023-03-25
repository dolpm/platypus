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

  let printnl_t = L.function_type i8_t [| i32_t |] in
  let _printnl_pipe = L.declare_function "printnl" printnl_t the_module in

  (* Generating code for things. A stringmap of llvalue stringmaps, where each element of the outer stringmap is a thing and each llvalue in the internal stringmap represents an initialized value of an element of the thingm *)
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

    (* Return a map of bindings of names and stored local variables *)
    (* needs work: formals instead of locals as there are no locals? *)
    (* let _local_vars =
      let add_formal m (_m, t, n) p =
        let () = L.set_value_name n p in
        let local = L.build_alloca (ltype_of_typ t) n builder in
        let _ = L.build_store p local builder in
        StringMap.add n local m
      in

      let add_local m (_m, t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder in
        StringMap.add n local_var m
      in

      let formals =
        List.fold_left2 add_formal StringMap.empty pdecl.sformals
          (Array.to_list (L.params the_pipe))
      in
      List.fold_left add_local formals pdecl.slocals
    in *)

    ()
  in
  let _ = List.iter build_pipe_body pipes in
  the_module