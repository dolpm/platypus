module L = Llvm
module A = Ast
open Sast
module StringMap = Map.Make (String)

(* Code Generation from the SAST. Returns an LLVM module if successful,
   throws an exception if something is wrong. *)
let translate (_globals, pipes) =
  let context = L.global_context () in
  (* Add types to the context so we can use them in our LLVM code *)
  let i32_t = L.i32_type context
  and i8_t = L.i8_type context
  and i1_t = L.i1_type context
  and float_t = L.double_type context
  and unit_t = L.void_type context
  and string_t = L.pointer_type (L.i8_type context)
  (* Create an LLVM module -- this is a "container" into which we'll
      generate actual code *)
  (* struct pointers can be used to implement recursive structs *)
  and the_module = L.create_module context "Platypus" in

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
    | A.Thing (_, members) ->
        L.pointer_type
          (L.struct_type context
             (Array.of_list (List.map (fun (_, t) -> ltype_of_typ t) members)))
    | A.Ident _ -> string_t
    | t ->
        raise
          (Failure ("Cannot convert type" ^ A.string_of_typ t ^ "to LLVM IR"))
  in

  (* Declare each global variable; remember its value in a map *)
  (* let _global_vars : L.llvalue StringMap.t =
     let global_var m (t, n) =
       let init = match t with
           A.Float -> L.const_float (ltype_of_typ t) 0.0
         | A.Int -> L.const_int (ltype_of_typ t) 0
         | A.Bool -> L.const_int (ltype_of_typ t) 0
         (* empty char *)
         (* | A.Char ->
         (* empty unit *)
         | A.Unit ->
         (* null ptr *)
         | A.Tuple ->
         | A.String ->
         | A.Box t     ->
         | A.Borrow (t, _)   ->
         | A.MutBorrow (t, _) ->
         | A.Thing (_, members) ->
         | A.Ident _   ->  *)
         | _ -> L.const_int (ltype_of_typ t) 0 (* null ptr *)
       in StringMap.add n (L.define_global n init the_module) m in
     List.fold_left global_var StringMap.empty globals in *)

  (* Declare printnl_t as a function *)
  let printnl_t = L.function_type i8_t [| i32_t |] in
  (* Check over parameter types *)
  let _printnl_pipe = L.declare_function "printnl" printnl_t the_module in

  (*Define all pipes (arguments and return type) to define body and call later *)
  let pipe_decls =
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

  (* Fill the body of a pipe *)
  (* Fill in the body of the given pipe *)
  let build_pipe_body pdecl =
    let the_pipe, _ = StringMap.find pdecl.sname pipe_decls in
    let builder = L.builder_at_end context (L.entry_block the_pipe) in

    let _int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and _float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in

    (* Construct the pipe's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let _local_vars =
      let add_formal m (_m, t, n) p =
        let () = L.set_value_name n p in
        let local = L.build_alloca (ltype_of_typ t) n builder in
        let _ = L.build_store p local builder in
        StringMap.add n local m
      in

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      let add_local m (_m, t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder in
        StringMap.add n local_var m
      in

      let formals =
        List.fold_left2 add_formal StringMap.empty pdecl.sformals
          (Array.to_list (L.params the_pipe))
      in
      List.fold_left add_local formals pdecl.slocals
    in

    ()
  in
  let _ = List.iter build_pipe_body pipes in
  the_module
