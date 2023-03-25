module L = Llvm
module A = Ast
(* open Sast  *)

(* Code Generation from the SAST. Returns an LLVM module if successful,
   throws an exception if something is wrong. *)
let translate (_globals, _functions) =
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
    | A.Generic -> raise (Failure "Cannot convert generic type to LLVM IR")
    | _ -> raise (Failure "Cannot convert generic type to LLVM IR")
  in

  (* Declare printnl_t as a function *)
  let printnl_t = L.function_type i8_t [| i32_t |] in
  (* Check over parameter types *)
  let _printnl_pipe = L.declare_function "printnl" printnl_t the_module in

  (*Define all pipes (arguments and return type) to define body and call later *)

  (* let pipe_decls: (L.llvalue * pipe_declaration) StringMap.t =
       let pipe_decl m pdecl =
         let name = pdecl.name
         and formal_types =
     Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) pdecl.formals)
           in let ftype = L.function_type (ltype_of_typ fdecl.return_type) formal_types in
           StringMap.add name (L.define_function name ftype the_module, pdecl) m in
         List.fold_left pipe_decl StringMap.empty functions in *)

  (* Fill the body of a function *)
  ltype_of_typ A.Int
