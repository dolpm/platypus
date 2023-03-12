module L = Llvm
module A = Ast
open Sast 

(* Code Generation from the SAST. Returns an LLVM module if successful,
   throws an exception if something is wrong. *)
let translate (globals, functions) =
  let context    = L.global_context () in
  (* Add types to the context so we can use them in our LLVM code *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and unit_t     = L.void_type   context 
  and vector_t   = L.vector_type context 
  and thing_t    = L.struct_type context 
  (* Create an LLVM module -- this is a "container" into which we'll 
      generate actual code *)
  and the_module = L.create_module context "Platypus" in

  (* Convert Platypus types to LLVM types *)
  let ltype_of_typ = function
      A.Int    -> i32_t
    | A.Float  -> float_t
    | A.Bool   -> i1_t
    | A.Char   -> i8_t
    | A.String -> i8_t list (* no right??? *)
    | A.Vector -> vector_t
    | A.Unit   -> unit_t
    | A.Thing  -> thing_t
  in

  (* Declare printnl_t as a function *)
  let printnl_t = L.function_type i8_t [| i32_t |] in ( *Check over parameter types *)
  let printnl_pipe = L.declare_function "printnl" printnl_t the_module in

  (*Define all pipes (arguments and return type) to define body and call later *)

let pipe_decls: (L.llvalue * pipe_declaration) StringMap.t = 
  let pipe_decl m pdecl = 
    let name = pdecl.name
    and formal_types =
Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) pdecl.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.return_type) formal_types in
      StringMap.add name (L.define_function name ftype the_module, pdecl) m in
    List.fold_left pipe_decl StringMap.empty functions in   

    (* Fill the body of a function *)




