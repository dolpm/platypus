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