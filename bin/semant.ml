open Ast
module StringMap = Map.Make (String);

(* box[int] x <| Heap_alloc_bool <| [true]; *)

let check (things, pipes) =
  let stdlib_fn_names =
    [
      ("printnl", Unit);
      ("panic", Unit);
      ("int_to_string", String);
      ("float_to_string", Float);
      ("char_to_string", Char);
      ("bool_to_string", Bool);
      ("Heap_alloc", Box);
      ("Vector_length", Int);
      ("Vector_alloc", Vector);
      ("Vector_get", Option);
      ("Vector_push", Unit);
      ("Vector_pop", Option);
      ("option_is_none", Bool);
      ("option_is_some", Bool);
    ]
  in

  let built_in_decls =
    let add_bind map (name, ty) =
      StringMap.add name
        {
          typ = Void;
          fname = name;
          formals = [ (ty, "x") ];
          locals = [];
          body = [];
        }
        map
    in
    List.fold_left add_bind StringMap.empty stdlib_fn_names
  in

  ()
