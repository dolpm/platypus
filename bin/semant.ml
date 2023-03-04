open Ast
module StringMap = Map.Make (String)

let check (_things, pipes) =
  (* built in pipe definitions *)
  (* (name, [param(is_mut, type, name)], ret_type) *)
  let stdlib_pipe_decls =
    [
      ("printnl", [ (false, String, "x") ], Unit);
      ("panic", [ (false, String, "x") ], Unit);
      ("int_to_string", [ (false, Int, "x") ], String);
      ("float_to_string", [ (false, Float, "x") ], String);
      ("char_to_string", [ (false, Char, "x") ], String);
      ("bool_to_string", [ (false, Bool, "x") ], String);
      (* TODO: does x have to be mutable? *)
      ("Heap_alloc", [ (true, Generic, "x") ], Box Generic);
      ("Vector_length", [ (false, Vector Generic, "x") ], Int);
      ("Vector_alloc", [], Vector Generic);
      ("Vector_get", [ (false, Vector Generic, "x") ], Option Generic);
      ("Vector_push", [ (true, Vector Generic, "x") ], Unit);
      ("Vector_pop", [ (true, Vector Generic, "x") ], Option Generic);
      ("option_is_none", [ (false, Option Generic, "x") ], Bool);
      ("option_is_some", [ (false, Option Generic, "x") ], Bool);
    ]
  in

  (* transform the built-in defn's into actual declaration types *)
  let built_in_pipe_decls =
    let add_bind map (name, args, ret_ty) =
      StringMap.add name
        {
          return_type = ret_ty;
          name;
          lifetimes = [];
          formals = args;
          body = [];
        }
        map
    in
    List.fold_left add_bind StringMap.empty stdlib_pipe_decls
  in

  (* add pipe to map if no collision, else raise *)
  let add_pipe map pdecl =
    let built_in_err =
      "pipe " ^ pdecl.name ^ " may not be defined - it's built-in"
    and dup_err = "duplicate pipe " ^ pdecl.name
    and make_err er = raise (Failure er)
    and n = pdecl.name in
    match pdecl with
    (* No duplicate functions or redefinitions of built-ins *)
    | _ when StringMap.mem n built_in_pipe_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ -> StringMap.add n pdecl map
  in

  (* add pipe declarations to a single symbol table *)
  (* checking for naming collisions *)
  let pipe_decls = List.fold_left add_pipe built_in_pipe_decls pipes in

  (* lookup pdecl *)
  let get_pipe s =
    try StringMap.find s pipe_decls
    with Not_found -> raise (Failure ("pipe \"" ^ s ^ "\" does not exist"))
  in

  (* make sure entrypoint is defined, also validate form *)
  let ep = get_pipe "main" in
  let ret_not_unit_err = "program entrypoint doesn't have a unit return type"
  and arg_lifetime_nonempty =
    "program entrypoint can't have arguments or lifetimes"
  and make_err er = raise (Failure er) in
  (* validate return type is unit, and parameters don't exist *)
  let _ =
    match ep.return_type with
    | Unit ->
        if List.length ep.formals > 0 || List.length ep.lifetimes > 0 then
          make_err arg_lifetime_nonempty
    | _ -> make_err ret_not_unit_err
  in

  (* TODO: if rhs is mutable reference, make sure lhs is mutable*)

  (* make sure there aren't any duplicate bindings in functions *)
  let check_bindings to_check =
    let name_compare (_, _, n1) (_, _, n2) = compare n1 n2 in
    let check_it checked binding =
      let _, _, n1 = binding in
      let dup_err = "duplicate binding: " ^ n1 in
      match checked with
      | (_, _, n2) :: _ when n1 = n2 -> raise (Failure dup_err)
      | _ -> binding :: checked
    in

    let _ = List.fold_left check_it [] (List.sort name_compare to_check) in
    to_check
  in

  (* recursively fetch all assignments in a statement list *)
  let rec find_bindings body =
    List.flatten
      (List.map
         (fun s ->
           match s with
           | Assign (is_mut, typ, name, _) -> [ (is_mut, typ, name) ]
           | Block stmts -> find_bindings stmts
           | _ -> [])
         body)
  in

  let check_pipe p =
    let _formals' = check_bindings p.formals in
    let locals' = find_bindings p.body in
    (* make sure lhs and rhs of assignments and re-assignments are of eq type *)
    let _check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in
    List.iter
      (fun (is_mut, typ, name) ->
        print_string
          (string_of_bool is_mut ^ " " ^ string_of_typ typ ^ " " ^ name ^ "\n"))
      locals'
  in

  let _ = List.iter check_pipe pipes in
  ([], [])
