open Ast
module StringMap = Map.Make (String)

type graph_node =
  (* id, children id's, parent id *)
  | Lifetime of string * string list * string option
  | Binding of string * (bool * defined_type * string)
  | Rebinding of string * string
  | PipeCall of string * (string * expr list)

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
      (List.filter_map
         (fun s ->
           match s with
           | Assign (is_mut, typ, name, _) -> Some [ (is_mut, typ, name) ]
           | Block stmts -> Some (find_bindings stmts)
           | _ -> None)
         body)
  in

  let generate_lifetime_graph pipe =
    let rec gen_children pid stmt ct map =
      match stmt with
      | Block stmts ->
          let nid = pid ^ "." ^ string_of_int ct in
          let _, new_map, children =
            List.fold_left
              (fun (c, m, children) s ->
                ( c + 1,
                  gen_children nid s c m,
                  (nid ^ "." ^ string_of_int c) :: children ))
              (0, map, []) stmts
          in
          StringMap.add nid (Lifetime (nid, children, Some pid)) new_map
      | If (_, Block stmts, Block []) -> gen_children pid (Block stmts) ct map
      | If (_, Block stmts, Block stmts2) ->
          let m1 = gen_children pid (Block stmts) ct map in
          gen_children pid (Block stmts2) ct m1
      | Loop (_, _, _, _, Block stmts) -> gen_children pid (Block stmts) ct map
      | While (_, Block stmts) -> gen_children pid (Block stmts) ct map
      | Assign (is_mut, typ, name, _) ->
          let nid = pid ^ "." ^ string_of_int ct in
          StringMap.add nid (Binding (nid, (is_mut, typ, name))) map
      | ReAssign (name, _) ->
          let nid = pid ^ "." ^ string_of_int ct in
          StringMap.add nid (Rebinding (nid, name)) map
      | Expr (PipeIn (n, args)) ->
          let nid = pid ^ "." ^ string_of_int ct in
          StringMap.add nid (PipeCall (nid, (n, args))) map
      | _ -> map
    in
    let _, new_map, children =
      List.fold_left
        (fun (c, m, children) s ->
          ( c + 1,
            gen_children pipe.name s c m,
            (pipe.name ^ "." ^ string_of_int c) :: children ))
        (0, StringMap.empty, []) pipe.body
    in
    let children = match children with [] -> [] | _ :: c -> c in
    StringMap.add pipe.name (Lifetime (pipe.name, children, None)) new_map
  in

  let pipe_lifetime_maps pipes =
    List.fold_left
      (fun m p -> StringMap.add p.name (generate_lifetime_graph p) m)
      StringMap.empty pipes
  in

  let _check_pipe p =
    let formals' = check_bindings p.formals in
    let locals' = find_bindings p.body in

    (* make sure lhs and rhs of assignments and re-assignments are of eq type *)
    let _check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    let _symbols =
      List.fold_left
        (fun m (is_mut, typ, name) -> StringMap.add name (is_mut, typ) m)
        StringMap.empty (formals' @ locals')
    in
    ()
  in

  let pipe_lifetimes = pipe_lifetime_maps pipes in

  let _ =
    StringMap.iter
      (fun _p_name ltg ->
        let _ = print_string "\n" in
        let _ =
          StringMap.iter
            (fun _nid node ->
              let _ =
                match node with
                | Lifetime (id, children, pid) ->
                    let _ = print_string "node_type: Lifetime\n" in
                    let _ = print_string ("id: " ^ id ^ "\n") in
                    let _ =
                      print_string
                        ("pid: "
                        ^ (match pid with Some pid -> pid | None -> "None")
                        ^ "\n")
                    in
                    print_string
                      ("children: " ^ String.concat ", " children ^ "\n")
                | Binding (id, (is_mut, typ, name)) ->
                    let _ = print_string "node_type: Binding\n" in
                    let _ = print_string ("id: " ^ id ^ "\n") in
                    let _ =
                      print_string ("is_mut: " ^ string_of_bool is_mut ^ "\n")
                    in
                    let _ =
                      print_string ("type: " ^ string_of_typ typ ^ "\n")
                    in
                    print_string ("name: " ^ name ^ "\n")
                | Rebinding (id, name) ->
                    let _ = print_string "node_type: Rebinding\n" in
                    let _ = print_string ("id: " ^ id ^ "\n") in
                    print_string ("name: " ^ name ^ "\n")
                | PipeCall (id, (name, args)) ->
                    let _ = print_string "node_type: PipeCall\n" in
                    let _ = print_string ("id: " ^ id ^ "\n") in
                    let _ = print_string ("name: " ^ name ^ "\n") in
                    print_string
                      ("args: "
                      ^ String.concat ", "
                          (List.map (fun e -> string_of_expr e) args)
                      ^ "\n")
              in
              print_string "\n")
            ltg
        in
        print_string "\n------\n\n")
      pipe_lifetimes
  in

  (* variables can be safely immutibly borrowed iff *)
  (* 1. there are no active mutable borrows on said value *)
  (* 2. the value is in scope *)

  (* in the context of our lifetime tree, this translates to... *)
  (* no nodes to the left of current node (under closest re-assignment) *)
  (* mutably borrow the variable. also, there must not be any reassigments *)
  (* to the left of our current node. *)

  (* variables can be safely mutably borrowed iff *)
  (* 1. there are no active borrows (immutable/mutable) on said value *)

  (* in the context of our lifetime tree, this translates to... *)
  (* no nodes to the left of current node (under closest re-assignment) *)
  (* borrow the variable. also, there must not be any reassigments *)
  (* to the left of our current node. *)
  ([], [])
