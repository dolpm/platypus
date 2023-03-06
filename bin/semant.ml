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
      (List.filter_map
         (fun s ->
           match s with
           | Assign (is_mut, typ, name, _) -> Some [ (is_mut, typ, name) ]
           | Block stmts -> Some (find_bindings stmts)
           | _ -> None)
         body)
  in

  (* build the lifetime graph for a pipe *)
  (* todo should we follow inner pipe calls? *)
  (* this would allow us to create a lt graph for the entire program *)
  let generate_lifetime_graph pipe =
    let rec lexical_lifetimes parent block ltid lifetime_map =
      match block with
      | Block body ->
          let ( child_ids,
                lifetime_map_with_children,
                shallow_bindings,
                shallow_rebindings,
                shallow_pipe_ins ) =
            List.fold_left
              (fun (c_ids, m, bs, rbs, pins) s ->
                match s with
                | Block [] -> (c_ids, m, bs, rbs, pins)
                | Block stmts ->
                    let c_ltid =
                      ltid ^ "." ^ string_of_int (List.length c_ids)
                    in
                    ( c_ltid :: c_ids,
                      lexical_lifetimes (Some ltid) (Block stmts) c_ltid m,
                      bs,
                      rbs,
                      pins )
                | If (_, Block stmts, Block []) ->
                    let c_ltid =
                      ltid ^ "." ^ string_of_int (List.length c_ids)
                    in
                    ( c_ltid :: c_ids,
                      lexical_lifetimes (Some ltid) (Block stmts) c_ltid m,
                      bs,
                      rbs,
                      pins )
                | If (_, Block stmts, Block stmts2) ->
                    let c1_ltid =
                      ltid ^ "." ^ string_of_int (List.length c_ids)
                    in
                    let s1_map =
                      lexical_lifetimes (Some ltid) (Block stmts) c1_ltid m
                    in
                    let c2_ltid =
                      ltid ^ "." ^ string_of_int (List.length c_ids + 1)
                    in
                    ( c2_ltid :: c1_ltid :: c_ids,
                      lexical_lifetimes (Some ltid) (Block stmts2) c1_ltid
                        s1_map,
                      bs,
                      rbs,
                      pins )
                | Loop (_, _, _, _, Block stmts) ->
                    let c_ltid =
                      ltid ^ "." ^ string_of_int (List.length c_ids)
                    in
                    ( c_ltid :: c_ids,
                      lexical_lifetimes (Some ltid) (Block stmts) c_ltid m,
                      bs,
                      rbs,
                      pins )
                | While (_, Block stmts) ->
                    let c_ltid =
                      ltid ^ "." ^ string_of_int (List.length c_ids)
                    in
                    ( c_ltid :: c_ids,
                      lexical_lifetimes (Some ltid) (Block stmts) c_ltid m,
                      bs,
                      rbs,
                      pins )
                | Assign (is_mut, typ, name, _) ->
                    let binding =
                      match c_ids with
                      | [] -> ((is_mut, typ, name), None)
                      | last_cid :: _ -> ((is_mut, typ, name), Some last_cid)
                    in
                    (c_ids, m, binding :: bs, rbs, pins)
                | ReAssign (name, _) ->
                    let rebinding =
                      match c_ids with
                      | [] -> (name, None)
                      | last_cid :: _ -> (name, Some last_cid)
                    in
                    (c_ids, m, bs, rebinding :: rbs, pins)
                | Expr (PipeIn (n, args)) ->
                    let pin =
                      match c_ids with
                      | [] -> ((n, args), None)
                      | last_cid :: _ -> ((n, args), Some last_cid)
                    in
                    (c_ids, m, bs, rbs, pin :: pins)
                | _ -> (c_ids, m, bs, rbs, pins))
              ([], lifetime_map, [], [], [])
              body
          in
          StringMap.add ltid
            ( ltid,
              parent,
              child_ids,
              shallow_bindings,
              shallow_rebindings,
              shallow_pipe_ins )
            lifetime_map_with_children
      | _ -> lifetime_map
    in
    lexical_lifetimes None (Block pipe.body) pipe.name StringMap.empty
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
            (fun lt (_ltid, parent, c_ltids, bs, rbs, pipe_ins) ->
              print_string
                (lt ^ " --> " ^ "\nparent: "
                ^ (match parent with Some p -> p | None -> "None")
                ^ ";\nchildren: [" ^ String.concat "," c_ltids
                ^ "];\nshallow binds: ["
                ^ String.concat ","
                    (List.map
                       (fun b ->
                         match b with
                         | (is_mut, typ, name), last_lt -> (
                             "(mutable: " ^ string_of_bool is_mut ^ ", type: "
                             ^ string_of_typ typ ^ ", name: " ^ name
                             ^ ", last_entered_lt: "
                             ^
                             match last_lt with
                             | None -> "None"
                             | Some x -> x ^ ")"))
                       bs)
                ^ "];\nre-binds: ["
                ^ String.concat ","
                    (List.map
                       (fun (n, last_lt) ->
                         "(" ^ "name: " ^ n ^ ", last_entered_lt: "
                         ^
                         match last_lt with None -> "None" | Some x -> x ^ ")")
                       rbs)
                ^ "];\nshallow pipe-ins: ["
                ^ String.concat ","
                    (List.map
                       (fun ((n, args), last_lt) ->
                         n ^ "("
                         ^ String.concat ","
                             (List.map (fun a -> string_of_expr a) args)
                         ^ ") - last_entered_lt: "
                         ^ match last_lt with None -> "None" | Some x -> x)
                       pipe_ins)
                ^ "]\n\n"))
            ltg
        in
        print_string "\n")
      pipe_lifetimes
  in

  let rec _lifetime_of_binding p_name (ltid, parent, c_ltids, bs, rbs, pipe_ins)
      var_name =
    match List.find_opt (fun b -> b = var_name) bs with
    | None -> (
        match parent with
        | None -> None
        | Some parent ->
            _lifetime_of_binding parent
              (StringMap.find parent (StringMap.find p_name pipe_lifetimes))
              var_name)
    | Some _ -> Some (ltid, parent, c_ltids, bs, rbs, pipe_ins)
  in

  let rec _is_borrowed p_name mut (_ltid, _parent, c_ltids, bs, _rbs, _pipe_ins)
      var_name =
    match
      List.find_opt
        (fun ((_is_mut, typ, v_name), _last_lt) ->
          if v_name <> var_name then false
          else
            match typ with Borrow _ -> !mut | MutBorrow _ -> true | _ -> false)
        bs
    with
    | Some _ -> true
    | None -> (
        match
          List.find_opt
            (fun c_ltid ->
              _is_borrowed p_name mut
                (StringMap.find c_ltid (StringMap.find p_name pipe_lifetimes))
                var_name)
            c_ltids
        with
        | Some _ -> true
        | None -> false)
  in

  let preceeding_blocks p_name root stop =
    let rec inner (ltid, _parent, c_ltids, _bs, _rbs, _pipe_ins)
        (ltid', _parent', _c_ltids', _bs', _rbs', _pipe_ins') soln =
      if ltid = ltid' then (true, soln)
      else
        let d, a =
          List.fold_left
            (fun (fin, accum) c_ltid ->
              if fin then (fin, accum)
              else
                let c =
                  StringMap.find c_ltid (StringMap.find p_name pipe_lifetimes)
                in
                let f, s = inner c stop [] in
                (f, s @ accum))
            (false, []) c_ltids
        in
        (d, ltid :: a)
    in
    snd (inner root stop [])
  in

  let _ =
    let blocks =
      preceeding_blocks "main"
        (StringMap.find "main" (StringMap.find "main" pipe_lifetimes))
        (StringMap.find "main.0.2.0" (StringMap.find "main" pipe_lifetimes))
    in
    List.iter (fun s -> print_string (s ^ "\n")) blocks
  in

  (* takes in a var name and borrow type, returns bool *)
  (*
  let _can_be_borrowed p_name (ltid, parent, c_ltids, bs, rbs, pipe_ins)
      var_name mut =
    if mut then
      match
        lifetime_of_binding p_name
          (ltid, parent, c_ltids, bs, rbs, pipe_ins)
          var_name
      with
      | None -> false
      (* must have next node in path to cur to know what is to the LEFT *)
      | Some (ltid', parent', c_ltids', bs', rbs', pipe_ins') ->
          (* run a pre-order on the tree with this as our root *)
          (* stop at ltid *)
          true
    else false
  in
  *)

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
