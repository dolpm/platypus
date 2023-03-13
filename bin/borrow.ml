open Ast
module StringMap = Map.Make (String)

type graph_node =
  (* id, children id's, parent id *)
  | Lifetime of string * string list * string option
  (* id, binding, pid *)
  | Binding of string * (bool * defined_type * string * expr) * string option
  (* id, v2b name, pid *)
  | Rebinding of string * (string * expr) * string option
  (* id, p2c, pid *)
  | PipeCall of string * (string * expr list) * string option
  (* id, expr, pid *)
  | PipeReturn of string * expr * string option

let borrow_ck pipes verbose =
  let generate_lifetime_graph pipe =
    let rec gen_children pid stmt ct map =
      match stmt with
      | Block stmts ->
          let nid = pid ^ "." ^ string_of_int ct in
          let _, new_map, children =
            List.fold_left
              (fun (c, m, children) s ->
                let updated, m = gen_children nid s c m in
                if updated then
                  (c + 1, m, (nid ^ "." ^ string_of_int c) :: children)
                else (c, m, children))
              (0, map, []) stmts
          in
          ( true,
            StringMap.add nid
              (Lifetime (nid, List.rev children, Some pid))
              new_map )
      | Loop (_, _, _, _, Block stmts)
      | While (_, Block stmts)
      | If (_, Block stmts, Block []) ->
          gen_children pid (Block stmts) ct map
      | If (_, Block stmts, Block stmts2) ->
          let updated, m1 = gen_children pid (Block stmts) ct map in
          gen_children pid (Block stmts2) (if updated then ct + 1 else ct) m1
      | Assign (is_mut, typ, name, expr) ->
          let nid = pid ^ "." ^ string_of_int ct in
          ( true,
            StringMap.add nid
              (Binding (nid, (is_mut, typ, name, expr), Some pid))
              map )
      | ReAssign (name, expr) ->
          let nid = pid ^ "." ^ string_of_int ct in
          (true, StringMap.add nid (Rebinding (nid, (name, expr), Some pid)) map)
      | Expr (PipeIn (n, args)) ->
          let nid = pid ^ "." ^ string_of_int ct in
          (true, StringMap.add nid (PipeCall (nid, (n, args), Some pid)) map)
      | PipeOut expr ->
          let nid = pid ^ "." ^ string_of_int ct in
          (true, StringMap.add nid (PipeReturn (nid, expr, Some pid)) map)
      | _ -> (false, map)
    in
    (* create dummy assignment nodes for args in fn lifetime *)
    let map_with_args =
      snd
        (List.fold_left
           (fun (ct, m) (is_mut, typ, name) ->
             let c_name = pipe.name ^ "." ^ string_of_int ct in
             ( ct + 1,
               StringMap.add c_name
                 (Binding (c_name, (is_mut, typ, name, NoExpr), Some pipe.name))
                 m ))
           (0, StringMap.empty) pipe.formals)
    in
    let body =
      snd
        (gen_children pipe.name (Block pipe.body)
           (StringMap.cardinal map_with_args)
           map_with_args)
    in
    StringMap.add pipe.name
      (Lifetime
         ( pipe.name,
           List.rev
             ((pipe.name ^ "."
              ^ string_of_int (StringMap.cardinal map_with_args))
             :: List.map (fun (k, _) -> k) (StringMap.bindings map_with_args)),
           None ))
      body
  in

  let pipe_lifetime_maps pipes =
    List.fold_left
      (fun m p -> StringMap.add p.name (generate_lifetime_graph p) m)
      StringMap.empty pipes
  in

  let pipe_lifetimes = pipe_lifetime_maps pipes in

  (* pretty-print the lt graph *)
  let _print_lt_graph =
    if verbose then
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
                  | Binding (id, (is_mut, typ, name, expr), pid) ->
                      let _ = print_string "node_type: Binding\n" in
                      let _ = print_string ("id: " ^ id ^ "\n") in
                      let _ =
                        print_string
                          ("pid: "
                          ^ (match pid with Some pid -> pid | None -> "None")
                          ^ "\n")
                      in
                      let _ =
                        print_string ("is_mut: " ^ string_of_bool is_mut ^ "\n")
                      in
                      let _ =
                        print_string ("type: " ^ string_of_typ typ ^ "\n")
                      in
                      let _ = print_string ("name: " ^ name ^ "\n") in

                      print_string ("expr: " ^ string_of_expr expr ^ "\n")
                  | Rebinding (id, (name, expr), pid) ->
                      let _ = print_string "node_type: Rebinding\n" in
                      let _ = print_string ("id: " ^ id ^ "\n") in
                      let _ =
                        print_string
                          ("pid: "
                          ^ (match pid with Some pid -> pid | None -> "None")
                          ^ "\n")
                      in
                      let _ = print_string ("name: " ^ name ^ "\n") in
                      print_string ("expr: " ^ string_of_expr expr ^ "\n")
                  | PipeCall (id, (name, args), pid) ->
                      let _ = print_string "node_type: PipeCall\n" in
                      let _ = print_string ("id: " ^ id ^ "\n") in
                      let _ =
                        print_string
                          ("pid: "
                          ^ (match pid with Some pid -> pid | None -> "None")
                          ^ "\n")
                      in
                      let _ = print_string ("name: " ^ name ^ "\n") in
                      print_string
                        ("args: "
                        ^ String.concat ", "
                            (List.map (fun e -> string_of_expr e) args)
                        ^ "\n")
                  | PipeReturn (id, expr, pid) ->
                      let _ = print_string "node_type: PipeReturn\n" in
                      let _ = print_string ("id: " ^ id ^ "\n") in
                      let _ =
                        print_string
                          ("pid: "
                          ^ (match pid with Some pid -> pid | None -> "None")
                          ^ "\n")
                      in
                      print_string ("expr: " ^ string_of_expr expr ^ "\n")
                in
                print_string "\n")
              ltg
          in
          print_string "\n------\n\n")
        pipe_lifetimes
  in

  (* if a ref is returned, make sure that it is the smallest of all returnable *)
  (*
    idea - create a return stmt node in our graph. if the return type of the
    pipe is a ref, then we must check to see which argument each return was
    derived. once we have this set, we can take the rightmost (smallest) one
    and make sure it matches the return-type lifetime.
  *)
  let validate_arg_lifetimes p =
    (* if return isn't a borrow, who cares *)
    if match p.return_type with MutBorrow _ | Borrow _ -> false | _ -> true
    then ()
    else
      let return_lifetime_no_match correct_lifetime =
        "lifetime of return value must be the smallest (rightmost) lifetime of \
         all possible returned arguments: " ^ correct_lifetime
      and make_err er = raise (Failure er) in
      let lt_of_return =
        match p.return_type with
        | MutBorrow (_, lt) | Borrow (_, lt) -> lt
        | _ -> ""
      in
      (* we are kinda gonna have to limit returned values to args *)
      (* vs. allowing re-bindings of refs of args to also be returned *)
      let possible_ret_vars =
        Seq.fold_left
          (fun ret_vals (_id, node) ->
            match node with
            | PipeReturn (_id, Ident arg_name, _pid) -> arg_name :: ret_vals
            | _ -> ret_vals)
          []
          (StringMap.to_seq (StringMap.find p.name pipe_lifetimes))
      in
      let possible_lts =
        List.filter_map
          (fun (_, typ, n) ->
            if List.mem n possible_ret_vars then
              match typ with
              | MutBorrow (_, lt) | Borrow (_, lt) -> Some lt
              | _ -> None
            else None)
          p.formals
      in
      let smallest_possible_lt =
        List.fold_left
          (fun (smallest, lt_as_str) cur_lt ->
            let rec index_of_lt x lst =
              match lst with
              | [] -> raise (Failure "Not Found")
              | h :: t -> if x = h then 0 else 1 + index_of_lt x t
            in
            let i = index_of_lt cur_lt p.lifetimes in
            if i > smallest then (i, cur_lt) else (smallest, lt_as_str))
          (-1, "'static") possible_lts
      in
      if lt_of_return <> snd smallest_possible_lt then
        make_err (return_lifetime_no_match (snd smallest_possible_lt))
      else ()
  in

  let _ =
    List.iter
      (fun p ->
        let _ = validate_arg_lifetimes p in
        if verbose then
          print_string
            ("argument lifetime validation for " ^ p.name ^ " passed!\n"))
      pipes
  in

  let rec expr_borrows map expr =
    match expr with
    | Unop (Ref, Ident v) -> [ (v, false) ]
    | Unop (MutRef, Ident v) -> [ (v, true) ]
    | ThingValue l ->
        let exprs = List.map (fun (_, expr) -> expr) l in
        List.flatten (List.map (fun e -> expr_borrows map e) exprs)
    | TupleValue exprs | PipeIn (_, exprs) ->
        List.flatten (List.map (fun e -> expr_borrows map e) exprs)
    | Binop (e1, _, e2) -> expr_borrows map e1 @ expr_borrows map e2
    | Unop (_, e1) -> expr_borrows map e1
    | Ident v -> (
        match StringMap.find_opt v map with None -> [] | Some x -> [ (v, x) ])
    | _ -> []
  in

  (* returns borrows (literal references) inside of node *)
  (* semant should fail if we are using a borrow without deref '@' token? *)
  let node_borrows map node =
    let exprs_to_check =
      match node with
      | Binding (_, (_, _, _, e), _)
      | Rebinding (_, (_, e), _)
      | PipeReturn (_, e, _) ->
          [ e ]
      | PipeCall (_, (_, exprs), _) -> exprs
      | Lifetime (_, _, _) -> []
    in
    match exprs_to_check with
    | [] -> []
    | exprs_to_check ->
        List.flatten
          (List.map
             (fun expr_to_check -> expr_borrows map expr_to_check)
             exprs_to_check)
  in

  let borrow_ck pipe_name =
    let root =
      StringMap.find pipe_name (StringMap.find pipe_name pipe_lifetimes)
    in
    let already_mut_borrowed v_name =
      "variable " ^ v_name ^ " is already mutably borrowed."
    and mut_borrow_when_immut_borrowed v_name =
      "variable " ^ v_name
      ^ " is already immutably borrowed, thus it cant be mutably borrowed."
    and make_err er = raise (Failure er) in
    let rec drill cur_node borrow_map =
      match cur_node with
      | Lifetime (_id, cids, _pid) ->
          let _ =
            List.fold_left
              (fun m cid ->
                drill
                  (StringMap.find cid (StringMap.find pipe_name pipe_lifetimes))
                  m)
              borrow_map cids
          in
          borrow_map
      | n -> (
          let borrows_for_node = node_borrows borrow_map n in
          let new_map =
            List.fold_left
              (fun borrow_map (v_name, is_mut) ->
                (* check if borrow exists already *)
                let _validate =
                  if StringMap.mem v_name borrow_map then
                    (* check if borrow is mutable or immutable *)
                    if StringMap.find v_name borrow_map then
                      make_err (already_mut_borrowed v_name)
                    else if is_mut then
                      make_err (mut_borrow_when_immut_borrowed v_name)
                in
                StringMap.add v_name is_mut borrow_map)
              borrow_map borrows_for_node
          in
          (* if passed directly to pipe, the pipe's borrow *)
          (* will fall out of scope before the next line is executed *)
          (* thus we JUST need to validate the invariants *)
          match n with PipeCall (_, _, _) -> borrow_map | _ -> new_map)
    in
    let _ = drill root StringMap.empty in
    ()
  in

  let _ =
    List.iter
      (fun p ->
        let _ = borrow_ck p.name in
        if verbose then
          print_string ("borrow check for " ^ p.name ^ " passed!\n"))
      pipes
  in

  let ownership_ck pipe_name =
    let err_gave_ownership v_name =
      "variable " ^ v_name
      ^ " gave ownership to another binding and can't be accessed."
    and make_err er = raise (Failure er) in
    let rec inner symbols node =
      match node with
      | Lifetime (_id, cids, _pid) ->
          let added_symbols, new_map =
            List.fold_left
              (fun (added_symbols, m) cid ->
                let c_node =
                  StringMap.find cid (StringMap.find pipe_name pipe_lifetimes)
                in
                let syms, m = inner m c_node in
                (syms @ added_symbols, m))
              ([], symbols) cids
          in
          (* remove the vars that were added in this lifetime (dealloc time!) *)
          (* if it doesn't exist in the table, it got a new owner somewhere *)
          (* so don't worry about it *)
          ( [],
            List.fold_left
              (fun m sym -> StringMap.remove sym m)
              new_map added_symbols )
      | Binding (_id, (_is_mut, typ, name, expr), _pid) -> (
          if
            (* this is a borrow, so no ownership is taken *)
            match typ with Borrow _ | MutBorrow _ -> true | _ -> false
          then ([ name ], StringMap.add name typ symbols)
          else
            match expr with
            | Ident n ->
                if StringMap.mem n symbols then
                  ([ name ], StringMap.remove n symbols)
                else make_err (err_gave_ownership n)
            | _ -> ([ name ], StringMap.add name typ symbols))
      | Rebinding (_id, (name, expr), _pid) -> (
          (* todo - check the type of the og binding with name name *)
          match expr with
          | Ident n ->
              if StringMap.mem n symbols then
                ([ name ], StringMap.remove n symbols)
              else make_err (err_gave_ownership n)
          | _ -> ([ name ], symbols))
      | PipeCall (_id, (_name, exprs), _pid) ->
          List.fold_left
            (fun (new_symbols, m) expr ->
              match expr with
              | Ident n ->
                  if StringMap.mem n m then (new_symbols, StringMap.remove n m)
                  else make_err (err_gave_ownership n)
              | _ -> (new_symbols, m))
            ([], symbols) exprs
      | PipeReturn (_id, _expr, _pid) -> ([], symbols)
    in
    inner StringMap.empty
      (StringMap.find pipe_name (StringMap.find pipe_name pipe_lifetimes))
  in

  let _ =
    List.iter
      (fun p ->
        let _ = ownership_ck p.name in
        if verbose then
          print_string ("ownership check for " ^ p.name ^ " passed!\n"))
      pipes
  in

  pipe_lifetimes
