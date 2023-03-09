open Ast
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

type graph_node =
  (* id, children id's, parent id *)
  | Lifetime of string * string list * string option
  (* id, binding, pid *)
  | Binding of string * (bool * defined_type * string * expr) * string option
  (* id, v2b name, pid *)
  | Rebinding of string * (string * expr) * string option
  (* id, p2c, pid *)
  | PipeCall of string * (string * expr list) * string option

(*
  the basic intuition for ownership here is that if we have:
    {
      int x = 1;
      {
        {
            int y = x;
            // can't access x here, mem ownership passed to y
        } // y dropped
        // can't access x here
      }
      // can't access x here
    }

  in terms of our graph, all nodes to the right of the new owner
  can't mention x
*)

(*
  idea: do a dfs on ltg, keeping track of the values that have
  given ownership. if a value is still in scope, is referenced,
  but appears in this set, then we have a problem.   
*)

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
      | If (_, Block stmts, Block []) -> gen_children pid (Block stmts) ct map
      | If (_, Block stmts, Block stmts2) ->
          let _updated, m1 = gen_children pid (Block stmts) ct map in
          gen_children pid (Block stmts2) ct m1
      | Loop (_, _, _, _, Block stmts) -> gen_children pid (Block stmts) ct map
      | While (_, Block stmts) -> gen_children pid (Block stmts) ct map
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

  let _ =
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
                in
                print_string "\n")
              ltg
          in
          print_string "\n------\n\n")
        pipe_lifetimes
  in

  (* todo: if we are looking at an identifier, then look it up in the graph *)
  let rec expr_borrows map expr =
    match expr with
    | Unop (Ref, Ident v) -> [ (v, false) ]
    | Unop (MutRef, Ident v) -> [ (v, true) ]
    | ThingValue l ->
        let exprs = List.map (fun (_, expr) -> expr) l in
        List.flatten (List.map (fun e -> expr_borrows map e) exprs)
    | TupleValue exprs ->
        List.flatten (List.map (fun e -> expr_borrows map e) exprs)
    | Binop (e1, _, e2) -> expr_borrows map e1 @ expr_borrows map e2
    | Unop (_, e1) -> expr_borrows map e1
    | PipeIn (_, exprs) ->
        List.flatten (List.map (fun e -> expr_borrows map e) exprs)
    | Ident v -> (
        match StringMap.find_opt v map with None -> [] | Some x -> [ (v, x) ])
    | _ -> []
  in

  (* returns borrows inside of node *)
  let node_borrows map node =
    let exprs_to_check =
      match node with
      | Binding (_, (_, _, _, e), _) -> [ e ]
      | Rebinding (_, (_, e), _) -> [ e ]
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
          ( [],
            List.fold_left
              (fun m sym -> StringSet.remove sym m)
              new_map added_symbols )
      | Binding (_id, (_is_mut, _typ, name, expr), _pid) -> (
          match expr with
          | Ident n ->
              if StringSet.mem n symbols then
                ([ name ], StringSet.remove n symbols)
              else make_err (err_gave_ownership n)
          | _ -> ([ name ], StringSet.add name symbols))
      | Rebinding (_id, (name, expr), _pid) -> (
          match expr with
          | Ident n ->
              if StringSet.mem n symbols then
                ([ name ], StringSet.remove n symbols)
              else make_err (err_gave_ownership n)
          | _ -> ([ name ], StringSet.add name symbols))
      | PipeCall (_id, (name, exprs), _pid) ->
          List.fold_left
            (fun (new_symbols, m) expr ->
              match expr with
              | Ident n ->
                  if StringSet.mem n m then
                    (name :: new_symbols, StringSet.remove n m)
                  else make_err (err_gave_ownership n)
              | _ -> (name :: new_symbols, StringSet.add name m))
            ([], symbols) exprs
    in
    inner StringSet.empty
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
