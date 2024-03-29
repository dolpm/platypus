(* Dylan M. | Ronit S. *)
open Sast
open Ast
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

type graph_node =
  | Lifetime of {
      parent : string option;
      children : string list;
      node_id : string;
      depth : int;
      loop : string option;
      (* rest *)
      owned_vars : string list;
      assoc_sblock_id : string option;
      is_ifelse_block : int;
          (* 0 if neither, 1 if is then clause of, 2 if is else clause*)
    }
  | Binding of {
      parent : string option;
      node_id : string;
      depth : int;
      loop : string option;
      (* rest *)
      is_mut : bool;
      (* TODO: REMOVE? *)
      typ : defined_type;
      name : string;
      expr : s_expr;
    }
  | Rebinding of {
      parent : string option;
      node_id : string;
      depth : int;
      loop : string option;
      (* rest *)
      name : string;
      expr : s_expr;
    }
  | PipeCall of {
      parent : string option;
      node_id : string;
      depth : int;
      loop : string option;
      (* rest *)
      pipe_name : string;
      args : s_expr list;
    }
  | PipeReturn of {
      parent : string option;
      node_id : string;
      depth : int;
      loop : string option;
      (* rest *)
      returned : s_expr;
    }
  | ExprCatchAll of {
      parent : string option;
      node_id : string;
      depth : int;
      loop : string option;
      (* rest *)
      value : s_expr;
    }

let borrow_ck pipes built_in_pipe_decls verbose =
  let _ = if verbose then print_string "generating graph!\n" else () in

  (* get data that is consistent across all node-types *)
  let node_common_data n =
    match n with
    | Lifetime { parent; node_id; depth; loop; _ }
    | Binding { parent; node_id; depth; loop; _ }
    | Rebinding { parent; node_id; depth; loop; _ }
    | PipeCall { parent; node_id; depth; loop; _ }
    | PipeReturn { parent; node_id; depth; loop; _ }
    | ExprCatchAll { parent; node_id; depth; loop; _ } ->
        (parent, node_id, depth, loop)
  in

  let generate_graph_for_pipe pipe =
    (* create top-level nodes for pipe arguments and add to graph *)
    let graph_with_pipe_args =
      snd
        (List.fold_left
           (fun (num_child, graph) (is_mut, typ, name) ->
             let depth =
               match typ with
               | MutBorrow (_, lt) | Borrow (_, lt) ->
                   if lt = "'_" then 1
                   else
                     let idx_of_lt, _ =
                       List.fold_left
                         (fun (cur_idx, found) lt' ->
                           if found then (cur_idx, found)
                           else if lt = lt' then (cur_idx, true)
                           else (cur_idx + 1, false))
                         (0, false) pipe.slifetimes
                     in
                     -1 - (List.length pipe.slifetimes - idx_of_lt)
               | _ -> 1
             in

             let is_mut_borrow =
               match typ with MutBorrow _ -> true | _ -> false
             in

             let child_id = pipe.sname ^ "." ^ string_of_int num_child in
             ( num_child + 1,
               StringMap.add child_id
                 (Binding
                    {
                      parent = Some pipe.sname;
                      node_id = child_id;
                      depth;
                      loop = None;
                      is_mut = is_mut || is_mut_borrow;
                      typ;
                      name;
                      expr = (typ, SNoexpr);
                    })
                 graph ))
           (0, StringMap.empty) pipe.sformals)
    in

    (* create graph node for an arbitrary statement, recurse if needed *)
    (*
         returns (bool * graph) where bool denotes whether any nodes
         were added.
      *)
    let rec gen_children parent_id parent_depth parent_loop child_id stmt graph
        =
      match stmt with
      | SBlock (stmts, sblock_id) ->
          let node_id = parent_id ^ "." ^ string_of_int child_id in
          let _, updated_graph, children =
            List.fold_left
              (fun (child_id, graph, children) stmt ->
                let was_updated, graph =
                  gen_children node_id (parent_depth + 1) parent_loop child_id
                    stmt graph
                in
                if was_updated then
                  ( child_id + 1,
                    graph,
                    (node_id ^ "." ^ string_of_int child_id) :: children )
                else (child_id, graph, children))
              (0, graph, []) stmts
          in
          ( true,
            StringMap.add node_id
              (Lifetime
                 {
                   parent = Some parent_id;
                   depth = parent_depth + 1;
                   loop = parent_loop;
                   node_id;
                   children = List.rev children;
                   owned_vars = [];
                   assoc_sblock_id =
                     (if sblock_id = "-1" then None else Some sblock_id);
                   is_ifelse_block = 0;
                 })
              updated_graph )
      | SAssign (is_mut, typ, name, expr) ->
          let node_id = parent_id ^ "." ^ string_of_int child_id in
          ( true,
            StringMap.add node_id
              (Binding
                 {
                   parent = Some parent_id;
                   node_id;
                   depth = parent_depth + 1;
                   loop = parent_loop;
                   is_mut;
                   typ;
                   name;
                   expr;
                 })
              graph )
      | SReAssign (_is_mutborrow, name, expr) ->
          let node_id = parent_id ^ "." ^ string_of_int child_id in
          ( true,
            StringMap.add node_id
              (Rebinding
                 {
                   parent = Some parent_id;
                   node_id;
                   depth = parent_depth + 1;
                   loop = parent_loop;
                   name;
                   expr;
                 })
              graph )
      | SExpr (_typ_of_exp, SPipeIn (pipe_name, args)) ->
          let node_id = parent_id ^ "." ^ string_of_int child_id in
          ( true,
            StringMap.add node_id
              (PipeCall
                 {
                   parent = Some parent_id;
                   node_id;
                   depth = parent_depth + 1;
                   loop = parent_loop;
                   pipe_name;
                   args;
                 })
              graph )
      | SPipeOut expr ->
          let node_id = parent_id ^ "." ^ string_of_int child_id in
          ( true,
            StringMap.add node_id
              (PipeReturn
                 {
                   parent = Some parent_id;
                   node_id;
                   depth = parent_depth + 1;
                   loop = parent_loop;
                   returned = expr;
                 })
              graph )
      | SExpr e ->
          let node_id = parent_id ^ "." ^ string_of_int child_id in
          ( true,
            StringMap.add node_id
              (ExprCatchAll
                 {
                   parent = Some parent_id;
                   node_id;
                   depth = parent_depth + 1;
                   loop = parent_loop;
                   value = e;
                 })
              graph )
      | sstmt ->
          let node_id = parent_id ^ "." ^ string_of_int child_id in
          let matched, graph', children =
            match sstmt with
            | SWhile (sex, SBlock (stmts, sblock_id), _) ->
                let wrapper_id = node_id ^ ".0" in
                let sex_id = wrapper_id ^ ".0" in
                let block_id = wrapper_id ^ ".1" in

                let block_wrapper =
                  Lifetime
                    {
                      parent = Some node_id;
                      node_id = wrapper_id;
                      depth = parent_depth + 1;
                      loop = Some node_id;
                      children = [ sex_id; block_id ];
                      owned_vars = [];
                      assoc_sblock_id = None;
                      is_ifelse_block = 0;
                    }
                in

                let sex_node =
                  ExprCatchAll
                    {
                      parent = Some wrapper_id;
                      node_id = sex_id;
                      depth = parent_depth + 2;
                      loop = Some node_id;
                      value = sex;
                    }
                in

                let graph' = StringMap.add wrapper_id block_wrapper graph in
                let graph' = StringMap.add sex_id sex_node graph' in

                let _, graph' =
                  gen_children wrapper_id (parent_depth + 2) (Some wrapper_id) 1
                    (SBlock (stmts, sblock_id))
                    graph'
                in

                (true, graph', [ wrapper_id ])
            | SIf
                ( sex,
                  SBlock (stmts1, sblock_id1),
                  SBlock (stmts2, sblock_id2),
                  _,
                  _ ) ->
                let sex_id = node_id ^ ".0" in
                let block1_id = node_id ^ ".1" in
                let block2_id = node_id ^ ".2" in

                let sex_node =
                  ExprCatchAll
                    {
                      parent = Some node_id;
                      node_id = sex_id;
                      depth = parent_depth + 1;
                      loop = parent_loop;
                      value = sex;
                    }
                in

                let graph' = StringMap.add sex_id sex_node graph in

                let _, graph' =
                  gen_children node_id (parent_depth + 1) parent_loop 1
                    (SBlock (stmts1, sblock_id1))
                    graph'
                in

                (* tag if block when there is an else after it *)
                let graph' =
                  if List.length stmts2 > 0 then
                    let then_block_id = node_id ^ ".1" in
                    let then_block_node = StringMap.find then_block_id graph' in
                    match then_block_node with
                    | Lifetime lt ->
                        let then_block_node' =
                          Lifetime
                            {
                              parent = lt.parent;
                              depth = lt.depth;
                              loop = lt.loop;
                              node_id = lt.node_id;
                              children = lt.children;
                              owned_vars = lt.owned_vars;
                              assoc_sblock_id = lt.assoc_sblock_id;
                              is_ifelse_block = 1;
                            }
                        in
                        StringMap.add then_block_id then_block_node' graph'
                    | _ ->
                        raise
                          (Failure
                             ("Then block " ^ then_block_id
                            ^ " is not a lifetime?"))
                  else graph'
                in

                let _, graph' =
                  gen_children node_id (parent_depth + 1) parent_loop 2
                    (SBlock (stmts2, sblock_id2))
                    graph'
                in

                (* tag else block *)
                let graph' =
                  let else_block_id = node_id ^ ".2" in
                  let else_block_node = StringMap.find else_block_id graph' in
                  match else_block_node with
                  | Lifetime lt ->
                      let else_block_node' =
                        Lifetime
                          {
                            parent = lt.parent;
                            depth = lt.depth;
                            loop = lt.loop;
                            node_id = lt.node_id;
                            children = lt.children;
                            owned_vars = lt.owned_vars;
                            assoc_sblock_id = lt.assoc_sblock_id;
                            is_ifelse_block = 2;
                          }
                      in
                      StringMap.add else_block_id else_block_node' graph'
                  | _ ->
                      raise
                        (Failure
                           ("Else block " ^ else_block_id
                          ^ " is not a lifetime?"))
                in

                (true, graph', [ sex_id; block1_id; block2_id ])
            | SLoop (sex1, sex2, ident, sex3, SBlock (stmts, sblock_id), _) ->
                let wrapper_id = node_id ^ ".0" in
                let sex1_id = wrapper_id ^ ".0" in
                let sex2_id = wrapper_id ^ ".1" in
                let ident_id = wrapper_id ^ ".2" in
                let sex3_id = wrapper_id ^ ".3" in
                let block_id = wrapper_id ^ ".4" in
                let wrapper_node =
                  Lifetime
                    {
                      parent = Some node_id;
                      node_id = wrapper_id;
                      depth = parent_depth + 1;
                      loop = Some node_id;
                      children =
                        [ sex1_id; sex2_id; ident_id; sex3_id; block_id ];
                      owned_vars = [];
                      assoc_sblock_id = None;
                      is_ifelse_block = 0;
                    }
                in
                let sex_nodes =
                  [
                    ExprCatchAll
                      {
                        parent = Some node_id;
                        node_id = sex1_id;
                        depth = parent_depth + 2;
                        loop = parent_loop;
                        value = sex1;
                      };
                    ExprCatchAll
                      {
                        parent = Some node_id;
                        node_id = sex2_id;
                        depth = parent_depth + 2;
                        loop = parent_loop;
                        value = sex2;
                      };
                    ExprCatchAll
                      {
                        parent = Some node_id;
                        node_id = sex3_id;
                        depth = parent_depth + 2;
                        loop = parent_loop;
                        value = sex3;
                      };
                  ]
                in

                let ident_node =
                  Binding
                    {
                      parent = Some node_id;
                      node_id = ident_id;
                      depth = parent_depth + 2;
                      loop = parent_loop;
                      is_mut = false;
                      typ = Int;
                      name = ident;
                      expr = sex1;
                    }
                in

                let graph' = StringMap.add wrapper_id wrapper_node graph in
                let graph' =
                  List.fold_left
                    (fun g sex_node ->
                      match sex_node with
                      | ExprCatchAll sn ->
                          StringMap.add sn.node_id (ExprCatchAll sn) g
                      | _ -> raise (Failure "panic!"))
                    graph' sex_nodes
                in
                let graph' = StringMap.add ident_id ident_node graph' in

                let _, graph' =
                  gen_children wrapper_id (parent_depth + 2) (Some wrapper_id) 4
                    (SBlock (stmts, sblock_id))
                    graph'
                in

                (true, graph', [ wrapper_id ])
            | _ -> (false, graph, [])
          in

          if not matched then (false, graph)
          else
            ( true,
              StringMap.add node_id
                (Lifetime
                   {
                     parent = Some parent_id;
                     node_id;
                     depth = parent_depth + 1;
                     loop = parent_loop;
                     children;
                     owned_vars = [];
                     assoc_sblock_id = None;
                     is_ifelse_block = 0;
                   })
                graph' )
    in

    (* create a block containing the body statements and gen thier nodes *)
    let graph_with_pipe_body =
      snd
        (gen_children pipe.sname 1 None
           (StringMap.cardinal graph_with_pipe_args)
           (SBlock (pipe.sbody, pipe.sname ^ "_wrapper"))
           graph_with_pipe_args)
    in

    (* TODO: owned vars here should just be the non-borrow args *)
    let pipe_graph =
      StringMap.add pipe.sname
        (Lifetime
           {
             parent = None;
             depth = 0;
             loop = None;
             children =
               List.rev
                 ((pipe.sname ^ "."
                  ^ string_of_int (StringMap.cardinal graph_with_pipe_args))
                 :: List.rev
                      (List.map
                         (fun (k, _) -> k)
                         (StringMap.bindings graph_with_pipe_args)));
             node_id = pipe.sname;
             owned_vars = [];
             assoc_sblock_id = Some (pipe.sname ^ "_with_args");
             is_ifelse_block = 0;
           })
        graph_with_pipe_body
    in

    pipe_graph
  in

  (* generate a graph for each pipe and add each to a global map *)
  let graph : graph_node StringMap.t StringMap.t =
    List.fold_left
      (fun graph pipe ->
        StringMap.add pipe.sname (generate_graph_for_pipe pipe) graph)
      StringMap.empty pipes
  in

  (* pretty-print the graph *)
  let print_graph g =
    if verbose then
      StringMap.iter
        (fun _p_name p_graph ->
          let _ = print_string "\n" in
          let _ =
            StringMap.iter
              (fun _nid node ->
                let _ =
                  match node with
                  | Lifetime l ->
                      let _ = print_string "node_type: Lifetime\n" in
                      let _ = print_string ("node_id: " ^ l.node_id ^ "\n") in
                      let _ =
                        print_string
                          ("parent: "
                          ^ (match l.parent with
                            | Some pid -> pid
                            | None -> "None")
                          ^ "\n")
                      in
                      let _ =
                        print_string
                          ("owned_vars: "
                          ^ String.concat ", " l.owned_vars
                          ^ "\n")
                      in
                      let _ =
                        print_string
                          ("assoc_sblock_id: "
                          ^ (match l.assoc_sblock_id with
                            | Some sbid -> sbid
                            | None -> "None")
                          ^ "\n")
                      in
                      print_string
                        ("children: " ^ String.concat ", " l.children ^ "\n")
                  | Binding b ->
                      let _ = print_string "node_type: Binding\n" in
                      let _ = print_string ("node_id: " ^ b.node_id ^ "\n") in
                      let _ =
                        print_string
                          ("parent: "
                          ^ (match b.parent with
                            | Some pid -> pid
                            | None -> "None")
                          ^ "\n")
                      in
                      let _ =
                        print_string
                          ("is_mut: " ^ string_of_bool b.is_mut ^ "\n")
                      in
                      let _ =
                        print_string ("type: " ^ string_of_typ b.typ ^ "\n")
                      in
                      let _ = print_string ("name: " ^ b.name ^ "\n") in

                      print_string ("expr: " ^ string_of_s_expr b.expr ^ "\n")
                  | Rebinding rb ->
                      let _ = print_string "node_type: Rebinding\n" in
                      let _ = print_string ("node_id: " ^ rb.node_id ^ "\n") in
                      let _ =
                        print_string
                          ("pid: "
                          ^ (match rb.parent with
                            | Some pid -> pid
                            | None -> "None")
                          ^ "\n")
                      in
                      let _ = print_string ("name: " ^ rb.name ^ "\n") in
                      print_string ("expr: " ^ string_of_s_expr rb.expr ^ "\n")
                  | PipeCall pc ->
                      let _ = print_string "node_type: PipeCall\n" in
                      let _ = print_string ("node_id: " ^ pc.node_id ^ "\n") in
                      let _ =
                        print_string
                          ("parent: "
                          ^ (match pc.parent with
                            | Some pid -> pid
                            | None -> "None")
                          ^ "\n")
                      in
                      let _ =
                        print_string ("pipe_name: " ^ pc.pipe_name ^ "\n")
                      in
                      print_string
                        ("args: "
                        ^ String.concat ", "
                            (List.map (fun e -> string_of_s_expr e) pc.args)
                        ^ "\n")
                  | ExprCatchAll eca ->
                      let _ = print_string "node_type: ExprCatchAll\n" in
                      let _ = print_string ("node_id: " ^ eca.node_id ^ "\n") in
                      let _ =
                        print_string
                          ("parent: "
                          ^ (match eca.parent with
                            | Some pid -> pid
                            | None -> "None")
                          ^ "\n")
                      in
                      print_string
                        ("value: " ^ string_of_s_expr eca.value ^ "\n")
                  | PipeReturn pr ->
                      let _ = print_string "node_type: PipeReturn\n" in
                      let _ = print_string ("node_id: " ^ pr.node_id ^ "\n") in
                      let _ =
                        print_string
                          ("parent: "
                          ^ (match pr.parent with
                            | Some pid -> pid
                            | None -> "None")
                          ^ "\n")
                      in
                      print_string
                        ("returned_expr: " ^ string_of_s_expr pr.returned ^ "\n")
                in
                print_string "\n")
              p_graph
          in
          print_string "\n------\n\n")
        g
  in

  let _ = if verbose then print_string "generated graph!\n" else () in

  (* find indentifiers in an expression *)
  let find_identifiers (sex : s_expr) : string list =
    let rec inner ((_t, e) : s_expr) (names : string list) : string list =
      match e with
      | SIdent name -> name :: names
      | SBinop (s1, _, s2) -> inner s2 names @ inner s1 names
      | SUnop (_, s) -> inner s names
      | SPipeIn (_, sl) | STupleValue sl ->
          List.fold_left (fun l s -> inner s l) names sl
      | SThingValue (_, tl) ->
          List.fold_left (fun l (_n, s) -> inner s l) names tl
      | _ -> names
    in
    inner sex []
  in

  let find_moves (sex : s_expr) (active_refs : StringSet.t) : string list =
    let rec inner ((_t, e) : s_expr) (names : string list) : string list =
      match e with
      | SIdent name ->
          if StringSet.mem name active_refs then [] else name :: names
      | SBinop _ | SUnop _ -> []
      (*
      Binops/unops create new values -- don't take ownership in any case.

      | SBinop (s1, _, s2) -> inner s2 names @ inner s1 names
      | SUnop (Ref, _) | SUnop (MutRef, _) | SUnop (Clone, _) -> []
      | SUnop (_, s) -> inner s names
      *)
      | SPipeIn (_, sl) | STupleValue sl ->
          List.fold_left (fun l s -> inner s l) names sl
      | SThingValue (_, tl) ->
          List.fold_left (fun l (_n, s) -> inner s l) names tl
      | _ -> names
    in
    inner sex []
  in

  let get_depth_of_defn graph_for_pipe (node : string) (ident_name : string) :
      graph_node option * int =
    let rec inner (current_node : string option) (ident_name : string) :
        graph_node option * int =
      match current_node with
      | None -> (None, 0)
      | Some current_node -> (
          let current_node = StringMap.find current_node graph_for_pipe in
          match current_node with
          | Lifetime l -> (
              let found =
                List.find_opt
                  (fun c ->
                    (* todo: do rebindings matter here? *)
                    let c = StringMap.find c graph_for_pipe in
                    match c with
                    | Binding b -> if b.name = ident_name then true else false
                    | _ -> false)
                  l.children
              in
              match found with
              | Some fc ->
                  let fc' = StringMap.find fc graph_for_pipe in
                  let _, _, depth, _ = node_common_data fc' in
                  (Some fc', depth)
              | _ -> inner l.parent ident_name)
          | n ->
              let parent, _, _, _ = node_common_data n in
              inner parent ident_name)
    in
    inner (Some node) ident_name
  in

  let deepest_origin pipe e cur_node_id =
    let graph_for_pipe = StringMap.find pipe.sname graph in
    (* return the identifier and it's smallest lt (in depth) *)
    let rec deepest_origin_inner e cur_node_id =
      match e with
      (* check for args that are borrowed here *)
      | _ty, SUnop (MutRef, (_, SIdent n))
      | _ty, SUnop (Ref, (_, SIdent n))
      | _ty, SIdent n -> (
          (* return max depth of all possible origins *)
          let defn_node, depth_of_defn =
            get_depth_of_defn graph_for_pipe cur_node_id n
          in
          match defn_node with
          | Some (Binding b) -> (
              match b.typ with
              | MutBorrow _ | Borrow _ -> (
                  let inner_defn_node, depth_of_inner_defn =
                    deepest_origin_inner b.expr b.node_id
                  in
                  match inner_defn_node with
                  (* deepest origin could be a pipe arg that is a borrow *)
                  (* which would cause this to be none *)
                  | None -> (Some n, depth_of_defn)
                  | _ -> (inner_defn_node, depth_of_inner_defn))
              | _ -> (Some n, depth_of_defn))
          | _ -> raise (Failure "panic! not possible!"))
      | _ty, SPipeIn (_p_name, args) ->
          List.fold_left
            (fun (max_node, max_depth) cur_arg ->
              let n, d = deepest_origin_inner cur_arg cur_node_id in
              match n with
              | None -> (max_node, max_depth)
              | Some n ->
                  if d > max_depth then (Some n, d) else (max_node, max_depth))
            (None, -1 - List.length pipe.slifetimes)
            args
      | _ -> (None, -1)
    in
    deepest_origin_inner e cur_node_id
  in

  let get_possible_ret_args p ret_v_map =
    let graph_for_pipe = StringMap.find p.sname graph in

    let ident_assoc_lt_map = ref StringMap.empty in

    let rec explore_assocs (n : string) (set : StringSet.t) : StringSet.t =
      if n = "_non_ident" || not (StringMap.mem n !ident_assoc_lt_map) then set
      else
        let _, _, assoc_idents = StringMap.find n !ident_assoc_lt_map in
        let set = StringSet.add n set in
        if List.length assoc_idents > 0 then
          List.fold_left
            (fun set ident -> explore_assocs ident set)
            set assoc_idents
        else set
    in

    let possible_ret_vars =
      Seq.fold_left
        (fun ret_vals (_id, node) ->
          match node with
          | PipeReturn pr -> (
              match pr.returned with
              | _, SIdent n ->
                  let _ =
                    match p.sreturn_type with
                    | MutBorrow (_, lt) | Borrow (_, lt) ->
                        (* get the depth of the argument whose lifetime is the return type lifetime *)
                        let depth_of_ret_lt =
                          let idx_of_lt, _ =
                            List.fold_left
                              (fun (cur_idx, found) lt' ->
                                if found then (cur_idx, found)
                                else if lt = lt' then (cur_idx, true)
                                else (cur_idx + 1, false))
                              (0, false) p.slifetimes
                          in
                          -1 - (List.length p.slifetimes - idx_of_lt)
                        in
                        let d, lt', _ = StringMap.find n !ident_assoc_lt_map in
                        let _ =
                          if lt = "'_" then
                            raise
                              (Failure
                                 ("return type "
                                 ^ string_of_typ p.sreturn_type
                                 ^ " of pipe " ^ p.sname
                                 ^ " has unspecified lifetime; when a pipe \
                                    returns a reference, it must have explicit \
                                    lifetimes."))
                        in
                        if d > depth_of_ret_lt then
                          let lt_in_def =
                            List.find_opt (fun plt -> lt' = plt) p.slifetimes
                          in
                          if
                            (match lt_in_def with
                            | None -> true
                            | Some _ -> false)
                            && lt' <> "'_"
                          then
                            raise
                              (Failure
                                 ("lifetime " ^ lt' ^ " used but not defined in "
                                ^ p.sname))
                          else
                            raise
                              (Failure
                                 ("return value " ^ n ^ " has lifetime " ^ lt'
                                ^ ", which is smaller than return lifetime "
                                ^ lt))
                    | _ -> ()
                  in
                  explore_assocs n (StringSet.add n ret_vals)
              | _, SPipeIn (p_name, args) ->
                  let idxs = StringMap.find p_name ret_v_map in

                  let _, pos_returned_args =
                    List.fold_left
                      (fun (i, ret_vals) a ->
                        ( i + 1,
                          match a with
                          | _, SIdent n ->
                              if List.mem i idxs then
                                (* check if lt is local to the function *)
                                (* (i.e., will outlive pipe )... *)
                                (* throw if not the case *)
                                let _, lt, _ =
                                  StringMap.find n !ident_assoc_lt_map
                                in

                                if lt = "'_" then
                                  raise
                                    (Failure
                                       ("argument " ^ string_of_s_expr a
                                      ^ " that might be returned from " ^ p_name
                                      ^ " doesn't outlive pipe " ^ p.sname
                                      ^ " in pipe-out call"))
                                else explore_assocs n (StringSet.add n ret_vals)
                              else ret_vals
                          | _ ->
                              if List.mem i idxs then
                                raise
                                  (Failure
                                     ("argument " ^ string_of_s_expr a
                                    ^ " that might be returned from " ^ p_name
                                    ^ " doesn't outlive pipe " ^ p.sname
                                    ^ " in pipe-out call"))
                              else ret_vals ))
                      (0, ret_vals) args
                  in

                  pos_returned_args
              | _ -> ret_vals)
          (* Also, populate the lt map as we go *)
          | Binding b ->
              (* update map only if this binding is a borrow *)
              let _ =
                match b.typ with
                | MutBorrow (_, lhs_lt) | Borrow (_, lhs_lt) -> (
                    (* furthermore, need to set depth correctly based on rhs *)
                    (* & x <| pipe_call (a1, a2, a3) *)
                    match snd b.expr with
                    | SPipeIn (pname, args) ->
                        let decl_of_called_pipe =
                          List.find (fun p -> p.sname = pname) pipes
                        in

                        let _, depths_of_args_w_lts, lts_of_args, ident_names =
                          List.fold_left
                            (fun (idx, depth_l, lt_l, n_l) (t, e) ->
                              match t with
                              | MutBorrow _ | Borrow _ ->
                                  let _, arg_ty_in_decl, _ =
                                    List.nth decl_of_called_pipe.sformals idx
                                  in

                                  let has_explicit_lt_in_decl =
                                    match arg_ty_in_decl with
                                    | Borrow (_, lt) | MutBorrow (_, lt) ->
                                        lt <> "'_"
                                    | _ ->
                                        raise
                                          (Failure
                                             ("in " ^ p.sname
                                            ^ ", when calling "
                                            ^ decl_of_called_pipe.sname
                                            ^ ": arg has type "
                                             ^ Ast.string_of_typ arg_ty_in_decl
                                             ))
                                  in

                                  (* only care about borrows with explcit lifetimes
                                      in the declaration of called pipe *)
                                  if has_explicit_lt_in_decl then
                                    (* if the argument supplied is an ident,
                                        we get its entry in the map *)
                                    match e with
                                    | SIdent arg_name ->
                                        let arg_depth, arg_lt, _ =
                                          StringMap.find arg_name
                                            !ident_assoc_lt_map
                                        in
                                        ( idx + 1,
                                          arg_depth :: depth_l,
                                          arg_lt :: lt_l,
                                          arg_name :: n_l )
                                    | _ ->
                                        ( idx + 1,
                                          b.depth :: depth_l,
                                          "'_" :: lt_l,
                                          "_non_ident" :: n_l )
                                  else (idx + 1, depth_l, lt_l, n_l)
                              | _ -> (idx + 1, depth_l, lt_l, n_l))
                            (0, [], [], []) args
                        in

                        let max_depth, lt_name_of_max_depth =
                          if List.length lts_of_args > 0 then
                            List.fold_left
                              (fun (curr_max, max_name) (depth, name) ->
                                if curr_max < depth then (depth, name)
                                else (curr_max, max_name))
                              ( List.nth depths_of_args_w_lts 0,
                                List.nth lts_of_args 0 )
                              (List.combine depths_of_args_w_lts lts_of_args)
                          else (b.depth, lhs_lt)
                        in
                        ident_assoc_lt_map :=
                          StringMap.add b.name
                            (max_depth, lt_name_of_max_depth, ident_names)
                            !ident_assoc_lt_map
                    | _ ->
                        ident_assoc_lt_map :=
                          StringMap.add b.name (b.depth, lhs_lt, [])
                            !ident_assoc_lt_map)
                | _ -> ()
              in
              ret_vals
          | _ -> ret_vals)
        StringSet.empty
        (StringMap.to_seq graph_for_pipe)
    in

    (* convert ss to list *)
    (StringSet.elements possible_ret_vars, !ident_assoc_lt_map)
  in

  (* In the case that we need to get the origins of a function that returns a borrow or mutborrow, *)
  (* the pipe-call may have multiple origins, so we want to build up a set instead of just returning the max *)
  let origins_of_pipecall (pipe : s_pipe_declaration) (sex : s_expr)
      (ret_v_map : int list StringMap.t) (cur_node_id : string) : string list =
    let rec deep_pc olist (t, e) =
      match (t, e) with
      | MutBorrow _, SPipeIn (p_name, args) | Borrow _, SPipeIn (p_name, args)
        ->
          let called_p = List.find (fun p -> p.sname = p_name) pipes in
          let ret_vars, _ = get_possible_ret_args called_p ret_v_map in
          let idx_of_ret_args =
            List.rev
              (snd
                 (List.fold_left
                    (fun (idx, filtered_idxs) (_, _, formal_name) ->
                      if List.mem formal_name ret_vars then
                        (idx + 1, idx :: filtered_idxs)
                      else (idx + 1, filtered_idxs))
                    (0, []) called_p.sformals))
          in

          let args_to_consider =
            List.rev
              (List.fold_left
                 (fun l i -> List.nth args i :: l)
                 [] idx_of_ret_args)
          in

          List.fold_left (fun ol arg -> deep_pc ol arg) olist args_to_consider
      | _ -> (
          let n, _ = deepest_origin pipe (t, e) cur_node_id in
          match n with None -> olist | Some x -> x :: olist)
    in
    List.rev (deep_pc [] sex)
  in

  let ownership_ck pipe =
    let graph_for_pipe = StringMap.find pipe.sname graph in

    let err_gave_ownership v_name =
      "variable " ^ v_name
      ^ " gave ownership to another binding and can't be accessed."
    and make_err er = raise (Failure er) in

    let rec check_children current_node symbol_table active_refs graph =
      let current_node = StringMap.find current_node graph in

      let held_state = ref None in

      match current_node with
      | Lifetime l ->
          (* process ownership of child nodes -- this will remove all variables who are owned in lower lifetimes *)
          let symbol_table', _active_refs', graph' =
            List.fold_left
              (fun (st, active_refs, graph) child ->
                let child_node = StringMap.find child graph in

                let if_else_block =
                  match child_node with
                  | Lifetime lt -> lt.is_ifelse_block
                  | _ -> 0
                in

                if if_else_block = 1 then
                  let _ = held_state := Some (st, active_refs, graph) in
                  check_children child st active_refs graph
                else
                  match !held_state with
                  | Some (st_held, active_refs_held, graph_held) ->
                      let st', _active_refs', _graph' =
                        check_children child st_held active_refs_held graph_held
                      in
                      let _ = held_state := None in
                      let owned_by_if =
                        StringMap.filter
                          (fun k _v -> not (StringMap.mem k st))
                          st'
                      in
                      let owned_by_else =
                        StringMap.filter
                          (fun k _v -> not (StringMap.mem k st'))
                          st
                      in
                      let owned_by_ifelse =
                        StringMap.union
                          (fun _k v1 _v2 -> Some v1)
                          owned_by_if owned_by_else
                      in

                      let remaining_after_ifelse =
                        StringMap.filter
                          (fun k _v -> not (StringMap.mem k owned_by_ifelse))
                          st_held
                      in
                      (remaining_after_ifelse, active_refs_held, graph_held)
                  | None -> check_children child st active_refs graph)
              (symbol_table, active_refs, graph)
              l.children
          in

          (* active refs will be reset past this point *)
          (* as all of those that were added down in the tree *)
          (* will have expired *)

          (* anything left in the symbol table are variables that this lifetime must deallocate *)
          let symbol_table', children_responsible_for_dealloc =
            List.fold_left
              (fun (st', dealloc_list) child ->
                match StringMap.find child graph_for_pipe with
                | Binding b ->
                    if StringMap.mem b.name st' then
                      (StringMap.remove b.name st', b.name :: dealloc_list)
                    else (st', dealloc_list)
                | _ -> (st', dealloc_list))
              (symbol_table', []) l.children
          in

          ( symbol_table',
            active_refs,
            StringMap.add l.node_id
              (Lifetime
                 {
                   children = l.children;
                   node_id = l.node_id;
                   depth = l.depth;
                   loop = l.loop;
                   parent = l.parent;
                   owned_vars = children_responsible_for_dealloc;
                   assoc_sblock_id = l.assoc_sblock_id;
                   is_ifelse_block = l.is_ifelse_block;
                 })
              graph' )
      | Binding b ->
          let names = find_identifiers b.expr in

          (* make sure all idents in expr are in symbol table *)
          let _ =
            List.iter
              (fun n ->
                if
                  (not (StringMap.mem n symbol_table))
                  && not (StringSet.mem n active_refs)
                then make_err (err_gave_ownership n))
              names
          in

          let active_refs =
            match b.expr with
            | MutBorrow _, _ | Borrow _, _ -> StringSet.add b.name active_refs
            | _ -> active_refs
          in

          (* if ownership of another var given to new binding *)
          (* remove the original from the table *)
          (* and validate that rhs is in same loop *)
          let symbol_table', active_refs' =
            match b.expr with
            | _ty, SIdent v_name ->
                if StringSet.mem v_name active_refs then
                  (symbol_table, active_refs)
                else
                  let v_node =
                    StringMap.find (StringMap.find v_name symbol_table) graph
                  in
                  let _v_parent, _v_node_id, _v_depth, v_loop =
                    node_common_data v_node
                  in
                  if v_loop <> b.loop then
                    raise
                      (Failure
                         ("ownership of " ^ v_name
                        ^ " could be taken in a previous loop iteration"))
                  else
                    let st = StringMap.remove v_name symbol_table in
                    (StringMap.add b.name b.node_id st, active_refs)
            | _ty, SThingValue _ | _ty, STupleValue _ | _ty, SPipeIn _ ->
                (* check ownership of identifiers in rhs *)
                (* throw err if ownership already given elsewhere *)
                let names = find_identifiers b.expr in

                let _ =
                  List.iter
                    (fun n ->
                      if
                        (not (StringMap.mem n symbol_table))
                        && not (StringSet.mem n active_refs)
                      then make_err (err_gave_ownership n))
                    names
                in

                (* find vars that get moved -- ownership transferred -- inside of rhs *)
                let moves = find_moves b.expr active_refs in

                (* if ownership of another var given to rhs *)
                (* remove the original from the table *)
                (* and validate that rhs is in same loop *)
                let symbol_table' =
                  List.fold_left
                    (fun symbol_table v_name ->
                      if StringSet.mem v_name active_refs then symbol_table
                      else
                        let v_node =
                          StringMap.find
                            (StringMap.find v_name symbol_table)
                            graph
                        in
                        let _v_parent, _v_node_id, _v_depth, v_loop =
                          node_common_data v_node
                        in

                        if v_loop <> b.loop then
                          raise
                            (Failure
                               ("ownership of " ^ v_name
                              ^ " could be taken in a previous loop iteration"))
                        else StringMap.remove v_name symbol_table)
                    symbol_table moves
                in
                (StringMap.add b.name b.node_id symbol_table', active_refs)
            | _, SThingAccess _
            | _, STupleIndex _
            | _, SUnop (Ref, (_, SIdent _))
            | _, SUnop (MutRef, (_, SIdent _))
            (* arg placeholder nodes *)
            | Borrow _, SNoexpr
            | MutBorrow _, SNoexpr ->
                (symbol_table, StringSet.add b.name active_refs)
            | _expr -> (StringMap.add b.name b.node_id symbol_table, active_refs)
          in

          (symbol_table', active_refs', graph)
      (* in ExprCatchAll, we only need to check that all found identifiers are in the symbol_table table *)
      (* we don't have to remove anything from the symbol table *)
      | PipeCall pc ->
          (* check ownership of identifiers in rhs *)
          (* throw err if ownership already given elsewhere *)
          let names =
            List.fold_left
              (fun idents e -> idents @ find_identifiers e)
              [] pc.args
          in

          let _ =
            List.iter
              (fun n ->
                if
                  (not (StringMap.mem n symbol_table))
                  && not (StringSet.mem n active_refs)
                then make_err (err_gave_ownership n))
              names
          in

          (* find vars that get moved -- ownership transferred -- inside of rhs *)
          let moves =
            List.fold_left
              (fun moves e -> moves @ find_moves e active_refs)
              [] pc.args
          in

          (* if ownership of another var given to rhs *)
          (* remove the original from the table *)
          (* and validate that rhs is in same loop *)
          let symbol_table' =
            List.fold_left
              (fun symbol_table v_name ->
                if StringSet.mem v_name active_refs then symbol_table
                else
                  let v_node =
                    StringMap.find (StringMap.find v_name symbol_table) graph
                  in
                  let _v_parent, _v_node_id, _v_depth, v_loop =
                    node_common_data v_node
                  in

                  if v_loop <> pc.loop then
                    raise
                      (Failure
                         ("ownership of " ^ v_name
                        ^ " could be taken in a previous loop iteration"))
                  else StringMap.remove v_name symbol_table)
              symbol_table moves
          in
          (symbol_table', active_refs, graph)
      | v ->
          let e =
            match v with
            | PipeReturn pr -> pr.returned
            | Rebinding rb -> rb.expr
            | ExprCatchAll eca -> eca.value
            | _ -> make_err "panic! not possible!"
          in
          let names = find_identifiers e in
          (* make sure all idents in expr are in symbol table *)
          let _ =
            List.iter
              (fun n ->
                if
                  (not (StringMap.mem n symbol_table))
                  && not (StringSet.mem n active_refs)
                then make_err (err_gave_ownership n))
              names
          in

          let symbol_table' =
            match v with
            | Rebinding rb -> (
                match rb.expr with
                (* don't need to worry about ownership transfer if it is a borrow *)
                | MutBorrow _, _ | Borrow _, _ -> symbol_table
                | _, SIdent n ->
                    let n_node =
                      StringMap.find (StringMap.find n symbol_table) graph
                    in
                    let _, _, _, n_loop = node_common_data n_node in
                    let _ =
                      if rb.loop <> n_loop then
                        raise
                          (Failure
                             ("ownership of " ^ n
                            ^ " could be taken in a previous loop iteration"))
                    in
                    StringMap.remove n symbol_table
                | _ -> symbol_table)
            | _ -> symbol_table
          in

          (symbol_table', active_refs, graph)
    in

    check_children pipe.sname StringMap.empty StringSet.empty graph_for_pipe
  in
  let _graph = print_graph graph in

  (* graph with populated lifetime-owned-vars *)
  let graph =
    List.fold_left
      (fun graph p ->
        let _, _, graph' = ownership_ck p in
        let _ =
          if verbose then
            print_string ("ownership check for " ^ p.sname ^ " passed!\n")
        in
        StringMap.add p.sname graph' graph)
      graph pipes
  in

  let _graph = print_graph graph in

  (* find borrows in an expression *)
  (* returns (name, is_mut) list *)
  let find_borrows (sex : s_expr) borrow_table : (string * bool) list =
    let rec inner ((_t, e) : s_expr) (borrows : (string * bool) list) :
        (string * bool) list =
      match e with
      | SIdent n ->
          if StringMap.mem n borrow_table then
            (n, fst (StringMap.find n borrow_table)) :: borrows
          else []
      | SUnop (Ref, (_typ, SIdent v)) -> (v, false) :: borrows
      | SUnop (MutRef, (_typ, SIdent v)) -> (v, true) :: borrows
      | SThingAccess (_, s, _) -> inner s borrows
      | STupleIndex (s, _) -> inner s borrows
      | SBinop (s1, _, s2) -> inner s2 borrows @ inner s1 borrows
      | SUnop (_, s) -> inner s borrows
      | SPipeIn (_, sl) | STupleValue sl ->
          List.fold_left (fun l s -> inner s l) borrows sl
      (* probably redundant because things can't store borrows but idk *)
      | SThingValue (_, tl) ->
          List.fold_left (fun l (_n, s) -> inner s l) borrows tl
      | _ -> borrows
    in
    inner sex []
  in

  (* if a ref is returned, make sure that it is the smallest of all returnable *)
  (*
    idea - create a return stmt node in our graph. if the return type of the
    pipe is a ref, then we must check to see which argument each return was
    derived. once we have this set, we can take the rightmost (smallest) one
    and make sure it matches the return-type lifetime.
  *)
  let validate_arg_lifetimes p ret_v_map =
    let err_return_lifetime_no_match correct_lifetime =
      "lifetime of return value in " ^ p.sname
      ^ " must be the smallest (rightmost) lifetime of all possible returned \
         arguments: " ^ correct_lifetime
    and err_unnecessary_lifetime v_name =
      "variable " ^ v_name ^ " will not be returned from " ^ p.sname
      ^ " and thus its lifetime isn't required"
    and err_lifetime_not_defined lt =
      "lifetime " ^ lt ^ " used but not defined in " ^ p.sname
    and make_err er = raise (Failure er) in

    if match p.sreturn_type with MutBorrow _ | Borrow _ -> false | _ -> true
    then
      let _ =
        List.iter
          (fun (_, t, n) ->
            match t with
            | MutBorrow (_, lt) | Borrow (_, lt) ->
                if lt <> "'_" then make_err (err_unnecessary_lifetime n)
            | _ -> ())
          p.sformals
      in

      []
    else
      let lt_of_return =
        match p.sreturn_type with
        | MutBorrow (_, lt) | Borrow (_, lt) -> lt
        | _ ->
            make_err "if returning a borrow, it must have an explicit lifetime"
      in

      (* we are kinda gonna have to limit returned values to args *)
      (* vs. allowing re-bindings of refs of args to also be returned *)
      let possible_ret_vars, ident_assoc_lt_map =
        get_possible_ret_args p ret_v_map
      in

      (* make sure dev isnt' overly verbose with lifetime decls *)
      (* a.k.a they aren't adding explicit lifetimes when not necessary *)
      let _ =
        List.iter
          (fun (_, typ, n) ->
            if not (List.mem n possible_ret_vars) then
              match typ with
              | MutBorrow (_, lt) | Borrow (_, lt) ->
                  if lt <> "'_" then make_err (err_unnecessary_lifetime n)
              | _ -> ())
          p.sformals
      in

      let pos_ret_borrows_idxs =
        match p.sreturn_type with
        | Borrow _ | MutBorrow _ ->
            List.rev
              (snd
                 (List.fold_left
                    (fun (idx, filtered_idxs) (_, _, formal_name) ->
                      if List.mem formal_name possible_ret_vars then
                        (idx + 1, idx :: filtered_idxs)
                      else (idx + 1, filtered_idxs))
                    (0, []) p.sformals))
        | _ -> []
      in

      let possible_lts =
        List.filter_map
          (fun (_, typ, n) ->
            if List.mem n possible_ret_vars then
              match typ with
              | MutBorrow _ | Borrow _ ->
                  let _, lt, _ = StringMap.find n ident_assoc_lt_map in
                  Some lt
              | _ -> None
            else None)
          p.sformals
      in
      let smallest_possible_lt =
        List.fold_left
          (fun (smallest, lt_as_str) cur_lt ->
            let rec index_of_lt x lst =
              match lst with
              | [] -> make_err (err_lifetime_not_defined x)
              | h :: t -> if x = h then 0 else 1 + index_of_lt x t
            in
            let i = index_of_lt cur_lt p.slifetimes in
            if i > smallest then (i, cur_lt) else (smallest, lt_as_str))
          (-1, "'_") possible_lts
      in
      if lt_of_return <> snd smallest_possible_lt then
        make_err (err_return_lifetime_no_match (snd smallest_possible_lt))
      else pos_ret_borrows_idxs
  in

  let ret_v_map =
    List.fold_left
      (fun ret_v_map p ->
        (* returns list of possible returned args by idx (iff borrow) *)
        let is_builtin =
          match
            List.find_opt (fun p' -> p'.name = p.sname) built_in_pipe_decls
          with
          | Some _ -> true
          | None -> false
        in

        let ret_ty_is_mutborrow =
          match p.sreturn_type with MutBorrow _ -> true | _ -> false
        in

        let idxs =
          if (not is_builtin) || (is_builtin && ret_ty_is_mutborrow) then
            validate_arg_lifetimes p ret_v_map
          else []
        in

        let _ =
          if verbose then
            print_string
              ("argument lifetime validation for " ^ p.sname ^ " passed!\n")
        in

        let graph_for_pipe = StringMap.find p.sname graph in

        (* helper function for traverse_child *)
        (* makes sure a mutborrow is still usable (i.e. not passed into and returned by a pipe) *)
        let rec check_if_mutborrow_valid (sex : s_expr)
            (data : StringSet.t StringMap.t) =
          match sex with
          | MutBorrow _, SIdent n ->
              if not (StringMap.mem n data) then
                raise
                  (Failure
                     ("Mutable reference " ^ n
                    ^ " cannot be used until its aliases have fallen out of \
                       scope."))
          | _, STupleIndex (se, _) -> check_if_mutborrow_valid se data
          | _, SThingAccess (_, se, _) -> check_if_mutborrow_valid se data
          | _, SUnop (MutRef, (_ty, SIdent n)) ->
              StringMap.iter
                (fun s oset ->
                  if StringSet.mem n oset then
                    raise
                      (Failure
                         ("Mutable reference " ^ n
                        ^ " cannot be used until its alias " ^ s
                        ^ " has fallen out of scope.")))
                data
          | _, SUnop (_, se) -> check_if_mutborrow_valid se data
          | _, SBinop (se1, _, se2) ->
              let _ = check_if_mutborrow_valid se1 data in
              check_if_mutborrow_valid se2 data
          | _, SPipeIn (_, ses) ->
              List.iter (fun se -> check_if_mutborrow_valid se data) ses
          | _ -> ()
        in

        (* each entry in data is: variable_name -> set of possible mutborrow origins *)
        let rec traverse_child c (data : StringSet.t StringMap.t) =
          match c with
          | Lifetime l ->
              let _ =
                List.fold_left
                  (fun data_inner c_inner ->
                    traverse_child
                      (StringMap.find c_inner graph_for_pipe)
                      data_inner)
                  data l.children
              in
              data
          | Binding b -> (
              let _ = check_if_mutborrow_valid b.expr data in
              match b.expr with
              | MutBorrow _, SPipeIn (p_name, args) ->
                  let ret_args_idxs = StringMap.find p_name ret_v_map in
                  let ret_args =
                    List.fold_right
                      (fun (idx : int) (l : s_expr list) ->
                        List.nth args idx :: l)
                      ret_args_idxs []
                  in

                  let accumulate_origins (sex : s_expr) (oset : StringSet.t)
                      (d : StringSet.t StringMap.t) :
                      StringSet.t StringMap.t * StringSet.t =
                    match sex with
                    | MutBorrow _, SIdent n ->
                        let oset_of_n = StringMap.find n data in
                        (* remove mutborrow that will be overwritten by this binding *)
                        let d' = StringMap.remove n d in
                        (d', StringSet.union oset oset_of_n)
                    | _, SUnop (MutRef, (_ty, SIdent o)) ->
                        (d, StringSet.add o oset)
                    | (_, SPipeIn (_, _args)) as e ->
                        let our_origins =
                          StringSet.union oset
                            (StringSet.of_list
                               (origins_of_pipecall p e ret_v_map b.node_id))
                        in
                        (* remove any element from d whose oset is a subset of origins *)
                        let d' =
                          StringMap.filter
                            (fun _n oset_of_n ->
                              StringSet.cardinal
                                (StringSet.inter oset_of_n our_origins)
                              = 0)
                            d
                        in
                        (d', our_origins)
                    | _ -> (d, oset)
                  in

                  (* get the origins of our new mutborrow, the one created by this binding *)
                  (* only care about updating data map if arg is an ident *)
                  (* also remove mutborrows whose priority will be taken by this one *)
                  let data', b_origin_set =
                    List.fold_left
                      (fun (d, oset) arg -> accumulate_origins arg oset d)
                      (data, StringSet.empty) ret_args
                  in

                  let _ =
                    if verbose then
                      print_endline
                        ("origin_set of " ^ b.name ^ ": "
                        ^ String.concat "," (StringSet.elements b_origin_set))
                  in

                  StringMap.add b.name b_origin_set data'
              | (MutBorrow _, _) as sex ->
                  let origin_opt, _ = deepest_origin p sex b.node_id in
                  let origin =
                    match origin_opt with
                    | Some o -> o
                    (* In this case, the binding is a formal of a function *)
                    | None ->
                        let _, idx_of_formal =
                          List.fold_left
                            (fun (is_done, idx) (_, _, n) ->
                              if is_done then (is_done, idx)
                              else if b.name = n then (true, idx)
                              else (false, idx + 1))
                            (false, 0) p.sformals
                        in
                        string_of_int idx_of_formal
                  in
                  let oset = StringSet.add origin StringSet.empty in
                  StringMap.add b.name oset data
              | _ -> data)
          | Rebinding rb ->
              let _ = check_if_mutborrow_valid rb.expr data in
              data
          | PipeCall pc ->
              let _ =
                List.iter (fun a -> check_if_mutborrow_valid a data) pc.args
              in
              data
          | PipeReturn pr ->
              let _ = check_if_mutborrow_valid pr.returned data in
              data
          | ExprCatchAll eca ->
              let _ = check_if_mutborrow_valid eca.value data in
              data
        in
        let _data =
          if not is_builtin then
            traverse_child
              (StringMap.find p.sname graph_for_pipe)
              StringMap.empty
          else StringMap.empty
        in

        StringMap.add p.sname idxs ret_v_map)
      StringMap.empty pipes
  in

  let borrow_ck pipe =
    let graph_for_pipe = StringMap.find pipe.sname graph in

    let err_borrow_after_mut_borrow v_name =
      "variable " ^ v_name
      ^ " can't be borrowed after it has already been mutably borrowed."
    and err_mut_borrow_after_borrow v_name =
      "variable " ^ v_name
      ^ " can't be mutably borrowed after it has already been borrowed."
    and err_local_borrow_returned v_name =
      "variable " ^ v_name
      ^ " can't be returned as it was defined locally. You can only"
      ^ "return references to pipe arguments."
    and err_rebinding_of_immutable_binding v_name =
      "variable " ^ v_name
      ^ " can't be rebound as it was defined as immutable. please use the mut \
         keyword."
    and err_reassign_of_borrowed v_name =
      "variable " ^ v_name ^ " can't be reassigned as it is already borrowed."
    and err_binding_outlives_borrow =
      "the variable being bound is outlived"
      ^ " by the definition to which it is being bound."
    and make_err er = raise (Failure er) in

    let assert_binding_mutable (node_id : string) (name : string) : unit =
      let node_opt, _ = get_depth_of_defn graph_for_pipe node_id name in
      let node =
        match node_opt with None -> make_err "swedish panic!" | Some v -> v
      in
      let _ =
        match node with
        | Binding b ->
            if not b.is_mut then
              make_err
                ("can't take out mutable borrow on an immutable binding " ^ name
               ^ ". please use the 'mut' keyword in the original binding.")
        | _ -> make_err "How is this not a binding?!"
      in
      ()
    in

    (* all additions will be at the current depth because *)
    (* it is just an expression that will be instantly evaluated *)
    let ck_expr borrow_table node_id cur_depth e =
      let borrows_in_expr = find_borrows e borrow_table in
      let borrow_table' =
        List.fold_left
          (fun borrow_table (n, is_mut) ->
            if StringMap.mem n borrow_table then
              let is_mut', borrows = StringMap.find n borrow_table in
              if is_mut' then make_err (err_borrow_after_mut_borrow n)
              else if is_mut then make_err (err_mut_borrow_after_borrow n)
              else
                StringMap.add n
                  (is_mut', (node_id, cur_depth) :: borrows)
                  borrow_table
            else
              let _ = if is_mut then assert_binding_mutable node_id n in
              StringMap.add n (is_mut, [ (node_id, cur_depth) ]) borrow_table)
          borrow_table borrows_in_expr
      in
      borrow_table'
    in

    let validate_p_call_args called_name args cur_node_id =
      let called_pipe = List.find (fun p -> p.sname = called_name) pipes in
      if List.length called_pipe.slifetimes = 0 then []
      else
        let call_formals_w_lts =
          let formals = called_pipe.sformals in
          List.filter_map
            (fun (i, (_, typ, n)) ->
              match typ with
              | Borrow (_, lt) ->
                  if lt = "'_" then None else Some (false, i, n, lt)
              | MutBorrow (_, lt) ->
                  if lt = "'_" then None else Some (true, i, n, lt)
              | _ -> None)
            (List.rev
               (snd
                  (List.fold_left
                     (fun (i, prev) f -> (i + 1, (i, f) :: prev))
                     (0, []) formals)))
        in

        let rec index_of_lt x lst =
          match lst with
          | [] -> make_err "panic!"
          | h :: t -> if x = h then 0 else 1 + index_of_lt x t
        in

        (* sort declaration args from longest to shortest lifetime *)
        let sorted =
          List.sort
            (fun (_m1, _i1, _n1, lt1) (_m2, _i2, _n2, lt2) ->
              index_of_lt lt1 called_pipe.slifetimes
              - index_of_lt lt2 called_pipe.slifetimes)
            call_formals_w_lts
        in

        let _ =
          if verbose then
            print_string
              (String.concat " | "
                 (List.map
                    (fun (_mut, arg_id, n, lt) ->
                      string_of_int arg_id ^ " --> " ^ lt ^ " " ^ n)
                    sorted)
              ^ "\n")
        in

        (* list of argument indices and their associated max origin depth *)
        let borrowed_args =
          snd
            (List.fold_left
               (fun (i, args) arg ->
                 let n, deepest = deepest_origin pipe arg cur_node_id in
                 match n with
                 | None -> (i + 1, args)
                 | Some n ->
                     if deepest = -1 then (i + 1, args)
                     else (i + 1, (i, n, deepest) :: args))
               (0, []) args)
        in

        (* make sure mutably borrowed args don't come from same origin *)
        let _ =
          List.fold_left
            (fun running_origins ((ty, _se) as arg) ->
              match ty with
              | MutBorrow _ ->
                  let our_origins =
                    origins_of_pipecall pipe arg ret_v_map cur_node_id
                  in
                  List.fold_left
                    (fun running_origins o ->
                      if List.mem o running_origins then
                        make_err
                          ("argument " ^ string_of_s_expr arg ^ " of pipe "
                         ^ called_name
                         ^ " is a mutable borrow on already mutably borrowed \
                            value " ^ o)
                      else o :: running_origins)
                    running_origins our_origins
              | _ -> running_origins)
            [] args
        in

        (* we only care about arguments that correspond to formals with lifetimes *)
        let borrowed_args_corresponding_to_explicit_lts =
          List.filter_map
            (fun (i, n, d) ->
              let _, corresponding_formal_typ, _ =
                List.nth called_pipe.sformals i
              in
              match corresponding_formal_typ with
              | MutBorrow (_, lt) | Borrow (_, lt) ->
                  if lt = "'_" then None else Some (i, n, d)
              | _ ->
                  make_err
                    ("when calling " ^ called_pipe.sname ^ ", arg " ^ n
                   ^ " is a borrow but the matching formal is not.  \n\
                     \                Shouldn't we have resolved this in \
                      semant?"))
            borrowed_args
        in

        (* sort from smallest to largest depth (longest -> shortest lt) *)
        let borrowed_args_sorted =
          List.sort
            (fun (i1, _n1, d1) (i2, _n2, d2) ->
              if d1 = d2 then i1 - i2 else d1 - d2)
            borrowed_args_corresponding_to_explicit_lts
        in

        let _ =
          if verbose then
            let _ =
              print_string (called_pipe.sname ^ " borrowed_args_sorted: ")
            in
            print_string
              (String.concat " | "
                 (List.map
                    (fun (i, _n, depth) ->
                      string_of_int i ^ " --> " ^ string_of_int depth)
                    borrowed_args_sorted)
              ^ "\n")
        in

        let err_explicit_arg_invalid called_pipe =
          "argument lifetimes in calls to " ^ called_pipe
          ^ " must match the explicitly defined lifetimes for that pipe \
             definition."
        in

        let combined = List.combine sorted borrowed_args_sorted in

        (* check for mismatched lifetimes, i.e. if a function takes in a &'a, &'a borrows,
           then args passed in should have same lifetime, i.e. same depth *)
        let _ =
          if List.length combined > 1 then
            let _ =
              List.fold_left
                (fun idx ((_, _, n, lt), (_, n1, d)) ->
                  if idx + 1 < List.length combined then
                    let _, _, n', lt' = List.nth sorted (idx + 1) in
                    let _, n1', d' = List.nth borrowed_args_sorted (idx + 1) in
                    if d = d' && lt <> lt' then
                      make_err
                        ("arguments " ^ n1 ^ " and " ^ n1'
                       ^ " passed into the pipe " ^ called_pipe.sname
                       ^ " have equivalent lifetimes, however, their lifetimes "
                       ^ lt ^ " and " ^ lt' ^ " differ.")
                    else if d <> d' && lt = lt' then
                      make_err
                        ("in declaration of " ^ called_pipe.sname ^ ", formals "
                       ^ n ^ " and " ^ n' ^ " have the same lifetime " ^ lt
                       ^ ", but arguments " ^ n1 ^ " and " ^ n1'
                       ^ " have different depths.")
                    else idx + 1
                  else idx)
                0 combined
            in
            ()
        in

        List.map
          (fun ((m, i1, _, _), (i2, n, d)) ->
            if i1 <> i2 then
              make_err (err_explicit_arg_invalid called_pipe.sname)
            else (m, n, d, i1))
          combined
    in

    (*
      return:
        - borrow table
      
      params:
        - current node id
        - current depth
        - borrow table

      borrow table:
        - ident --> (is_mut, [(node_id_of_borrow, max_depth)])
          - max_depth is the highest depth that a borrow
            can be re-assigned to.

      Note: at the end of each lifetime, we can go through the map and remove
            all items whose max_depth value is equivalent to the current depth

      example:
        - if we bind the return value of a function which could be the borrow
          of either a variable `a`, or a variable `b`, to a variable `c`
          which is in the parent lifetime we would return the following:

            ...
            a --> (false, [...(node_id_of_rebind, cur_height - 1)])
            b --> (false, [...(node_id_of_rebind, cur_height - 1)])

    *)
    let rec check_children' cur_node_id cur_depth borrow_table =
      let current_node = StringMap.find cur_node_id graph_for_pipe in
      match current_node with
      | Lifetime l ->
          (* check all children in order, removing expired entries *)
          (* after visiting each child branch *)
          let borrow_table' =
            List.fold_left
              (fun borrow_table child ->
                (* check child *)
                let borrow_table' =
                  check_children' child (cur_depth + 1) borrow_table
                in

                borrow_table')
              borrow_table l.children
          in
          (* remove all entries that expired *)
          let borrow_table' =
            StringMap.filter_map
              (fun _k (is_mut, borrows) ->
                let borrows' =
                  List.filter
                    (fun (_nid, max_depth) -> max_depth <= cur_depth)
                    borrows
                in
                if List.length borrows' = 0 then None
                else Some (is_mut, borrows'))
              borrow_table'
          in

          borrow_table'
      | Binding b -> (
          (* we don't need to handle idents because *)
          (* that would be an ownership problem *)
          match b.expr with
          | _ty, SUnop (Ref, (_ty2, SIdent n)) ->
              (* new immutable borrow *)
              let n_is_borrowed = StringMap.mem n borrow_table in

              if n_is_borrowed then
                let borrow_is_mut, borrow_node_ids =
                  StringMap.find n borrow_table
                in
                (* if already mutably borrowed *)
                if borrow_is_mut then make_err (err_borrow_after_mut_borrow n)
                else
                  (* if already immutably borrowed *)
                  (* note: can re-use origin depth from prev. borrow *)
                  StringMap.add n
                    (false, (b.node_id, cur_depth) :: borrow_node_ids)
                    borrow_table
              else
                StringMap.add n (false, [ (b.node_id, cur_depth) ]) borrow_table
          | _ty, SUnop (MutRef, (_ty2, SIdent n)) ->
              (* new mutable borrow *)
              let n_is_borrowed = StringMap.mem n borrow_table in
              (* if already borrowed *)
              if n_is_borrowed then make_err (err_mut_borrow_after_borrow n)
                (* if not borrowed *)
              else
                let _ = assert_binding_mutable b.node_id n in
                StringMap.add n (true, [ (b.node_id, cur_depth) ]) borrow_table
          | _ty, SPipeIn (p_name, args) ->
              (* validate all arguments that may borrow things *)
              (* but don't worry about the resulting borrow table *)
              (* as they will be dropped after the call *)
              let _borrow_table' =
                List.fold_left
                  (fun borrow_table arg ->
                    ck_expr borrow_table b.node_id cur_depth arg)
                  borrow_table args
              in

              (* validate that the lifetimes of the arguments *)
              (* align with the explicit lifetimes defined in the called *)
              (* pipe declaration (iff return type is a borrow) *)
              let borrowed_args = validate_p_call_args p_name args b.node_id in

              let sexprs_of_borrowed_args =
                List.rev
                  (List.fold_left
                     (fun l (_m, _n, _d, i) -> List.nth args i :: l)
                     [] borrowed_args)
              in

              (* get max depth of all the args *)
              (* until which we will hold these borrows *)
              let max_arg_depth =
                List.fold_left
                  (fun m (_, _, d, _i) -> if d > m then d else m)
                  0 borrowed_args
              in

              let borrow_table' =
                List.fold_left2
                  (fun borrow_table (m, n, _d, _i) sex ->
                    if StringMap.mem n borrow_table then
                      let is_mut, borrows = StringMap.find n borrow_table in
                      if is_mut then
                        match sex with
                        | _, SUnop (Ref, _) | _, SUnop (MutRef, _) ->
                            make_err (err_borrow_after_mut_borrow n)
                        | _ ->
                            StringMap.add n
                              (true, (b.node_id, max_arg_depth) :: borrows)
                              borrow_table
                      else if m then make_err (err_mut_borrow_after_borrow n)
                      else
                        StringMap.add n
                          (false, (b.node_id, max_arg_depth) :: borrows)
                          borrow_table
                    else
                      let _ = if m then assert_binding_mutable b.node_id n in
                      StringMap.add n
                        (m, [ (b.node_id, max_arg_depth) ])
                        borrow_table)
                  borrow_table borrowed_args sexprs_of_borrowed_args
              in

              borrow_table'
          (* TODO: IDENT THAT IS A REF *)
          (* if the rhs is an arbitrary expr, check for borrows *)
          | e ->
              let _ = ck_expr borrow_table b.node_id cur_depth e in
              borrow_table)
      | Rebinding rb -> (
          (* make sure original binding is mutable *)
          let origin_node, origin_depth =
            get_depth_of_defn graph_for_pipe rb.node_id rb.name
          in

          let _ =
            match rb.expr with
            | Borrow _, SUnop (_, (_, SIdent n)) | Borrow _, SIdent n ->
                let _origin_of_n, origin_depth_of_n =
                  get_depth_of_defn graph_for_pipe rb.node_id n
                in
                if origin_depth_of_n > origin_depth then
                  make_err
                    ("Immutable borrow " ^ string_of_s_expr rb.expr
                   ^ " has larger lifetime than " ^ rb.name ^ ", so " ^ rb.name
                   ^ " cannot be rebound.")
            | _ -> ()
          in

          let _ =
            match origin_node with
            | Some (Binding b) ->
                if not b.is_mut then
                  make_err (err_rebinding_of_immutable_binding rb.name)
                else if StringMap.mem b.name borrow_table then
                  make_err (err_reassign_of_borrowed b.name)
            | _ ->
                make_err
                  ("panic! couldn't find original definition of " ^ rb.name)
          in

          let remove_borrows_for_binding borrow_table =
            (* remove the borrow from the currently-borrowed-ident *)
            (* as the borrow will be replaced by our new one *)
            let borrow_table' =
              StringMap.map
                (fun (is_mut, borrows) ->
                  ( is_mut,
                    List.filter
                      (fun (nid, _depth) ->
                        let node_of_borrow =
                          StringMap.find nid graph_for_pipe
                        in
                        match node_of_borrow with
                        | Binding b2 -> b2.name <> rb.name
                        | Rebinding rb2 -> rb2.name <> rb.name
                        | _ -> true)
                      borrows ))
                borrow_table
            in

            (* remove entries who have no more borrows on them *)
            let borrow_table' =
              StringMap.filter
                (fun _ (_, bs) -> List.length bs > 0)
                borrow_table'
            in

            borrow_table'
          in

          let borrow_table' = remove_borrows_for_binding borrow_table in

          match rb.expr with
          (* rebinding of some borrow to a new mutable borrow *)
          | _ty, SUnop (MutRef, (_ty2, SIdent n)) ->
              let has_b_map_entry = StringMap.mem n borrow_table' in
              if has_b_map_entry then make_err (err_mut_borrow_after_borrow n)
              else
                (* get the origin depth of the new borrow, add it *)
                let _, b_origin_depth =
                  get_depth_of_defn graph_for_pipe rb.node_id rb.name
                in
                (* binding outlives borrow... unsafe! *)
                if b_origin_depth > origin_depth then
                  make_err err_binding_outlives_borrow
                else
                  let _ = assert_binding_mutable rb.node_id n in
                  StringMap.add n
                    (true, [ (rb.node_id, b_origin_depth) ])
                    borrow_table'
              (* rebinding of some borrow to a new immutable borrow *)
          | _ty, SUnop (Ref, (_ty2, SIdent n)) ->
              let has_b_map_entry = StringMap.mem n borrow_table' in
              if has_b_map_entry then
                let is_mut, bs = StringMap.find n borrow_table' in
                if is_mut then make_err (err_borrow_after_mut_borrow n)
                else
                  let _, b_origin_depth =
                    get_depth_of_defn graph_for_pipe rb.node_id rb.name
                  in
                  StringMap.add n
                    (false, (rb.node_id, b_origin_depth) :: bs)
                    borrow_table'
              else
                (* get the origin depth of the new borrow, add it *)
                let _, b_origin_depth =
                  get_depth_of_defn graph_for_pipe rb.node_id rb.name
                in
                (* binding outlives borrow... unsafe! *)
                if b_origin_depth > origin_depth then
                  make_err err_binding_outlives_borrow
                else
                  StringMap.add n
                    (true, [ (rb.node_id, b_origin_depth) ])
                    borrow_table'
          | _ty, SPipeIn (p_name, args) ->
              (* validate all arguments that may borrow things *)
              (* but don't worry about the resulting borrow table *)
              (* as they will be dropped after the call *)
              let _borrow_table' =
                List.fold_left
                  (fun borrow_table arg ->
                    ck_expr borrow_table rb.node_id cur_depth arg)
                  borrow_table' args
              in

              (* validate that the lifetimes of the arguments *)
              (* align with the explicit lifetimes defined in the called *)
              (* pipe declaration (iff return type is a borrow) *)
              let borrowed_args = validate_p_call_args p_name args rb.node_id in

              (* get max depth of all the args *)
              (* until which we will hold these borrows *)
              let max_arg_depth =
                List.fold_left
                  (fun m (_, _, d, _i) -> if d > m then d else m)
                  0 borrowed_args
              in

              let _depth_check =
                if max_arg_depth > origin_depth then
                  make_err err_binding_outlives_borrow
              in

              let borrow_table' =
                List.fold_left
                  (fun borrow_table (m, n, _d, _i) ->
                    (* binding outlives borrow... unsafe! *)
                    if StringMap.mem n borrow_table then
                      let is_mut, borrows = StringMap.find n borrow_table in
                      if is_mut then make_err (err_borrow_after_mut_borrow n)
                      else if m then make_err (err_mut_borrow_after_borrow n)
                      else
                        StringMap.add n
                          (false, (rb.node_id, max_arg_depth) :: borrows)
                          borrow_table
                    else
                      let _ = if m then assert_binding_mutable rb.node_id n in
                      StringMap.add n
                        (m, [ (rb.node_id, max_arg_depth) ])
                        borrow_table)
                  borrow_table' borrowed_args
              in

              borrow_table'
          | e -> ck_expr borrow_table' rb.node_id cur_depth e)
      | PipeCall pc ->
          (* validate all arguments that may borrow things *)
          (* but these shouldn't be held past this validation *)
          (* because there is no return *)
          let _borrow_table' =
            List.fold_left
              (fun borrow_table arg ->
                ck_expr borrow_table pc.node_id cur_depth arg)
              borrow_table pc.args
          in

          (* return values that aren't borrow or unit need to be assigned *)
          (* to an owner *)
          let pipe = List.find (fun p -> p.sname = pc.pipe_name) pipes in
          let _ =
            match pipe.sreturn_type with
            | Borrow _ | MutBorrow _ | Unit -> ()
            | _ ->
                make_err
                  "If the returned value from the call to isn't a borrow or \
                   unit, it must be assigned to an owner."
          in

          (* validate that the lifetimes of the arguments *)
          (* align with the explicit lifetimes defined in the called *)
          (* pipe declaration *)
          let _ = validate_p_call_args pc.pipe_name pc.args pc.node_id in

          borrow_table
      | PipeReturn pr ->
          (* make sure the returned value is an arg if it *)
          (* is a borrow *)
          let _ =
            match pr.returned with
            | _ty, SUnop (MutRef, (_ty2, SIdent n))
            | _ty, SUnop (Ref, (_ty2, SIdent n)) -> (
                match
                  List.find_opt (fun (_, _, f_name) -> f_name = n) pipe.sformals
                with
                | Some _ -> ()
                | None -> make_err (err_local_borrow_returned n))
            | _ -> ()
          in

          (* then just validate the expression *)
          let borrow_table' =
            ck_expr borrow_table pr.node_id cur_depth pr.returned
          in

          borrow_table'
      | ExprCatchAll eca ->
          (* validate the expression and update the table *)
          ck_expr borrow_table eca.node_id cur_depth eca.value
    in

    let _ = check_children' pipe.sname 0 StringMap.empty in

    ()
  in

  let _ =
    List.iter
      (fun p ->
        let _ = borrow_ck p in
        if verbose then
          print_string ("borrow check for " ^ p.sname ^ " passed!\n"))
      pipes
  in

  let id_owned_vars_table =
    List.fold_left
      (fun map (_, p_graph) ->
        List.fold_left
          (fun map' (_, v) ->
            match v with
            | Lifetime l -> (
                match l.assoc_sblock_id with
                | None -> map'
                | Some sbid ->
                    if List.length l.owned_vars = 0 then map'
                    else StringMap.add sbid l.owned_vars map')
            | _ -> map')
          map
          (StringMap.bindings p_graph))
      StringMap.empty (StringMap.bindings graph)
  in

  id_owned_vars_table
