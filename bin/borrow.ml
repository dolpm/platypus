open Sast
open Ast
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

type graph_node =
  | Lifetime of {
      parent : string option;
      children : string list;
      node_id : string;
      (* rest *)
      owned_vars : string list;
    }
  | Binding of {
      parent : string option;
      node_id : string;
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
      (* rest *)
      name : string;
      expr : s_expr;
    }
  | PipeCall of {
      parent : string option;
      node_id : string;
      (* rest *)
      pipe_name : string;
      args : s_expr list;
    }
  | PipeReturn of {
      parent : string option;
      node_id : string;
      (* rest *)
      returned : s_expr;
    }
  | ExprCatchAll of {
      parent : string option;
      node_id : string;
      (* rest *)
      value : s_expr;
    }

let borrow_ck pipes verbose =
  let _ = if verbose then print_string "generating graph!\n" else () in
  let generate_graph_for_pipe pipe =
    (* create top-level nodes for pipe arguments and add to graph *)
    let graph_with_pipe_args =
      snd
        (List.fold_left
           (fun (num_child, graph) (is_mut, typ, name) ->
             let child_id = pipe.sname ^ "." ^ string_of_int num_child in
             ( num_child + 1,
               StringMap.add child_id
                 (Binding
                    {
                      parent = Some pipe.sname;
                      node_id = child_id;
                      is_mut;
                      typ;
                      name;
                      expr = (Unit, SNoexpr);
                    })
                 graph ))
           (0, StringMap.empty) pipe.sformals)
    in

    (* create graph node for an arbitrary statement, recurse if needed *)
    (*
         returns (bool * graph) where bool denotes whether any nodes
         were added.
      *)
    let rec gen_children parent_id child_id stmt graph =
      match stmt with
      | SBlock (stmts, _owned_vars) ->
          let node_id = parent_id ^ "." ^ string_of_int child_id in
          let _, updated_graph, children =
            List.fold_left
              (fun (child_id, graph, children) stmt ->
                let was_updated, graph =
                  gen_children node_id child_id stmt graph
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
                   node_id;
                   children = List.rev children;
                   owned_vars = [];
                 })
              updated_graph )
      | SAssign (is_mut, typ, name, expr) ->
          let node_id = parent_id ^ "." ^ string_of_int child_id in
          ( true,
            StringMap.add node_id
              (Binding
                 { parent = Some parent_id; node_id; is_mut; typ; name; expr })
              graph )
      | SReAssign (name, expr) ->
          let node_id = parent_id ^ "." ^ string_of_int child_id in
          ( true,
            StringMap.add node_id
              (Rebinding { parent = Some parent_id; node_id; name; expr })
              graph )
      | SExpr (_typ_of_exp, SPipeIn (pipe_name, args)) ->
          let node_id = parent_id ^ "." ^ string_of_int child_id in
          ( true,
            StringMap.add node_id
              (PipeCall { parent = Some parent_id; node_id; pipe_name; args })
              graph )
      | SPipeOut expr ->
          let node_id = parent_id ^ "." ^ string_of_int child_id in
          ( true,
            StringMap.add node_id
              (PipeReturn { parent = Some parent_id; node_id; returned = expr })
              graph )
      | sstmt ->
          let node_id = parent_id ^ "." ^ string_of_int child_id in
          let matched, graph', children =
            match sstmt with
            | SWhile (sex, SBlock (stmts, [])) ->
                let sex_id = node_id ^ ".0" in
                let block_id = node_id ^ ".1" in
                let sex_node =
                  ExprCatchAll
                    { parent = Some node_id; node_id = sex_id; value = sex }
                in

                let graph' = StringMap.add sex_id sex_node graph in

                let _, graph' =
                  gen_children node_id 1 (SBlock (stmts, [])) graph'
                in

                (true, graph', [ sex_id; block_id ])
            | SIf (sex, SBlock (stmts1, []), SBlock (stmts2, [])) ->
                let sex_id = node_id ^ ".0" in
                let block1_id = node_id ^ ".1" in
                let block2_id = node_id ^ ".2" in

                let sex_node =
                  ExprCatchAll
                    { parent = Some node_id; node_id = sex_id; value = sex }
                in

                let graph' = StringMap.add sex_id sex_node graph in

                let _, graph' =
                  gen_children node_id 1 (SBlock (stmts1, [])) graph'
                in
                let _, graph' =
                  gen_children node_id 2 (SBlock (stmts2, [])) graph'
                in

                (true, graph', [ sex_id; block1_id; block2_id ])
            | SLoop (sex1, sex2, _, sex3, SBlock (stmts, [])) ->
                let sex1_id = node_id ^ ".0" in
                let sex2_id = node_id ^ ".1" in
                let sex3_id = node_id ^ ".2" in
                let block_id = node_id ^ ".3" in
                let sex_nodes =
                  [
                    ExprCatchAll
                      { parent = Some node_id; node_id = sex1_id; value = sex1 };
                    ExprCatchAll
                      { parent = Some node_id; node_id = sex2_id; value = sex2 };
                    ExprCatchAll
                      { parent = Some node_id; node_id = sex3_id; value = sex3 };
                  ]
                in

                let graph' =
                  List.fold_left
                    (fun g sex_node ->
                      match sex_node with
                      | ExprCatchAll sn ->
                          StringMap.add sn.node_id (ExprCatchAll sn) g
                      | _ -> raise (Failure "panic!"))
                    graph sex_nodes
                in

                let _, graph' =
                  gen_children node_id 3 (SBlock (stmts, [])) graph'
                in

                (true, graph', [ sex1_id; sex2_id; sex3_id; block_id ])
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
                     children;
                     owned_vars = [];
                   })
                graph' )
    in

    (* create a block containing the body statements and gen thier nodes *)
    let graph_with_pipe_body =
      snd
        (gen_children pipe.sname
           (StringMap.cardinal graph_with_pipe_args)
           (SBlock (pipe.sbody, []))
           graph_with_pipe_args)
    in

    (* TODO: owned vars here should just be the non-borrow args *)
    let pipe_graph =
      StringMap.add pipe.sname
        (Lifetime
           {
             parent = None;
             children =
               (pipe.sname ^ "."
               ^ string_of_int (StringMap.cardinal graph_with_pipe_args))
               :: List.map
                    (fun (k, _) -> k)
                    (StringMap.bindings graph_with_pipe_args);
             node_id = pipe.sname;
             owned_vars = [];
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
      | SThingValue tl -> List.fold_left (fun l (_n, s) -> inner s l) names tl
      | _ -> names
    in
    inner sex []
  in

  let ownership_ck pipe =
    let graph_for_pipe = StringMap.find pipe.sname graph in

    let err_gave_ownership v_name =
      "variable " ^ v_name
      ^ " gave ownership to another binding and can't be accessed."
    and make_err er = raise (Failure er) in

    let rec check_children current_node symbol_table graph =
      let current_node = StringMap.find current_node graph in
      match current_node with
      | Lifetime l ->
          (* process ownership of child nodes -- this will remove all variables who are owned in lower lifetimes *)
          let symbol_table', graph' =
            List.fold_left
              (fun (st, graph) child -> check_children child st graph)
              (symbol_table, graph) l.children
          in

          (* anything left in the symbol table are variables that this lifetime must deallocate *)
          let symbol_table', children_responsible_for_dealloc =
            List.fold_left
              (fun (st', dealloc_list) child ->
                match StringMap.find child graph_for_pipe with
                | Binding b ->
                    if StringSet.mem b.name st' then
                      (StringSet.remove b.name st', b.name :: dealloc_list)
                    else (st', dealloc_list)
                | _ -> (st', dealloc_list))
              (symbol_table', []) l.children
          in

          ( symbol_table',
            StringMap.add l.node_id
              (Lifetime
                 {
                   children = l.children;
                   node_id = l.node_id;
                   parent = l.parent;
                   owned_vars = children_responsible_for_dealloc;
                 })
              graph' )
      | Binding b ->
          let names = find_identifiers b.expr in
          (* make sure all idents in expr are in symbol table *)
          let _ =
            List.iter
              (fun n ->
                if not (StringSet.mem n symbol_table) then
                  make_err (err_gave_ownership n))
              names
          in

          (* if ownership of another var given to new binding *)
          (* remove the original from the table *)
          let symbol_table' =
            match b.expr with
            | _ty, SIdent v_name -> StringSet.remove v_name symbol_table
            | _ -> symbol_table
          in

          (* add the current binding to the table *)
          let symbol_table' = StringSet.add b.name symbol_table' in
          (symbol_table', graph)
      (* in ExprCatchAll, we only need to check that all found identifiers are in the symbol_table table *)
      (* we don't have to remove anything from the symbol table *)
      | PipeCall pc ->
          let names =
            List.fold_left
              (fun idents sex -> find_identifiers sex @ idents)
              [] pc.args
          in
          let _ =
            List.iter
              (fun n ->
                if not (StringSet.mem n symbol_table) then
                  make_err (err_gave_ownership n))
              names
          in
          (symbol_table, graph)
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
                if not (StringSet.mem n symbol_table) then
                  make_err (err_gave_ownership n))
              names
          in
          (symbol_table, graph)
    in

    check_children pipe.sname StringSet.empty graph_for_pipe
  in

  (* graph with populated lifetime-owned-vars *)
  let graph =
    List.fold_left
      (fun graph p ->
        let _, graph' = ownership_ck p in
        let _ =
          if verbose then
            print_string ("ownership check for " ^ p.sname ^ " passed!\n")
        in
        StringMap.add p.sname graph' graph)
      graph pipes
  in

  (* find borrows in an expression *)
  (* returns (name, is_mut) list *)
  let find_borrows (sex : s_expr) : (string * bool) list =
    let rec inner ((_t, e) : s_expr) (borrows : (string * bool) list) :
        (string * bool) list =
      match e with
      | SUnop (Ref, (_typ, SIdent v)) -> (v, false) :: borrows
      | SUnop (MutRef, (_typ, SIdent v)) -> (v, true) :: borrows
      | SBinop (s1, _, s2) -> inner s2 borrows @ inner s1 borrows
      | SUnop (_, s) -> inner s borrows
      | SPipeIn (_, sl) | STupleValue sl ->
          List.fold_left (fun l s -> inner s l) borrows sl
      | SThingValue tl -> List.fold_left (fun l (_n, s) -> inner s l) borrows tl
      | _ -> borrows
    in
    inner sex []
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
    and make_err er = raise (Failure er) in

    let ck_expr borrow_table node_id e =
      let borrows_in_expr = find_borrows e in
      let borrow_table' =
        List.fold_left
          (fun borrow_table (n, is_mut) ->
            if StringMap.mem n borrow_table then
              let borrow_is_mut, borrow_node_ids =
                StringMap.find n borrow_table
              in
              if borrow_is_mut then make_err (err_borrow_after_mut_borrow n)
              else if is_mut then make_err (err_mut_borrow_after_borrow n)
              else
                StringMap.add n
                  (borrow_is_mut, node_id :: borrow_node_ids)
                  borrow_table
            else StringMap.add n (is_mut, [ node_id ]) borrow_table)
          borrow_table borrows_in_expr
      in
      borrow_table'
    in

    let rec check_children current_node borrow_table =
      let current_node = StringMap.find current_node graph_for_pipe in
      match current_node with
      | Lifetime l ->
          (* build borrow table up from left to right child *)
          (* we can throw out the table updates once we break out of *)
          (* this scope (since the borrows will all fall out) *)
          let _borrow_table' =
            List.fold_left
              (fun borrow_table child -> check_children child borrow_table)
              borrow_table l.children
          in
          borrow_table
      | Binding b -> (
          (* check if rhs borrows anything in expr (validate invariants) *)
          (* if it is a borrow itself, add to borrow table *)
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
                  StringMap.add n
                    (false, b.node_id :: borrow_node_ids)
                    borrow_table
              else StringMap.add n (false, [ b.node_id ]) borrow_table
          | _ty, SUnop (MutRef, (_ty2, SIdent n)) ->
              (* new mutable borrow *)
              let n_is_borrowed = StringMap.mem n borrow_table in
              (* if already borrowed *)
              if n_is_borrowed then make_err (err_mut_borrow_after_borrow n)
                (* if not borrowed *)
              else StringMap.add n (true, [ b.node_id ]) borrow_table
          | e ->
              let borrows_in_expr = find_borrows e in
              let borrow_table' =
                List.fold_left
                  (fun borrow_table (n, is_mut) ->
                    let has_b_map_entry = StringMap.mem n borrow_table in
                    if has_b_map_entry then
                      let borrow_is_mut, borrow_node_ids =
                        StringMap.find n borrow_table
                      in
                      if is_mut then make_err (err_mut_borrow_after_borrow n)
                      else if (* if current borrow is mutable *)
                              borrow_is_mut
                      then make_err (err_borrow_after_mut_borrow n)
                      else
                        StringMap.add n
                          (borrow_is_mut, b.node_id :: borrow_node_ids)
                          borrow_table (* this is the first borrow on n *)
                    else StringMap.add n (is_mut, [ b.node_id ]) borrow_table)
                  borrow_table borrows_in_expr
              in
              borrow_table')
      | Rebinding rb ->
          (* if we are rebinding, perhaps we want to remove the old borrow *)
          (* from the table entry for whatever it was borrowing *)
          (* in lieu of the new one... we'd need to store nodes w/ borrows *)
          let _ =
            match rb.expr with
            | Borrow _, e | MutBorrow _, e -> (
                (* if we are rebinding a borrow , we want to remove the *)
                (* old borrow from the table entry for whatever it was *)
                (* borrowing in lieu of the new one *)
                match e with
                | SUnop (MutRef, (_ty2, SIdent n)) ->
                    let borrow_table' =
                      StringMap.map
                        (fun (is_mut, borrowing_node_ids) ->
                          ( is_mut,
                            List.filter
                              (fun nid -> nid != rb.node_id)
                              borrowing_node_ids ))
                        borrow_table
                    in
                    let has_b_map_entry = StringMap.mem n borrow_table' in
                    if has_b_map_entry then
                      make_err (err_mut_borrow_after_borrow n)
                    else StringMap.add n (true, [ rb.node_id ]) borrow_table'
                | SUnop (Ref, (_ty2, SIdent n)) ->
                    let borrow_table' =
                      StringMap.map
                        (fun (is_mut, borrowing_node_ids) ->
                          ( is_mut,
                            List.filter
                              (fun nid -> nid != rb.node_id)
                              borrowing_node_ids ))
                        borrow_table
                    in
                    let has_b_map_entry = StringMap.mem n borrow_table' in
                    if has_b_map_entry then
                      let entry_is_mut, entry_node_ids =
                        StringMap.find n borrow_table'
                      in
                      if entry_is_mut then
                        make_err (err_borrow_after_mut_borrow n)
                      else
                        StringMap.add n
                          (false, rb.node_id :: entry_node_ids)
                          borrow_table'
                    else StringMap.add n (true, [ rb.node_id ]) borrow_table'
                | _ -> ck_expr borrow_table rb.node_id rb.expr)
            | _ -> borrow_table
          in

          (* remove the borrow from the prev. bound value *)
          borrow_table
      | PipeCall pc ->
          (* validate all arguments that may borrow things *)
          let borrow_table' =
            List.fold_left
              (fun borrow_table arg -> ck_expr borrow_table pc.node_id arg)
              borrow_table pc.args
          in
          (* and figure out how to handle the return value *)
          borrow_table'
      | PipeReturn pr ->
          (* if the returned value is a borrow, make sure it's an arg *)
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
          let borrow_table' = ck_expr borrow_table pr.node_id pr.returned in
          borrow_table'
      | ExprCatchAll eca ->
          (* validate the expression and update the table *)
          ck_expr borrow_table eca.node_id eca.value
    in

    let _ = check_children pipe.sname StringMap.empty in
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

  let _graph = print_graph graph in

  ()
