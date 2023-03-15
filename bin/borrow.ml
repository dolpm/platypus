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
          (* TODO: fill out owned vars *)
          (* just look for children that are non-borrow bindings *)
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
      | SLoop (_, _, _, _, SBlock (stmts, []))
      | SWhile (_, SBlock (stmts, []))
      | SIf (_, SBlock (stmts, []), SBlock ([], [])) ->
          gen_children parent_id child_id (SBlock (stmts, [])) graph
      | SIf (_, SBlock (stmts, []), SBlock (stmts_2, [])) ->
          let was_updated, m1 =
            gen_children parent_id child_id (SBlock (stmts, [])) graph
          in
          gen_children parent_id
            (if was_updated then child_id + 1 else child_id)
            (SBlock (stmts_2, []))
            m1
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
      | _ -> (false, graph)
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
  let _print_graph =
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
        graph
  in

  let _ = if verbose then print_string "generated graph!\n" else () in

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

          let _ =
            print_string (string_of_int (StringSet.cardinal symbol_table'))
          in
          let _ = print_string "\n" in

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
          (* Check all names are in scope *)
          let _ =
            List.iter
              (fun n ->
                if not (StringSet.mem n symbol_table) then
                  make_err (err_gave_ownership n))
              names
          in
          let symbol_table' = StringSet.add b.name symbol_table in
          (symbol_table', graph)
      | _ -> (symbol_table, graph)
    in

    check_children pipe.sname StringSet.empty graph_for_pipe
  in

  let _ =
    List.iter
      (fun p ->
        let _ = ownership_ck p in
        if verbose then
          print_string ("ownership check for " ^ p.sname ^ " passed!\n"))
      pipes
  in

  ()
