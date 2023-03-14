open Sast
open Ast
module StringMap = Map.Make (String)

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
      children : string list;
      node_id : string;
      (* rest *)
      is_mut : bool;
      typ : defined_type;
      name : string;
      expr : s_expr;
    }
  | Rebinding of {
      parent : string option;
      children : string list;
      node_id : string;
      (* rest *)
      name : string;
      expr : s_expr;
    }
  | PipeCall of {
      parent : string option;
      children : string list;
      node_id : string;
      (* rest *)
      pipe_name : string;
      args : s_expr list;
    }
  | PipeReturn of {
      parent : string option;
      children : string list;
      node_id : string;
      (* rest *)
      returned : s_expr;
    }

let borrow_ck pipes verbose =
  if verbose then print_string "generating graph\n"
  else
    let generate_graph_for_pipe pipe =
      let pipe_args_as_nodes =
        snd
          (List.fold_left
             (fun (num_child, graph) (is_mut, typ, name) ->
               let child_id = pipe.sname ^ "." ^ string_of_int num_child in
               ( num_child + 1,
                 StringMap.add child_id
                   (Binding
                      {
                        parent = Some pipe.sname;
                        children = [];
                        node_id = child_id;
                        is_mut;
                        typ;
                        name;
                        expr = (Unit, SNoexpr);
                      })
                   graph ))
             (0, StringMap.empty) pipe.sformals)
      in
      pipe_args_as_nodes
    in
    let _ltg =
      List.fold_left
        (fun graph pipe ->
          StringMap.add pipe.sname (generate_graph_for_pipe pipe) graph)
        StringMap.empty pipes
    in
    if verbose then print_string "generated graph\n" else ()
