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
      expr : expr;
    }
  | Rebinding of {
      parent : string option;
      children : string list;
      node_id : string;
      (* rest *)
      name : string;
      expr : expr;
    }
  | PipeCall of {
      parent : string option;
      children : string list;
      node_id : string;
      (* rest *)
      pipe_name : string;
      args : expr list;
    }
  | PipeReturn of {
      parent : string option;
      children : string list;
      node_id : string;
      (* rest *)
      returned : expr;
    }

let borrow_ck pipes verbose = ()
