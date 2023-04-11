open Ast

type s_expr = defined_type * sx

and sx =
  | SIntLiteral of int
  | SFloatLiteral of string
  | SBoolLiteral of bool
  | SCharLiteral of char
  | SUnitLiteral
  | SStringLiteral of string
  (* assignable thing value *)
  | SThingValue of string * (string * s_expr) list
  | SThingAccess of string * string * string list
  | STupleValue of s_expr list
  | STupleIndex of string * int
  | SIdent of string
  | SBinop of s_expr * binary_operator * s_expr
  | SUnop of unary_operator * s_expr
  (* function call, takes in fn name and a list of inputs *)
  | SPipeIn of string * s_expr list
  | SNoexpr

type s_stmt =
  (* string list contains symbols owned at end of life *)
  | SBlock of
      s_stmt list
      * string (* string is id to map it to borrow checker graph node *)
  | SExpr of s_expr
  (* return equivalent *)
  | SPipeOut of s_expr
  (* s_expression resolving to boolean, if true, if false *)
  | SIf of s_expr * s_stmt * s_stmt
  (* range start, range end, var name, range step if provided, statement *)
  | SLoop of s_expr * s_expr * string * s_expr * s_stmt
  | SWhile of s_expr * s_stmt
  (* is_mut ... *)
  | SAssign of bool * defined_type * string * s_expr
  (* is_mutborrow *)
  | SReAssign of bool * string * s_expr

type s_thing_declaration = { stname : string; selements : type_binding list }

type s_pipe_declaration = {
  sname : string;
  (* lifetime delcarations, just a list of lifetimes for now *)
  (* will probably need lifetime comparisons as well, or they *)
  (* could also enforce sorting from large to small (left to right) *)
  slifetimes : string list;
  sformals : type_binding list;
  (* locals : type_binding list; *)
  (* ask richard about why locals need to be here? *)
  sreturn_type : defined_type;
  sbody : s_stmt list;
}

type s_program = s_thing_declaration list * s_pipe_declaration list

(* Pretty-printing functions *)

let rec string_of_s_expr (t, e) =
  "(" ^ string_of_typ t ^ " : "
  ^ (match e with
    | SIntLiteral l -> string_of_int l
    | SFloatLiteral l -> l
    | SBoolLiteral true -> "true"
    | SBoolLiteral false -> "false"
    | SCharLiteral c -> "\x27" ^ String.make 1 c ^ "\x27"
    | SUnitLiteral -> "()"
    | SStringLiteral s -> "\x22" ^ s ^ "\x22"
    | SThingValue (name, children) ->
        name ^ " {\n"
        ^ String.concat ",\n"
            (List.map
               (fun (c_name, e) -> "    " ^ c_name ^ ": " ^ string_of_s_expr e)
               children)
        ^ "\n  }"
    | SThingAccess (_t_name, v_name, access_list) ->
        v_name ^ "." ^ String.concat "." access_list
    | STupleValue es ->
        "tuple("
        ^ String.concat ", " (List.map (fun e -> "" ^ string_of_s_expr e) es)
        ^ ")"
    | STupleIndex (name, i) -> name ^ "." ^ string_of_int i
    | SIdent s -> s
    | SBinop (e1, o, e2) ->
        string_of_s_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_s_expr e2
    | SUnop (o, e) -> string_of_uop o ^ string_of_s_expr e
    | SPipeIn (f, el) ->
        f ^ " <| [" ^ String.concat ", " (List.map string_of_s_expr el) ^ "]"
    | SNoexpr -> "")
  ^ ")"

let rec indent x =
  let s = "  " in
  match x with 0 -> "" | _ -> s ^ indent (x - 1)

let rec string_of_s_stmt s_stmt pad =
  match s_stmt with
  | SBlock (s_stmts, sblock_id) ->
      indent (pad - 1)
      ^ "{(id: " ^ sblock_id ^ ")\n"
      ^ String.concat "" (List.map (fun s -> string_of_s_stmt s pad) s_stmts)
      ^ indent (pad - 1)
      ^ "}\n"
  | SExpr s_expr -> indent pad ^ string_of_s_expr s_expr ^ ";\n"
  | SPipeOut s_expr -> indent pad ^ "|> " ^ string_of_s_expr s_expr ^ ";\n"
  | SIf (e, s, SBlock ([], _sblock_id)) ->
      indent pad ^ "if (" ^ string_of_s_expr e ^ ")\n"
      ^ string_of_s_stmt s (pad + 1)
  | SIf (e, s1, s2) -> (
      indent pad ^ "if (" ^ string_of_s_expr e ^ ")\n"
      ^ string_of_s_stmt s1 (pad + 1)
      ^ indent pad ^ "else\n"
      ^
      match s2 with
      | SIf (_, _, _) -> string_of_s_stmt s2 pad
      | _ -> string_of_s_stmt s2 (pad + 1))
  | SLoop (e1, e2, e3, e4, s) ->
      indent pad ^ "loop " ^ string_of_s_expr e1 ^ " -> " ^ string_of_s_expr e2
      ^ " as (" ^ e3 ^ "," ^ string_of_s_expr e4 ^ ")"
      ^ string_of_s_stmt s (pad + 1)
  | SWhile (e, s) ->
      indent pad ^ "while " ^ string_of_s_expr e ^ "\n"
      ^ string_of_s_stmt s (pad + 1)
  | SAssign (is_mut, t, v, e) ->
      indent pad
      ^ (if is_mut then "mut " else "")
      ^ string_of_typ t ^ " " ^ v ^ " <| " ^ string_of_s_expr e ^ ";\n"
  | SReAssign (_is_mutborrow, v, e) ->
      indent pad ^ v ^ " <| " ^ string_of_s_expr e ^ ";\n"

(* let string_of_tdecl t =
   match t with
   | Thing (s, l) ->
       "thing " ^ s ^ " <| {\n"
       ^ String.concat ",\n"
           (List.map (fun (n, t) -> indent 1 ^ n ^ ": " ^ string_of_typ t) l)
       ^ "\n}"
   | _ -> "" *)

let string_of_tdecl tdecl =
  "thing " ^ tdecl.stname ^ " <| {\n"
  ^ String.concat ",\n"
      (List.map
         (fun v -> match v with _, t, n -> n ^ ": " ^ string_of_typ t)
         tdecl.selements)
  ^ "\n}"

let string_of_spdecl pdecl =
  "pipe " ^ pdecl.sname ^ " |> ["
  ^ String.concat ", " pdecl.slifetimes
  ^ "] |> ["
  ^ String.concat ", "
      (List.map
         (fun v -> match v with _, t, n -> n ^ ": " ^ string_of_typ t)
         pdecl.sformals)
  ^ "] |> "
  ^ string_of_typ pdecl.sreturn_type
  ^ " {\n"
  ^ String.concat "" (List.map (fun s -> string_of_s_stmt s 1) pdecl.sbody)
  ^ "}\n"

let string_of_sprogram (things, funcs) =
  String.concat "\n" (List.map string_of_tdecl (List.rev things))
  ^ String.concat "\n" (List.map string_of_spdecl (List.rev funcs))
