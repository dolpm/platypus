type defined_type =
  | Int
  | Float
  | Bool
  | Tuple of defined_type list
  | Unit
  | Char
  | String
  | Str
  | Vector of defined_type
  (* thing names are user-defined *)
  (* | Thing of string *)
  | Box of defined_type
  (* type, lifetime of type --> todo maybe add more lifetime metadata *)
  | Borrow of defined_type * string
  | MutBorrow of defined_type * string
  (* name, children names --> children types *)
  (* | Thing of string * (string * defined_type) list *)
  | Ident of string
  | Generic
  (* todo: remove... but currently used in semant *)
  | Option of defined_type

type binary_operator =
  | Add
  | Sub
  | Mult
  | Div
  | Equal
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq
  | And
  | Or
  | Concat

type unary_operator = Neg | Not | Deref | Ref | MutRef
type fn_sig = defined_type list * defined_type

(* is_fluid (mutable), type, name bound *)
type type_binding = bool * defined_type * string

type expr =
  | IntLiteral of int
  | FloatLiteral of string
  | BoolLiteral of bool
  | CharLiteral of char
  | UnitLiteral
  | StringLiteral of string
  (* assignable thing value *)
  | ThingValue of (string * expr) list
  | TupleValue of expr list
  | TupleIndex of string * int
  | Ident of string
  | Binop of expr * binary_operator * expr
  | Unop of unary_operator * expr
  (* function call, takes in fn name and a list of inputs *)
  | PipeIn of string * expr list
  | NoExpr

type stmt =
  | Block of stmt list
  | Expr of expr
  (* return equivalent *)
  | PipeOut of expr
  (* expression resolving to boolean, if true, if false *)
  | If of expr * stmt * stmt
  (* range start, range end, var name, range step if provided, statement *)
  | Loop of expr * expr * string * expr * stmt
  | While of expr * stmt
  (* is_mut ... *)
  | Assign of bool * defined_type * string * expr
  | ReAssign of string * expr

type thing_declaration = { tname : string; elements : type_binding list }

type pipe_declaration = {
  name : string;
  (* lifetime delcarations, just a list of lifetimes for now *)
  (* will probably need lifetime comparisons as well, or they *)
  (* could also enforce sorting from large to small (left to right) *)
  lifetimes : string list;
  formals : type_binding list;
  (* locals : type_binding list; *)
  (* ask richard about why locals need to be here? *)
  return_type : defined_type;
  body : stmt list;
}

(* type program = defined_type list * pipe_declaration list *)
type program = thing_declaration list * pipe_declaration list

(* Pretty-printing functions *)

let string_of_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Leq -> "<="
  | Gt -> ">"
  | Geq -> ">="
  | And -> "and"
  | Or -> "or"
  | Concat -> "^"

let string_of_uop = function
  | Neg -> "-"
  | Not -> "!"
  | Deref -> "@"
  | Ref -> "&"
  | MutRef -> "~&"

let rec string_of_typ = function
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Tuple tl -> "[" ^ String.concat " * " (List.map string_of_typ tl) ^ "]"
  | Str -> "str"
  | Unit -> "unit"
  | Char -> "char"
  | String -> "string"
  | Vector t -> "vector[" ^ string_of_typ t ^ "]"
  | Box t -> "box[" ^ string_of_typ t ^ "]"
  | Borrow (t, lt) -> "&" ^ lt ^ " " ^ string_of_typ t
  | MutBorrow (t, lt) -> "~&" ^ lt ^ " " ^ string_of_typ t
  (* do we want to print children here? *)
  (* | Thing (n, _) -> n *)
  | Ident v -> v
  | Generic -> "" (* maybe raise an exception here, or just handle in semant *)
  | Option _ -> ""

let rec string_of_expr = function
  | IntLiteral l -> string_of_int l
  | FloatLiteral l -> l
  | BoolLiteral true -> "true"
  | BoolLiteral false -> "false"
  | CharLiteral c -> "\x27" ^ String.make 1 c ^ "\x27"
  | UnitLiteral -> "()"
  | StringLiteral s -> "\x22" ^ s ^ "\x22"
  | ThingValue children ->
      "{\n"
      ^ String.concat ",\n"
          (List.map
             (fun (c_name, e) -> "    " ^ c_name ^ ": " ^ string_of_expr e)
             children)
      ^ "\n  }"
  | TupleValue es ->
      "tuple("
      ^ String.concat ", " (List.map (fun e -> "" ^ string_of_expr e) es)
      ^ ")"
  | TupleIndex (name, i) -> name ^ "." ^ string_of_int i
  | Ident s -> s
  | Binop (e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop (o, e) -> string_of_uop o ^ string_of_expr e
  | PipeIn (f, el) ->
      f ^ " <| [" ^ String.concat ", " (List.map string_of_expr el) ^ "]"
  | NoExpr -> ""

let rec indent x =
  let s = "  " in
  match x with 0 -> "" | _ -> s ^ indent (x - 1)

let rec string_of_stmt stmt pad =
  match stmt with
  | Block stmts ->
      indent (pad - 1)
      ^ "{\n"
      ^ String.concat "" (List.map (fun s -> string_of_stmt s pad) stmts)
      ^ indent (pad - 1)
      ^ "}\n"
  | Expr expr -> indent pad ^ string_of_expr expr ^ ";\n"
  | PipeOut expr -> indent pad ^ "|> " ^ string_of_expr expr ^ ";\n"
  | If (e, s, Block []) ->
      indent pad ^ "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s (pad + 1)
  | If (e, s1, s2) -> (
      indent pad ^ "if (" ^ string_of_expr e ^ ")\n"
      ^ string_of_stmt s1 (pad + 1)
      ^ indent pad ^ "else\n"
      ^
      match s2 with
      | If (_, _, _) -> string_of_stmt s2 pad
      | _ -> string_of_stmt s2 (pad + 1))
  | Loop (e1, e2, e3, e4, s) ->
      indent pad ^ "loop " ^ string_of_expr e1 ^ " -> " ^ string_of_expr e2
      ^ " as (" ^ e3 ^ "," ^ string_of_expr e4 ^ ")"
      ^ string_of_stmt s (pad + 1)
  | While (e, s) ->
      indent pad ^ "while " ^ string_of_expr e ^ "\n"
      ^ string_of_stmt s (pad + 1)
  | Assign (is_mut, t, v, e) ->
      indent pad
      ^ (if is_mut then "mut " else "")
      ^ string_of_typ t ^ " " ^ v ^ " <| " ^ string_of_expr e ^ ";\n"
  | ReAssign (v, e) -> indent pad ^ v ^ " <| " ^ string_of_expr e ^ ";\n"

let string_of_tdecl tdecl =
  "thing " ^ tdecl.tname ^ " <| {\n"
  ^ String.concat ",\n"
      (List.map
         (fun v ->
           match v with _, t, n -> indent 1 ^ n ^ ": " ^ string_of_typ t)
         tdecl.elements)
  ^ "\n}"
(* match t with
   | Thing (s, l) ->
       "thing " ^ s ^ " <| {\n"
       ^ String.concat ",\n"
           (List.map (fun (n, t) -> indent 1 ^ n ^ ": " ^ string_of_typ t) l)
       ^ "\n}"
   | _ -> "" *)

let string_of_pdecl pdecl =
  "pipe " ^ pdecl.name ^ " |> ["
  ^ String.concat ", " pdecl.lifetimes
  ^ "] |> ["
  ^ String.concat ", "
      (List.map
         (fun v -> match v with _, t, n -> n ^ ": " ^ string_of_typ t)
         pdecl.formals)
  ^ "] |> "
  ^ string_of_typ pdecl.return_type
  ^ " {\n"
  ^ String.concat "" (List.map (fun s -> string_of_stmt s 1) pdecl.body)
  ^ "}\n"

let string_of_program (things, funcs) =
  String.concat "\n" (List.map string_of_tdecl (List.rev things))
  ^ String.concat "\n" (List.map string_of_pdecl (List.rev funcs))
