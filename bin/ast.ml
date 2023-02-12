type defined_type =
  | Int
  | Float
  | Bool
  | Tuple
  | Unit
  | Char
  | String
  | Vector of defined_type
  (* thing names are user-defined *)
  (* | Thing of string *)
  | Box of defined_type
  | Option of defined_type
  | Ref of defined_type
  | Fluid of defined_type
  (* name, children names --> children types *)
  | Thing of string * (string * defined_type) list

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

type unary_operator = Neg | Not | Deref | Ref
type fn_sig = defined_type list * defined_type

(* is_fluid (mutable), type, name bound *)
type type_binding = bool * defined_type * string

type expr =
  | IntLiteral of int
  | FloatLiteral of string
  | BoolLiteral of bool
  (*
  | TupleLiteral of defined_type list * expr list
  | ThingLiteral of defined_type list * expr list
  *)
  | CharLiteral of char
  | UnitLiteral
  | StringLiteral of
      string (* just added this - I think maybe it'll be nice to have *)
  | Ident of string
  | Binop of expr * binary_operator * expr
  | Unop of unary_operator * expr
  | Assign of string * expr
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
  | Loop of expr * expr * expr * expr * stmt
  | While of expr * stmt

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

type program = defined_type list * pipe_declaration list

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
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
  | Neg -> "-"
  | Not -> "!"
  | Deref -> "@"
  | Ref -> "&"

let rec string_of_expr = function
  | IntLiteral l -> string_of_int l
  | FloatLiteral l -> l
  | BoolLiteral true -> "true"
  | BoolLiteral false -> "false"
  | CharLiteral c -> String.make 1 c
  | UnitLiteral -> "()"
  | StringLiteral s -> s
  | Ident s -> s
  | Binop (e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop (o, e) -> string_of_uop o ^ string_of_expr e
  | Assign (v, e) -> v ^ " <| " ^ string_of_expr e
  | PipeIn (f, el) ->
      f ^ " <| [" ^ String.concat ", " (List.map string_of_expr el) ^ "]"
  | NoExpr -> ""

let rec string_of_stmt = function
  | Block stmts ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr expr -> string_of_expr expr ^ ";\n"
  | PipeOut expr -> "|> " ^ string_of_expr expr ^ ";\n"
  | If (e, s, Block []) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If (e, s1, s2) ->
      "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^ "else\n"
      ^ string_of_stmt s2
  | Loop (e1, e2, e3, e4, s) ->
      (* todo figure out how to match both types (e.g., when step is omitted) *)
      "loop " ^ string_of_expr e1 ^ ".." ^ string_of_expr e2 ^ " as ("
      ^ string_of_expr e3 ^ "," ^ string_of_expr e4 ^ ")" ^ string_of_stmt s
  | While (e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let rec string_of_typ = function
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Tuple -> "tuple"
  | Unit -> "unit"
  | Char -> "char"
  | String -> "string"
  | Vector t -> "vector[" ^ string_of_typ t ^ "]"
  | Box t -> "box[" ^ string_of_typ t ^ "]"
  | Option t -> "option[" ^ string_of_typ t ^ "]"
  | Ref t -> "&[" ^ string_of_typ t ^ "]"
  | Fluid t -> "~[" ^ string_of_typ t ^ "]"
  (* do we want to print children here? *)
  | Thing (n, _) -> n

let string_of_tdecl t =
  match t with
  | Thing (s, l) ->
      "thing " ^ s ^ " |> {\n"
      ^ String.concat ",\n"
          (List.map (fun (n, t) -> n ^ ": " ^ string_of_typ t) l)
      ^ "}\n"
  | _ -> ""

let string_of_pdecl pdecl =
  "pipe " ^ pdecl.name ^ " |> ["
  ^ String.concat ", " pdecl.lifetimes
  ^ "] |> ["
  ^ String.concat ", "
      (List.map (fun v -> match v with _, _, v -> v) pdecl.formals)
  ^ "] |> "
  ^ string_of_typ pdecl.return_type
  ^ " {\n"
  ^ String.concat "" (List.map string_of_stmt pdecl.body)
  ^ "}\n"

let string_of_program (things, funcs) =
  String.concat "" (List.map string_of_tdecl things)
  ^ "\n"
  ^ String.concat "\n" (List.map string_of_pdecl funcs)
