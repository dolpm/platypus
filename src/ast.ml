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

type unary_operator = Neg | Not

type defined_type =
  | Int
  | Float
  | Bool
  | Tuple
  | Unit
  | Char
  | String of defined_type
  | List of defined_type
  | Vector of defined_type
  | Fluid of defined_type
  | Ref of defined_type
  | Sig of defined_type list * defined_type
  | Thing of (defined_type * string) list
  | Box of defined_type
  | Option of defined_type

type fn_sig = defined_type list * defined_type
type type_binding = defined_type * string

type expr =
  | IntLiteral of int
  | FloatLiteral of float (* should this be string? *)
  | BoolLiteral of bool
  | TupleLiteral of defined_type list * expr list
  | CharLiteral of char
  | UnitLiteral of unit
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
  (* range start, range end, var name, range step, statement *)
  | Loop of expr * expr * expr * expr * stmt
  | While of expr * stmt

type pipe_declaration = {
  name : string;
  (* lifetime delcarations, just a list of lifetimes for now *)
  (* will probably need lifetime comparisons as well, or they *)
  (* could also enforce sorting from large to small (left to right) *)
  lifetimes : string list;
  formals : type_binding list;
  locals : type_binding list; (* ask richard *)
  return_type : defined_type;
  body : stmt list;
}

type program = type_binding list * pipe_declaration list
