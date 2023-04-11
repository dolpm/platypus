open Ast
open Sast
open Borrow
module StringMap = Map.Make (String)

let check (_things, pipes) verbosity =
  let cur_sblock_id = ref 0 in

  (* built in pipe definitions *)
  (* (name, [param(is_mut, type, name)], ret_type) *)
  let stdlib_pipe_decls =
    [
      ("Printnl", [ (false, Generic, "x") ], Unit);
      ("panic", [ (false, String, "x") ], Unit);
      ("int_to_string", [ (false, Int, "x") ], String);
      ("float_to_string", [ (false, Float, "x") ], String);
      ("char_to_string", [ (false, Char, "x") ], String);
      ("bool_to_string", [ (false, Bool, "x") ], String);
      ("Box_new", [ (true, Generic, "x") ], Box Generic);
      ("Vector_length", [ (false, Vector Generic, "x") ], Int);
      ("Vector_new", [], Vector Generic);
      ( "Vector_get_mut",
        [ (true, MutBorrow (Vector Generic, "'_"), "x"); (false, Int, "y") ],
        MutBorrow (Generic, "'_") );
      ( "Vector_get",
        [ (false, Borrow (Vector Generic, "'_"), "x"); (false, Int, "y") ],
        Borrow (Generic, "'_") );
      ( "Vector_push",
        [ (true, MutBorrow (Vector Generic, "'_"), "x"); (false, Generic, "y") ],
        Unit );
      ("Vector_pop", [ (true, MutBorrow (Vector Generic, "'_"), "x") ], Unit);
      ("String_new", [ (false, String, "x") ], String);
      ("String_concat", [ (false, String, "x"); (false, String, "y") ], String);
      ("option_is_none", [ (false, Option Generic, "x") ], Bool);
      ("option_is_some", [ (false, Option Generic, "x") ], Bool);
    ]
  in

  (* transform the built-in defn's into actual declaration types *)
  let built_in_pipe_decls =
    let add_bind map (name, args, ret_ty) =
      StringMap.add name
        {
          return_type = ret_ty;
          name;
          lifetimes = [];
          formals = args;
          body = [];
        }
        map
    in
    List.fold_left add_bind StringMap.empty stdlib_pipe_decls
  in

  (* add pipe to map if no collision, else raise *)
  let add_pipe map pdecl =
    let built_in_err =
      "pipe " ^ pdecl.name ^ " may not be defined - it's built-in"
    and dup_err = "duplicate pipe " ^ pdecl.name
    and make_err er = raise (Failure er)
    and n = pdecl.name in
    match pdecl with
    (* No duplicate functions or redefinitions of built-ins *)
    | _ when StringMap.mem n built_in_pipe_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ -> StringMap.add n pdecl map
  in

  (* add pipe declarations to a single symbol table *)
  (* checking for naming collisions *)
  let pipe_decls = List.fold_left add_pipe built_in_pipe_decls pipes in

  (* lookup pdecl *)
  let get_pipe s =
    try StringMap.find s pipe_decls
    with Not_found -> raise (Failure ("pipe \"" ^ s ^ "\" does not exist"))
  in

  (* make sure entrypoint is defined, also validate form *)
  let ep = get_pipe "main" in
  let ret_not_unit_err = "program entrypoint doesn't have a unit return type"
  and arg_lifetime_nonempty =
    "program entrypoint can't have arguments or lifetimes"
  and make_err er = raise (Failure er) in
  (* validate return type is unit, and parameters don't exist *)
  let _ =
    match ep.return_type with
    | Unit ->
        if List.length ep.formals > 0 || List.length ep.lifetimes > 0 then
          make_err arg_lifetime_nonempty
    | _ -> make_err ret_not_unit_err
  in

  (* TODO: if rhs is mutable reference, make sure lhs is mutable*)

  (* make sure there aren't any duplicate bindings in functions *)
  let check_bindings to_check =
    let name_compare (_, _, n1) (_, _, n2) = compare n1 n2 in
    let check_it checked binding =
      let _, _, n1 = binding in
      let dup_err = "duplicate binding: " ^ n1 in
      match checked with
      | (_, _, n2) :: _ when n1 = n2 -> raise (Failure dup_err)
      | _ -> binding :: checked
    in

    let _ = List.fold_left check_it [] (List.sort name_compare to_check) in
    to_check
  in

  let check_pipe p =
    let formals' = check_bindings p.formals in

    (* make sure lhs and rhs of assignments and re-assignments are of eq type *)
    let rec check_assign lvaluet rvaluet err =
      match (lvaluet, rvaluet) with
      | Vector Generic, Vector rt -> Vector rt
      | Vector lt, Vector Generic -> Vector lt
      | Box Generic, Box rt -> Box rt
      | Box lt, Box Generic -> Box lt
      | Generic, _ -> rvaluet
      | _, Generic -> lvaluet
      | MutBorrow (lt, l1), MutBorrow (rt, _l2) ->
          MutBorrow (check_assign lt rt err, l1)
      | Borrow (lt, l1), Borrow (rt, _l2) -> Borrow (check_assign lt rt err, l1)
      | _ -> if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    let symbols =
      List.fold_left
        (fun m (is_mut, typ, name) -> StringMap.add name (is_mut, typ) m)
        StringMap.empty formals'
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier (s : string)
        (symbols : (bool * defined_type) StringMap.t) : bool * defined_type =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Assure return statement exists *)
    let assert_return (slist : stmt list) =
      let rec check_stmt (have_seen_return : bool) (s : stmt) =
        match s with
        | PipeOut _ -> true
        | If (_, stmt1, stmt2) -> (
            match stmt2 with
            | Expr NoExpr -> have_seen_return
            | _ ->
                if
                  have_seen_return
                  || check_stmt have_seen_return stmt1
                     && check_stmt have_seen_return stmt2
                then true
                else make_err ("return value not provided in " ^ p.name))
        | Block sl ->
            List.fold_left check_stmt have_seen_return (List.rev sl)
            || have_seen_return
        | While (_, stmt) | Loop (_, _, _, _, stmt) ->
            check_stmt have_seen_return stmt
        | _ -> false
      in
      List.fold_left check_stmt false (List.rev slist)
    in

    let _ = assert_return p.body in

    (* Return a semantically-checked expression, i.e. with a type *)
    let rec expr e symbols =
      match e with
      | IntLiteral l -> (Int, SIntLiteral l)
      | FloatLiteral l -> (Float, SFloatLiteral l)
      | BoolLiteral l -> (Bool, SBoolLiteral l)
      | CharLiteral l -> (Char, SCharLiteral l)
      | UnitLiteral -> (Unit, SUnitLiteral)
      | StringLiteral l -> (String, SStringLiteral l)
      | Unop (op, e1) as e ->
          let t1, e1' = expr e1 symbols in
          let ty =
            match op with
            | Neg when t1 = Int || t1 = Float -> t1
            | Not when t1 = Bool -> t1
            | Ref -> (
                let _ =
                  match e1 with
                  | Ident _ -> ()
                  | _ -> make_err "borrows must be taken on identifiers"
                in
                match t1 with
                | MutBorrow _ | Borrow _ ->
                    make_err "can't take a borrow of a borrow"
                | _ -> Borrow (t1, "'_"))
            | MutRef -> (
                let _ =
                  match e1 with
                  | Ident _ -> ()
                  | _ -> make_err "borrows must be taken on identifiers"
                in
                match t1 with
                | MutBorrow _ | Borrow _ ->
                    make_err "can't take a borrow of a borrow"
                | _ -> MutBorrow (t1, "'_"))
            | Deref
              when match t1 with
                   | Borrow (_, _) | MutBorrow (_, _) -> true
                   | _ -> false -> (
                match t1 with
                | Borrow (t_inner, _) | MutBorrow (t_inner, _) -> t_inner
                | _ -> raise (Failure "panic!"))
            | _ ->
                raise
                  (Failure
                     ("illegal unary operator " ^ string_of_uop op ^ " "
                    ^ string_of_typ t1 ^ " in " ^ string_of_expr e))
          in
          (ty, SUnop (op, (t1, e1')))
      | Binop (e1, op, e2) as e ->
          let t1, e1' = expr e1 symbols and t2, e2' = expr e2 symbols in
          let same = t1 = t2 in
          let ty =
            match op with
            | (And | Or) when same && t1 = Bool -> Bool
            | (Lt | Leq | Gt | Geq) when same && (t1 = Int || t1 = Float) ->
                Bool
            | (Equal | Neq)
              when same
                   && (t1 = Int || t1 = Float || t1 = String || t1 = Char
                     || t1 = Bool) ->
                Bool
            | (Add | Sub | Mult | Div) when same && (t1 = Int || t1 = Float)
              -> (
                match (t1, t2) with
                | Int, Int -> Int
                | Float, Float -> Float
                | Float, Int -> Float
                | Int, Float -> Float
                | _ ->
                    raise
                      (Failure
                         ("invalid operands for arithmetic operation "
                        ^ string_of_op op)))
            | Concat when same && t1 = String && t2 = String -> String
            | _ ->
                raise
                  (Failure
                     ("illegal binary operator " ^ string_of_typ t1 ^ " "
                    ^ string_of_op op ^ " " ^ string_of_typ t2 ^ " in "
                    ^ string_of_expr e))
          in
          (ty, SBinop ((t1, e1'), op, (t2, e2')))
      | PipeIn (pname, args) as pipein ->
          let pd = get_pipe pname in
          let param_length = List.length pd.formals in
          if List.length args != param_length then
            raise
              (Failure
                 ("expecting " ^ string_of_int param_length ^ " arguments in "
                ^ string_of_expr pipein))
          else
            let args_checked = List.map (fun arg -> expr arg symbols) args in
            let check_pipein (_, ft, _) e =
              (* we will check lifetimes later - just make sure they are ambiguous *)
              (* for this step *)
              let ft =
                match ft with
                | Borrow (ty, _lt) -> Borrow (ty, "'_")
                | MutBorrow (ty, _lt) -> MutBorrow (ty, "'_")
                | f -> f
              in
              let et, e' = expr e symbols in
              let err =
                "illegal argument found " ^ string_of_typ et ^ " expected "
                ^ string_of_typ ft ^ " in " ^ string_of_expr e
              in
              (check_assign ft et err, e')
            in
            (* Make sure all generics are the same *)
            let solve_generic =
              let generic_type =
                List.fold_left2
                  (fun accum_t (_, ft, _) (et, _) ->
                    match ft with
                    | Generic -> (
                        match accum_t with
                        | Generic -> et
                        | _ ->
                            raise
                              (Failure
                                 "generic conflict-- two generics map to \
                                  different types"))
                    | _ -> accum_t)
                  Generic pd.formals args_checked
              in
              let rec generic_sub_helper ft =
                match ft with
                | Generic -> generic_type
                | Vector Generic -> Vector generic_type
                | MutBorrow (t, l) -> MutBorrow (generic_sub_helper t, l)
                | Borrow (t, l) -> Borrow (generic_sub_helper t, l)
                | typ -> typ
              in
              let generic_substitution =
                List.map
                  (fun (a, ft, c) -> (a, generic_sub_helper ft, c))
                  pd.formals
              in
              List.map2 check_pipein generic_substitution args
            in
            let args' = solve_generic in
            let first_arg_type =
              if List.length args' > 0 then fst (List.hd args') else Generic
            in
            let ret_type =
              match pname with
              | "Printnl" -> (
                  match first_arg_type with
                  | Borrow (t, _) | MutBorrow (t, _) -> (
                      match t with
                      | Int | Bool | Float | String -> Unit
                      | _ -> raise (Failure ("unexpected arg type in " ^ pname))
                      )
                  | Int | Bool | Float | String -> Unit
                  | _ -> raise (Failure ("unexpected arg type in " ^ pname)))
              | "Box_new" -> Box first_arg_type
              | "Vector_pop" -> (
                  match first_arg_type with
                  | MutBorrow (Vector t, _) -> t
                  | _ -> raise (Failure ("unexpected arg type in " ^ pname)))
              | "Vector_get" -> (
                  match first_arg_type with
                  | Borrow (Vector t, lt) -> Borrow (t, lt)
                  | _ -> raise (Failure ("unexpected arg type in " ^ pname)))
              | "Vector_get_mut" -> (
                  match first_arg_type with
                  | MutBorrow (Vector t, lt) -> MutBorrow (t, lt)
                  | _ -> raise (Failure ("unexpected arg type in " ^ pname)))
              | "Vector_push" -> Unit
              | _ -> pd.return_type
            in
            let ret_type =
              match ret_type with
              | Borrow (ty, _) -> Borrow (ty, "'_")
              | MutBorrow (ty, _) -> MutBorrow (ty, "'_")
              | _ -> ret_type
            in
            (ret_type, SPipeIn (pname, args'))
      | Ident s -> (snd (type_of_identifier s symbols), SIdent s)
      | _ -> (Unit, SNoexpr)
    in

    (* illegal to do references of references *)
    (* illegal to dereference a @& or @~& *)
    (* handle either syntactically or semantically *)

    (* Return a semantically-checked statement, i.e. containing s_exprs *)
    let rec check_stmt s symbols =
      match s with
      | Expr e -> SExpr (expr e symbols)
      | Block sl ->
          SBlock
            ( List.rev
                (snd
                   (List.fold_left
                      (fun (symbols, sstmts) stmt ->
                        let symbols' =
                          match stmt with
                          | Assign (is_mut, t, name, _) ->
                              if StringMap.mem name symbols then
                                make_err
                                  ("variable " ^ name ^ " already in scope.")
                              else StringMap.add name (is_mut, t) symbols
                          | _ -> symbols
                        in
                        (symbols', check_stmt stmt symbols' :: sstmts))
                      (symbols, []) sl)),
              let _ = cur_sblock_id := !cur_sblock_id + 1 in
              string_of_int (!cur_sblock_id - 1) )
      | Assign (is_mut, t, name, e) as ass ->
          (* make sure we aren't assigning a deref to a variable (illegal) *)
          let _ =
            match e with
            | Unop (Deref, _) ->
                make_err "Can't bind the result of a de-reference."
            | _ -> ()
          in

          let is_mutborrow = match t with MutBorrow _ -> true | _ -> false in

          let _, lt = type_of_identifier name symbols
          and rt, e' = expr e symbols in
          let err =
            "illegal assignment " ^ string_of_typ lt ^ " = " ^ string_of_typ rt
            ^ " in " ^ string_of_stmt ass 0
          in
          let _ = check_assign lt rt err in
          SAssign (is_mut || is_mutborrow, t, name, (rt, e'))
      | ReAssign (name, e) as ass ->
          (* make sure we aren't reassigning a deref to a variable (illegal) *)
          let _ =
            match e with
            | Unop (Deref, _) ->
                make_err "Can't bind the result of a de-reference."
            | _ -> ()
          in

          (*
             make sure the left hand side is either a
             1. mut ident
             2. mut borrow of an ident
          *)
          let is_mut, lt = type_of_identifier name symbols in

          let lt, is_mutborrow =
            match (is_mut, lt) with
            | _, MutBorrow (t, _) -> (t, true)
            | true, t -> (t, false)
            | _ ->
                make_err
                  "lhs of a reassignment must be a mutable borrow or a mutable \
                   identifier"
          in

          let rt, e' = expr e symbols in
          let err =
            "illegal reassignment " ^ string_of_typ lt ^ " = "
            ^ string_of_typ rt ^ " in " ^ string_of_stmt ass 0
          in
          let _ = check_assign rt lt err in
          SReAssign (is_mutborrow, name, (rt, e'))
      | Loop (e1, e2, n, e3, s) -> (
          if StringMap.mem n symbols then
            raise (Failure ("identifier: " ^ n ^ " already defined."))
          else
            (* add the iterator value to symbols (mutable for now) *)
            let symbols = StringMap.add n (true, Int) symbols in
            let exprs =
              List.map
                (fun e ->
                  let t, e' = expr e symbols in
                  let err = "expected integer " ^ string_of_expr e in
                  let t' = check_assign t Int err in
                  (t', e'))
                [ e1; e2; e3 ]
            in
            match exprs with
            | [ (t1', e1'); (t2', e2'); (t3', e3') ] ->
                (* todo: make sure n isn't in symbol table? *)
                SLoop
                  ((t1', e1'), (t2', e2'), n, (t3', e3'), check_stmt s symbols)
            | _ -> make_err "panic!")
      | While (e, s) ->
          let t, e' = expr e symbols in
          let err = "expected boolean " ^ string_of_expr e in
          let t' = check_assign t Bool err in
          SWhile ((t', e'), check_stmt s symbols)
      | PipeOut e -> SPipeOut (expr e symbols)
      | If (e, stmt1, stmt2) ->
          let t, e' = expr e symbols in
          let err = "expected boolean " ^ string_of_expr e in
          let t' = check_assign t Bool err in
          SIf ((t', e'), check_stmt stmt1 symbols, check_stmt stmt2 symbols)
    in
    let sbody =
      if StringMap.mem p.name built_in_pipe_decls then []
      else
        match check_stmt (Block p.body) symbols with
        | SBlock (sl, _sblock_id) -> sl
        | _ ->
            let err = "internal error: block didn't become a block?" in
            raise (Failure err)
    in

    {
      sreturn_type = p.return_type;
      sname = p.name;
      slifetimes = p.lifetimes;
      sformals = formals';
      sbody;
    }
  in

  let s_pipes_with_builtins =
    List.map check_pipe (List.map snd (StringMap.bindings pipe_decls))
  in

  (* For the borrow checker, will not be included in final SAST *)

  (* boolean denotes verbosity - set to true if you want to *)
  (* see generated graph nodes and fn tests *)
  let node_ownership_map = borrow_ck s_pipes_with_builtins verbosity in

  (* remove built-in pdecls before returing *)
  let s_pipes =
    List.filter
      (fun p_decl -> not (StringMap.mem p_decl.sname built_in_pipe_decls))
      s_pipes_with_builtins
  in

  (* print block ownership map *)
  let _ =
    if verbosity then
      let _ = print_string "\nownership:\n" in
      let _ =
        List.iter
          (fun (k, v) ->
            print_string ("block " ^ k ^ " --> " ^ String.concat "," v ^ "\n"))
          (StringMap.bindings node_ownership_map)
      in
      print_string "\n"
  in

  let s_pipes_w_wrappers =
    List.map
      (fun p ->
        {
          sreturn_type = p.sreturn_type;
          sname = p.sname;
          slifetimes = p.slifetimes;
          sformals = p.sformals;
          (*
            We wrap each function body with two blocks, the outermost one owns
            the function args and the second one contains the function body.
            We do this so that our SAST function structure corresponds to the dummy-lifetime structure 
            for functions that we create in the borrow-checker. This will allow us to free owned 
            arguments appropriately in codegen.
          *)
          sbody =
            [
              SBlock
                ( [ SBlock (p.sbody, p.sname ^ "_wrapper") ],
                  p.sname ^ "_with_args" );
            ];
        })
      s_pipes
  in

  (([], s_pipes_w_wrappers), node_ownership_map)
