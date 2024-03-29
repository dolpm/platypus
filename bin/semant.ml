(* Dylan M. | Ronit S. | Tony H. | Abe P. *)
open Ast
open Sast
open Borrow
module StringMap = Map.Make (String)

let check (things, pipes) verbosity =
  let cur_sblock_id = ref 0 in

  (* built in pipe definitions *)
  (* (name, lifetime, [param(is_mut, type, name)], body, ret_type) *)
  let stdlib_pipe_decls =
    [
      (* print *)
      ("Print", [], [ (false, Generic, "x") ], [ PipeOut NoExpr ], Unit);
      ("Printnl", [], [ (false, Generic, "x") ], [ PipeOut NoExpr ], Unit);
      (* rng *)
      ("Rng_init", [], [ (false, Int, "x") ], [ PipeOut NoExpr ], Unit);
      ( "Rng_generate",
        [],
        [ (false, Int, "x"); (false, Int, "y") ],
        [ PipeOut NoExpr ],
        Int );
      (* panic *)
      ("Panic", [], [ (false, Generic, "x") ], [ PipeOut NoExpr ], Unit);
      (* boxes *)
      ("Box_new", [], [ (true, Generic, "x") ], [ PipeOut NoExpr ], Box Generic);
      ( "Box_unbox",
        [ "'a" ],
        [ (false, Borrow (Box Generic, "'a"), "x") ],
        [ PipeOut NoExpr ],
        Borrow (Generic, "'a") );
      ( "Box_unbox_mut",
        [ "'a" ],
        [ (true, MutBorrow (Box Generic, "'a"), "x") ],
        [ PipeOut (Ident "x") ],
        MutBorrow (Generic, "'a") );
      (* vec *)
      ("Vector_length", [], [ (false, Generic, "x") ], [ PipeOut NoExpr ], Int);
      ("Vector_new", [], [], [ PipeOut NoExpr ], Vector Generic);
      ( "Vector_get_mut",
        [ "'a" ],
        [ (true, MutBorrow (Vector Generic, "'a"), "x"); (false, Int, "y") ],
        [ PipeOut (Ident "x") ],
        MutBorrow (Generic, "'a") );
      ( "Vector_get",
        [ "'a" ],
        [ (false, Borrow (Vector Generic, "'a"), "x"); (false, Int, "y") ],
        [ PipeOut NoExpr ],
        Borrow (Generic, "'a") );
      ( "Vector_update",
        [],
        [
          (true, MutBorrow (Vector Generic, "'_"), "x");
          (false, Int, "z");
          (false, Generic, "y");
        ],
        [ PipeOut NoExpr ],
        Unit );
      ( "Vector_push",
        [],
        [ (true, MutBorrow (Vector Generic, "'_"), "x"); (false, Generic, "y") ],
        [ PipeOut NoExpr ],
        Unit );
      ( "Vector_pop",
        [],
        [ (true, MutBorrow (Vector Generic, "'_"), "x") ],
        [ PipeOut NoExpr ],
        Unit );
      (* str *)
      ("Str_new", [], [ (false, String, "x") ], [ PipeOut NoExpr ], Str);
      ( "Str_push",
        [],
        [ (true, MutBorrow (Str, "'_"), "x"); (false, Char, "y") ],
        [ PipeOut NoExpr ],
        Unit );
      (* type casting *)
      ("Int", [], [ (false, Generic, "x") ], [ PipeOut NoExpr ], Int);
      ("Float", [], [ (false, Generic, "x") ], [ PipeOut NoExpr ], Float);
      ("Str", [], [ (false, Generic, "x") ], [ PipeOut NoExpr ], Str);
    ]
  in

  (* transform the built-in defn's into actual declaration types *)
  let built_in_pipe_decls =
    let add_bind l (name, lts, args, body, ret_ty) =
      l
      @ [
          { return_type = ret_ty; name; lifetimes = lts; formals = args; body };
        ]
    in
    List.fold_left add_bind [] stdlib_pipe_decls
  in

  (* add pipe to map if no collision, else raise *)
  let add_pipe list pdecl =
    let built_in_err =
      "pipe " ^ pdecl.name ^ " may not be defined - it's built-in"
    and dup_err = "duplicate pipe " ^ pdecl.name
    and make_err er = raise (Failure er)
    and n = pdecl.name in
    match pdecl with
    (* No duplicate functions or redefinitions of built-ins *)
    | _
      when match List.find_opt (fun p -> p.name = n) list with
           | Some _ -> true
           | None -> false ->
        make_err built_in_err
    | _
      when match List.find_opt (fun p -> p.name = n) list with
           | Some _ -> true
           | None -> false ->
        make_err dup_err
    | _ -> list @ [ pdecl ]
  in

  (* add pipe declarations to a single symbol table *)
  (* checking for naming collisions *)
  (* make sure user-defined ones go in top-down *)
  let pipe_decls =
    List.fold_left add_pipe built_in_pipe_decls (List.rev pipes)
  in

  (* lookup pdecl *)
  let get_pipe s =
    try List.find (fun p -> p.name = s) pipe_decls
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

  let check_things ts =
    let dup_err t_name = "duplicate thing: " ^ t_name in
    let check_thing t =
      (* make sure no top level borrows *)
      let _ =
        List.iter
          (fun (_, typ, _) ->
            match typ with
            | Unit ->
              raise (Failure
                     ("Illegal type! Thing " ^ t.tname
                    ^ " contains a unit type. Thing delcarations can't \
                       contain units."))
            | Borrow _ | MutBorrow _ ->
                raise
                  (Failure
                     ("Illegal type! Thing " ^ t.tname
                    ^ " contains a top-level borrow. Composite types can't \
                       contain borrows."))
            | _ -> ())
          t.elements
      in
      if List.length (List.filter (fun t' -> t'.tname = t.tname) ts) > 1 then
        raise (Failure (dup_err t.tname))
      else { stname = t.tname; selements = check_bindings t.elements }
    in
    List.map check_thing ts
  in

  let check_pipe p =
    let formals' = check_bindings p.formals in

    (* make sure lhs and rhs of assignments and re-assignments are of eq type *)
    let rec check_assign lvaluet rvaluet err =
      match (lvaluet, rvaluet) with
       Vector Unit, _
      | Box Unit, _
      | _, Vector Unit
      | _, Box Unit ->
          raise (Failure "Illegal type. Composite types can't contain units.")
      (* make sure can't put borrows in boxes/vecs *)
      | Vector (MutBorrow _), _
      | Vector (Borrow _), _
      | Box (MutBorrow _), _
      | Box (Borrow _), _
      | _, Vector (MutBorrow _)
      | _, Vector (Borrow _)
      | _, Box (MutBorrow _)
      | _, Box (Borrow _) ->
          raise (Failure "Illegal type. Composite types can't contain borrows.")
      | Vector Generic, Vector rt -> Vector rt
      | Vector lt, Vector Generic -> Vector lt
      | Box Generic, Box rt -> Box rt
      | Box lt, Box Generic -> Box lt
      | Generic, _ -> rvaluet
      | _, Generic -> lvaluet
      | MutBorrow (lt, l1), MutBorrow (rt, _l2) ->
          MutBorrow (check_assign lt rt err, l1)
      | Borrow (lt, l1), Borrow (rt, _l2) -> Borrow (check_assign lt rt err, l1)
      | Tuple tl1, Tuple tl2 ->
          Tuple 
          (List.map2 
            (fun t1 t2 -> 
              match t1,t2 with
              | Unit,_ | _,Unit ->
                 raise (Failure "Illegal type. Tuples cannot contain units.")
              | _ -> check_assign t1 t2 err) 
          tl1 tl2)
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
      let rec check_stmt (have_seen_return : bool) (in_if_branch : bool)
          (s : stmt) =
        let ret_val =
          match s with
          | PipeOut _ -> true
          | If (_, stmt1, stmt2) -> (
              match stmt2 with
              | Block [] ->
                  let _ = check_stmt have_seen_return true stmt1 in
                  have_seen_return
              | _ ->
                  if
                    have_seen_return
                    || check_stmt have_seen_return true stmt1
                       && check_stmt have_seen_return true stmt2
                  then true
                  else make_err ("return value not provided in " ^ p.name))
          | Block sl ->
              List.fold_left
                (fun have_seen_return stmt ->
                  check_stmt have_seen_return in_if_branch stmt)
                have_seen_return (List.rev sl)
              || have_seen_return
          | While (_, stmt) | Loop (_, _, _, _, stmt) ->
              check_stmt have_seen_return in_if_branch stmt
          | _ -> have_seen_return
        in
        ret_val
      in
      let _ =
        if
          not
            (List.fold_left
               (fun have_seen_return stmt ->
                 check_stmt have_seen_return false stmt)
               false (List.rev slist))
        then make_err ("return value not provided in " ^ p.name)
      in
      ()
    in

    let detect_unreachable_code (sl : stmt list) =
      let rec is_guaranteed_return (s : stmt) =
        match s with
        | PipeOut _ -> true
        | If (_e, stmt1, stmt2) ->
            is_guaranteed_return stmt1 && is_guaranteed_return stmt2
        | Block sl ->
            List.fold_left
              (fun found stmt ->
                if found then make_err "unreachable code! oh no :("
                else is_guaranteed_return stmt)
              false sl
        | Loop (_, _, _, _, stmt) | While (_, stmt) -> is_guaranteed_return stmt
        | _ -> false
      in

      is_guaranteed_return (Block sl)
    in

    let _ = assert_return p.body in
    let _ = detect_unreachable_code p.body in

    (* Return a semantically-checked expression, i.e. with a type *)
    let rec expr e symbols =
      match e with
      | IntLiteral l -> (Int, SIntLiteral l)
      | FloatLiteral l -> (Float, SFloatLiteral l)
      | BoolLiteral l -> (Bool, SBoolLiteral l)
      | CharLiteral l -> (Char, SCharLiteral l)
      | UnitLiteral -> (Unit, SUnitLiteral)
      | StringLiteral l -> (String, SStringLiteral l)
      | TupleIndex (e, idx) ->
          let typ, e' = expr e symbols in

          let ttyp =
            match typ with
            | MutBorrow (ttyp, _) | Borrow (ttyp, _) -> ttyp
            | _ -> raise (Failure "tuple indexing must be done on a borrow.")
          in

          let inner_typ =
            match ttyp with
            | Tuple inner_types -> List.nth inner_types idx
            | _ -> make_err "Tuple access must be done on a tuple."
          in

          let type_of_access =
            match typ with
            | MutBorrow _ -> MutBorrow (inner_typ, "'_")
            | Borrow _ -> Borrow (inner_typ, "'_")
            | _ -> raise (Failure "not possible!")
          in

          (type_of_access, STupleIndex ((typ, e'), idx))
      | ThingAccess (e, elem_to_access) ->
          let typ, e' = expr e symbols in

          let ttyp =
            match typ with
            | MutBorrow (ttyp, _) | Borrow (ttyp, _) -> ttyp
            | _ -> raise (Failure "thing access must be done on a borrow.")
          in

          let typ_of_access =
            let t_name =
              match ttyp with
              | Ident t_name -> t_name
              | _ ->
                  raise (Failure "thing access must be done on a thing type.")
            in
            (* get the actual thing to recurse on inner type *)
            let assoc_t = List.find (fun t -> t.tname = t_name) things in

            (* make sure inner element exists *)
            let _, typ', _ =
              List.find
                (fun (_is_mut, _typ, n) -> n = elem_to_access)
                assoc_t.elements
            in

            match typ with
            | MutBorrow _ -> MutBorrow (typ', "'_")
            | Borrow _ -> Borrow (typ', "'_")
            | _ -> raise (Failure "not possible!")
          in

          let t_to_be_accessed =
            match ttyp with
            | Ident t_name -> t_name
            | _ -> raise (Failure "thing access must be done on a thing type.")
          in

          ( typ_of_access,
            SThingAccess (t_to_be_accessed, (typ, e'), elem_to_access) )
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
            | Clone -> (
                (* Disallow clones of borrows and clones of clones *)
                let _ =
                  match e1 with
                  | Unop (Clone, _) -> make_err "can't clone on clone!"
                  | _ -> ()
                in
                match t1 with
                | MutBorrow _ | Borrow _ -> make_err "can't clone a borrow!"
                | _ -> t1)
            | _ ->
                make_err
                  ("illegal unary operator " ^ string_of_uop op ^ " "
                 ^ string_of_typ t1 ^ " in " ^ string_of_expr e)
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
                   && (t1 = Int || t1 = Float || t1 = String || t1 = Str
                     || t1 = Char || t1 = Bool) ->
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
            | Concat when same && t1 = Str && t2 = Str -> Str
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
              (* check for derefs in args (illegal) *)
              let _ =
                match e with
                | Unop (Deref, _) ->
                    make_err
                      "Can't provide the deref, which is an owned value, as a \
                       pipe argument. Please clone the value before passing \
                       it."
                | _ -> ()
              in

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
              | "Panic" | "Print" | "Printnl" -> (
                  match first_arg_type with
                  | Borrow (t, _) | MutBorrow (t, _) -> (
                      match t with
                      | Int | Bool | Float | String | Str -> Unit
                      | _ ->
                          raise
                            (Failure
                               ("unexpected arg type "
                               ^ string_of_typ first_arg_type
                               ^ " in " ^ pname)))
                  | Int | Bool | Float | String -> Unit
                  | _ ->
                      raise
                        (Failure
                           ("unexpected arg type "
                           ^ string_of_typ first_arg_type
                           ^ " in " ^ pname)))
              | "Box_new" -> Box first_arg_type
              | "Box_unbox" -> (
                  match first_arg_type with
                  | Borrow (Box t, lt) -> Borrow (t, lt)
                  | _ ->
                      raise
                        (Failure
                           ("unexpected arg type "
                           ^ string_of_typ first_arg_type
                           ^ " in " ^ pname)))
              | "Box_unbox_mut" -> (
                  match first_arg_type with
                  | MutBorrow (Box t, lt) -> MutBorrow (t, lt)
                  | _ ->
                      raise
                        (Failure
                           ("unexpected arg type "
                           ^ string_of_typ first_arg_type
                           ^ " in " ^ pname)))
              | "Vector_length" -> (
                  match first_arg_type with
                  | Borrow (Vector _t, _lt) -> Int
                  | _ ->
                      raise
                        (Failure
                           ("unexpected arg type "
                           ^ string_of_typ first_arg_type
                           ^ " in " ^ pname)))
              | "Vector_get" -> (
                  match first_arg_type with
                  | Borrow (Vector t, lt) -> Borrow (t, lt)
                  | _ ->
                      raise
                        (Failure
                           ("unexpected arg type "
                           ^ string_of_typ first_arg_type
                           ^ " in " ^ pname)))
              | "Vector_update" -> (
                  match first_arg_type with
                  | MutBorrow (Vector t, _lt) ->
                      let t3 = fst (List.nth args' 2) in
                      if t3 <> t then
                        raise
                          (Failure
                             ("unexpected arg type "
                             ^ string_of_typ first_arg_type
                             ^ " in " ^ pname))
                      else Unit
                  | _ ->
                      raise
                        (Failure
                           ("unexpected arg type "
                           ^ string_of_typ first_arg_type
                           ^ " in " ^ pname)))
              | "Vector_get_mut" -> (
                  match first_arg_type with
                  | MutBorrow (Vector t, lt) -> MutBorrow (t, lt)
                  | _ ->
                      raise
                        (Failure
                           ("unexpected arg type "
                           ^ string_of_typ first_arg_type
                           ^ " in " ^ pname)))
              | "Vector_pop" -> (
                  match first_arg_type with
                  | MutBorrow (Vector t, _) -> t
                  | _ ->
                      raise
                        (Failure
                           ("unexpected arg type "
                           ^ string_of_typ first_arg_type
                           ^ " in " ^ pname)))
              | "Vector_push" -> (
                  match first_arg_type with
                  | MutBorrow (Vector _t, _lt) -> Unit
                  | _ ->
                      raise
                        (Failure
                           ("unexpected arg type "
                           ^ string_of_typ first_arg_type
                           ^ " in " ^ pname)))
              | "Int" -> (
                  match first_arg_type with
                  | Int | Bool | Char | Float -> Int
                  | _ ->
                      raise
                        (Failure
                           ("unexpected arg type "
                           ^ string_of_typ first_arg_type
                           ^ " in " ^ pname)))
              | "Float" -> (
                  match first_arg_type with
                  | Int | Bool | Char | Float -> Float
                  | _ ->
                      raise
                        (Failure
                           ("unexpected arg type "
                           ^ string_of_typ first_arg_type
                           ^ " in " ^ pname)))
              | "Str" -> (
                  match first_arg_type with
                  | Int | Bool | Char | Float -> Str
                  | _ ->
                      raise
                        (Failure
                           ("unexpected arg type "
                           ^ string_of_typ first_arg_type
                           ^ " in " ^ pname)))
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
      | TupleValue exprs ->
          let values = List.map (fun e -> expr e symbols) exprs in
          let _ =
            List.iter
              (fun (t, _) ->
                match t with
                | Borrow _ | MutBorrow _ ->
                    raise
                      (Failure
                         "Illegal type! Tuple contains a top-level borrow. \
                          Composite types can't contain borrows.")
                | _ -> ())
              values
          in
          (Tuple (List.map fst values), STupleValue values)
      | ThingValue (t_name, children) ->
          let thing_defn =
            List.find (fun t_defn -> t_defn.tname = t_name) things
          in

          let children' =
            List.fold_left
              (fun accum (n, e) ->
                (* get thing item with name *)
                let _, b_typ, _ =
                  List.find
                    (fun (_, _, b_name) -> b_name = n)
                    thing_defn.elements
                in

                (* evalute expr *)
                let et, e' = expr e symbols in

                let err_msg =
                  "illegal value binding for member " ^ n ^ " of " ^ t_name
                  ^ " -- " ^ string_of_typ b_typ ^ ": " ^ string_of_typ et
                  ^ " failed in expresson " ^ string_of_expr e
                in

                (* make sure types match *)
                let _ = check_assign et b_typ err_msg in

                (n, (et, e')) :: accum)
              [] children
          in

          let _ =
            if List.length children' <> List.length thing_defn.elements then
              make_err
                ("number of fields in instance of thing " ^ t_name
               ^ " does not match thing definition")
            else ()
          in

          (Ident t_name, SThingValue (t_name, List.rev children'))
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
                make_err
                  "Can't rebind a deref because it is an owned value. Please \
                   clone the value before binding it."
            | _ -> ()
          in

          (* make sure we aren't assigning a unit value *)
          let _ =
            match t with Unit -> make_err "Can't bind a unit value." | _ -> ()
          in

          let is_mutborrow = match t with MutBorrow _ -> true | _ -> false in

          let _, lt = type_of_identifier name symbols
          and rt, e' = expr e symbols in
          (* If assigning to a borrow mutborrow,
             don't allow explicit lifetimes*)
          let _ =
            match lt with
            | MutBorrow (_, lifetime) | Borrow (_, lifetime) ->
                if lifetime <> "'_" then
                  make_err
                    ("Cannot define explicit lifetime " ^ lifetime
                   ^ " in assignment of " ^ name
                   ^ ". Explicit lifetimes are only permitted for function \
                      arguments and return types.")
            | _ -> ()
          in

          (* Can assign an ident that is of type (mut)borrow to a value *)
          let _ =
            match rt with
            | MutBorrow _ | Borrow _ -> (
                match e' with
                | SIdent n ->
                    make_err
                      ("cannot have ident" ^ n
                     ^ " of type (mut)borrow on right-hand side of assignment")
                | _ -> ())
            | _ -> ()
          in
          let err =
            "illegal assignment " ^ string_of_typ lt ^ " = " ^ string_of_typ rt
            ^ " in " ^ string_of_stmt ass 0
          in
          let _ = check_assign lt rt err in
          SAssign (is_mut || is_mutborrow, t, name, (rt, e'))
      | ReAssign (rb_is_mb, name, e) as ass ->
          (* make sure we aren't reassigning a deref to a variable (illegal) *)
          let _ =
            match e with
            | Unop (Deref, _) ->
                make_err
                  "Can't bind a deref because it is an owned value. Please \
                   clone the value before binding it."
            | _ -> ()
          in

          (*
             make sure the left hand side is either a
             1. mut ident (that is not of type immutable borrow)
             2. mut borrow of an ident
          *)
          let is_mut, lt = type_of_identifier name symbols in

          let lt, is_mutborrow =
            match (is_mut, lt) with
            | _, MutBorrow (t, _) ->
                if rb_is_mb then (t, true)
                else
                  make_err
                    ("mutborrow " ^ name
                   ^ " must be dereferenced to be assigned to")
            | true, Borrow _ ->
                if rb_is_mb then
                  make_err
                    ("deference of immutable borrow " ^ name
                   ^ " can never be reassigned")
                else (lt, false)
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
            let symbols = StringMap.add n (false, Int) symbols in
            let exprs =
              List.map
                (fun e ->
                  let t, e' = expr e symbols in
                  let err = "expected integer " ^ string_of_expr e in
                  let t' = check_assign t Int err in
                  (t', e'))
                [ e1; e2; e3 ]
            in
            let stmt_returns = detect_unreachable_code [ s ] in
            match exprs with
            | [ (t1', e1'); (t2', e2'); (t3', e3') ] ->
                (* todo: make sure n isn't in symbol table? *)
                SLoop
                  ( (t1', e1'),
                    (t2', e2'),
                    n,
                    (t3', e3'),
                    check_stmt s symbols,
                    stmt_returns )
            | _ -> make_err "panic!")
      | While (e, s) ->
          let t, e' = expr e symbols in
          let err = "expected boolean " ^ string_of_expr e in
          let t' = check_assign t Bool err in
          let stmt_returns = detect_unreachable_code [ s ] in
          SWhile ((t', e'), check_stmt s symbols, stmt_returns)
      | PipeOut e ->
          let t, e' = expr e symbols in

          (* check for derefs in args (illegal) *)
          let _ =
            match e with
            | Unop (Deref, _) ->
                make_err
                  "Can't provide the deref, which is an owned value, as a pipe \
                   return value. Please clone the value before returning it."
            | _ -> ()
          in

          let err =
            "expected return type of " ^ string_of_typ t ^ " for pipe " ^ p.name
          in
          let _ = check_assign t p.return_type err in
          SPipeOut (t, e')
      | If (e, stmt1, stmt2) ->
          let t, e' = expr e symbols in
          let err = "expected boolean " ^ string_of_expr e in
          let t' = check_assign t Bool err in

          let stmt1_returns = detect_unreachable_code [ stmt1 ]
          and stmt2_returns = detect_unreachable_code [ stmt2 ] in

          SIf
            ( (t', e'),
              check_stmt stmt1 symbols,
              check_stmt stmt2 symbols,
              stmt1_returns,
              stmt2_returns )
    in
    let sbody =
      if
        match
          List.find_opt (fun p' -> p'.name = p.name) built_in_pipe_decls
        with
        | Some _ -> (
            match p.return_type with MutBorrow _ -> false | _ -> true)
        | None -> false
      then []
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

  let s_pipes_with_builtins = List.map check_pipe pipe_decls in

  (* For the borrow checker, will not be included in final SAST *)

  (* boolean denotes verbosity - set to true if you want to *)
  (* see generated graph nodes and fn tests *)
  let node_ownership_map =
    borrow_ck s_pipes_with_builtins built_in_pipe_decls verbosity
  in

  (* remove built-in pdecls before returing *)
  let s_pipes =
    List.filter
      (fun p_decl ->
        not
          (match
             List.find_opt (fun p -> p.name = p_decl.sname) built_in_pipe_decls
           with
          | Some _ -> true
          | None -> false))
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

  ((check_things things, s_pipes_w_wrappers), node_ownership_map)
