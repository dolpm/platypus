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
      ("printnl", [ (false, String, "x") ], Unit);
      ("panic", [ (false, String, "x") ], Unit);
      ("int_to_string", [ (false, Int, "x") ], String);
      ("float_to_string", [ (false, Float, "x") ], String);
      ("char_to_string", [ (false, Char, "x") ], String);
      ("bool_to_string", [ (false, Bool, "x") ], String);
      (* TODO: does x have to be mutable? *)
      ("Heap_alloc", [ (true, Generic, "x") ], Box Generic);
      ("Vector_length", [ (false, Vector Generic, "x") ], Int);
      ("Vector_alloc", [], Vector Generic);
      ("Vector_get", [ (false, Vector Generic, "x") ], Option Generic);
      ( "Vector_push",
        [ (true, Vector Generic, "x"); (false, Generic, "y") ],
        Unit );
      ("Vector_pop", [ (true, Vector Generic, "x") ], Option Generic);
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

  (* recursively fetch all assignments in a statement list *)
  let rec find_bindings (body : stmt list) =
    List.flatten
      (List.filter_map
         (fun s ->
           match s with
           | Assign (is_mut, typ, name, _) -> Some [ (is_mut, typ, name) ]
           | Block stmts -> Some (find_bindings stmts)
           | While (_, s) -> Some (find_bindings [ s ])
           | Loop (_, _, id, _, s1) ->
               Some ((false, Int, id) :: find_bindings [ s1 ])
           | If (_, s1, s2) -> Some (find_bindings [ s1; s2 ])
           | _ -> None)
         body)
  in

  let check_pipe p =
    let formals' = check_bindings p.formals in
    let locals' = find_bindings p.body in

    (* make sure lhs and rhs of assignments and re-assignments are of eq type *)
    let check_assign lvaluet rvaluet err =
      match (lvaluet, rvaluet) with
      | Vector Generic, Vector rt -> Vector rt
      | Box Generic, Box rt -> Box rt
      | Generic, _ -> rvaluet
      | _ -> if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    let symbols =
      List.fold_left
        (fun m (is_mut, typ, name) -> StringMap.add name (is_mut, typ) m)
        StringMap.empty (formals' @ locals')
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier (s : string) : bool * defined_type =
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
    let rec expr = function
      | IntLiteral l -> (Int, SIntLiteral l)
      | FloatLiteral l -> (Float, SFloatLiteral l)
      | BoolLiteral l -> (Bool, SBoolLiteral l)
      | CharLiteral l -> (Char, SCharLiteral l)
      | UnitLiteral -> (Unit, SUnitLiteral)
      | StringLiteral l -> (String, SStringLiteral l)
      | Unop (op, e1) as e ->
          let t1, e1' = expr e1 in
          let ty =
            match op with
            | Neg when t1 = Int || t1 = Float -> t1
            | Not when t1 = Bool -> t1
            | Ref -> (
                match t1 with
                | MutBorrow _ | Borrow _ ->
                    make_err "can't take a borrow of a borrow"
                | _ -> Borrow (t1, "'_"))
            | MutRef -> (
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
          let t1, e1' = expr e1 and t2, e2' = expr e2 in
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
            let check_pipein (_, ft, _) e =
              (* we will check lifetimes later - just make sure they are ambiguous *)
              (* for this step *)
              let ft =
                match ft with
                | Borrow (ty, _lt) -> Borrow (ty, "'_")
                | MutBorrow (ty, _lt) -> MutBorrow (ty, "'_")
                | f -> f
              in
              let et, e' = expr e in
              let err =
                "illegal argument found " ^ string_of_typ et ^ " expected "
                ^ string_of_typ ft ^ " in " ^ string_of_expr e
              in
              (check_assign ft et err, e')
            in
            let args' = List.map2 check_pipein pd.formals args in
            let first_arg_type =
              if List.length args' > 0 then fst (List.hd args') else Generic
            in
            let ret_type =
              match pd.return_type with
              | Generic -> first_arg_type
              | Box Generic -> Box first_arg_type
              | Vector Generic -> Vector first_arg_type
              | Borrow (ty, _) -> Borrow (ty, "'_")
              | MutBorrow (ty, _) -> MutBorrow (ty, "'_")
              | _ -> pd.return_type
            in
            (ret_type, SPipeIn (pname, args'))
      | Ident s -> (snd (type_of_identifier s), SIdent s)
      | _ -> (Unit, SNoexpr)
    in

    (* illegal to do references of references *)
    (* illegal to dereference a @& or @~& *)
    (* handle either syntactically or semantically *)

    (* Return a semantically-checked statement, i.e. containing s_exprs *)
    let rec check_stmt = function
      | Expr e -> SExpr (expr e)
      | Block sl ->
          SBlock
            ( List.map check_stmt sl,
              let _ = cur_sblock_id := !cur_sblock_id + 1 in
              string_of_int (!cur_sblock_id - 1) )
      | Assign (is_mut, t, name, e) as ass ->
          let _, lt = type_of_identifier name and rt, e' = expr e in
          let err =
            "illegal assignment " ^ string_of_typ lt ^ " = " ^ string_of_typ rt
            ^ " in " ^ string_of_stmt ass 0
          in
          let _ = check_assign t lt err in
          SAssign (is_mut, t, name, (rt, e'))
      | ReAssign (name, e) as ass ->
          let _, lt = type_of_identifier name and rt, e' = expr e in
          let err =
            "illegal assignment " ^ string_of_typ lt ^ " = " ^ string_of_typ rt
            ^ " in " ^ string_of_stmt ass 0
          in
          let _ = check_assign rt lt err in
          SReAssign (name, (rt, e'))
      | Loop (e1, e2, n, e3, s) -> (
          let exprs =
            List.map
              (fun e ->
                let t, e' = expr e in
                let err = "expected integer " ^ string_of_expr e in
                let t' = check_assign t Int err in
                (t', e'))
              [ e1; e2; e3 ]
          in
          match exprs with
          | [ (t1', e1'); (t2', e2'); (t3', e3') ] ->
              (* todo: make sure n isn't in symbol table? *)
              SLoop ((t1', e1'), (t2', e2'), n, (t3', e3'), check_stmt s)
          | _ -> make_err "panic!")
      | While (e, s) ->
          let t, e' = expr e in
          let err = "expected boolean " ^ string_of_expr e in
          let t' = check_assign t Bool err in
          SWhile ((t', e'), check_stmt s)
      | PipeOut e -> SPipeOut (expr e)
      | If (e, stmt1, stmt2) ->
          let t, e' = expr e in
          let err = "expected boolean " ^ string_of_expr e in
          let t' = check_assign t Bool err in
          SIf ((t', e'), check_stmt stmt1, check_stmt stmt2)
    in
    let sbody =
      if StringMap.mem p.name built_in_pipe_decls then []
      else
        match check_stmt (Block p.body) with
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

  ([], s_pipes_w_wrappers)
