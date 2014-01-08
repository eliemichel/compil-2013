open Ast
open Lexing
open Format

exception Error of string * position * position

let (globals : (string, ty) Hashtbl.t) = Hashtbl.create 17


let rec string_of_ty = function
	| TyTypeNull       -> "NULL"
	| TyVoid           -> "void"
	| TyInt            -> "int"
	| TyClass s        -> s
	| TyPointer t      -> (string_of_ty t) ^ "*"
	| TyFun (rt, r, args, argenv) ->
		"Function : " ^
		(List.fold_left
			(fun s v ->
				let t, r = Env.Local.find v argenv in
				s ^ ", " ^
				(string_of_ty t) ^
				(if r then "&" else "")
			)
			""
			args
		) ^
		" -> " ^
		(string_of_ty rt) ^
		(if r then "&" else "")


let rec convert_type = function
	| Void -> TyVoid
	| Int  -> TyInt
	| TIdent s ->
		if Hashtbl.mem tidentTbl s
		then TyClass s
		else assert false (*raise (Error ("Undefined type : " ^ s))*)



let rec decl_var_extract_name t = function
	| Ident  s          -> convert_type t, s, false
	| Pointer_value v   ->
		let t', s, r = decl_var_extract_name t v in
			if r
			then raise (Error (
				("Pointer to reference is not possible (in declaration of " ^ s.node ^ ")"),
				s.start_pos,
				s.end_pos
				))
			else
				TyPointer t', s, false
	| Address v         ->
		let t', s, r = decl_var_extract_name t v in
			if r
			then raise (Error (
				("Reference to reference is not possible (in declaration of " ^ s.node ^ ")"),
				s.start_pos,
				s.end_pos
				))
			else t', s, true

let convert_qident  = function
	| Simple_qident     s      -> s
	| Namespace_qident (ns, s) -> raise TODO


let rec decl_function_extract_name t = function (* Recopiage… *)
	| QIdent_in_qvar qi -> convert_type t, convert_qident qi, false
	| QPointer_value v  ->
		let t', s, r = decl_function_extract_name t v in
		if r
			then raise (Error (
				("Pointer to reference is not possible (in declaration of " ^ s.node ^ ")"),
				s.start_pos,
				s.end_pos
				))
			else
				TyPointer t', s, false
	| QAddress v       ->
		let t', s, r = decl_function_extract_name t v in
			if r
			then raise (Error (
				("Reference to reference is not possible (in declaration of " ^ s.node ^ ")"),
				s.start_pos,
				s.end_pos
				))
			else t', s, true


let decl_var env t s r =
	if Env.mem_local s.node env
	then raise (Error (
		("This variable has already been declared : " ^ s.node),
		s.start_pos,
		s.end_pos
		))
	else (
		Env.add s.node (t, r) env
	)

let decl_glob_var env t v =
	let t, s, r = decl_var_extract_name t v in
		if r
		then raise (Error (
				("Uninitialized reference : " ^ s.node),
				s.start_pos,
				s.end_pos
				))
		else
			Hashtbl.add globals s.node t;
			decl_var env t s r

let type_proto env proto =
	let args, local_env = List.fold_left (* /!\ List.rev *)
		(fun (l, loc) (t, v) ->
			let t, s, r = decl_var_extract_name t v in
				s.node :: l,
				Env.Local.add s.node (t, r) loc
		)
		([], Env.Local.empty)
		proto.args
	in
	match proto.start with
		| Function (t, qvar) ->
			let t, s, r = decl_function_extract_name t qvar in
			let t = TyFun (t, r, args, local_env) in
				let env = decl_var env t s r in
					env, local_env, args, t, s
		| Constructor s   -> raise (Error (
			("Constructor of " ^ s.node ^ " out of class declaration"),
			s.start_pos,
			s.end_pos
			))
		| Method (s1, s2) -> raise TODO

let rec num = function
	| TyTypeNull | TyInt | TyPointer _ -> true
	| _                                -> false

let est_sous_classe c1 c2 =
	ignore (c1, c2) ; raise TODO

let rec est_sous_type t1 t2 = match t1, t2 with
	| TyInt, TyInt
	| TyTypeNull, TyPointer _ -> true
	| TyClass c1, TyClass c2 -> est_sous_classe c1 c2
	| TyPointer t1', TyPointer t2' -> est_sous_type t1' t2'
	| _ -> false


let t_binop_of_binop = function
	| Dbleq -> Tset Teq
	| Neq   -> Tset Tneq
	| Lt    -> Tset Tlt
	| Leq   -> Tset Tleq
	| Gt    -> Tset Tgt
	| Geq   -> Tset Tgeq
	| Add   -> Tarith Tadd
	| Sub   -> Tarith Tsub
	| Mult  -> Tarith Tmult
	| Div   -> Tarith Tdiv
	| Mod   -> Tarith Tmod
	| And   -> Tarith Tand
	| Or    -> Tarith Tor

let t_unop_of_unop op expr = match op with
	| Not       -> Tnot expr
	| Minus     -> Tbinop (Tarith Tsub, Tint "0", expr)
	| Plus      -> expr
	| IncrLeft  -> Tincrleft expr
	| DecrLeft  -> Tdecrleft expr
	| IncrRight -> Tincrright expr
	| DecrRight -> Tdecrright expr
	| Star      -> Tdereference expr
	| Amp       -> Tgetaddr expr

let rec type_expr env e = match e.node with
	| This -> (
		try fst (Env.find "this" env), Tthis, true
		with Not_found -> raise (
				Error ("Undefined identifier 'this'", e.start_pos, e.end_pos)
			)
		)
	| Null -> TyTypeNull, Tnull, false
	| Integer     s -> TyInt, Tint s, false
	| QIdent (Simple_qident s) ->
		(try let t, _ = Env.find s.node env in
			match t with
				| TyFun _ -> t, Tfun s.node, true
				| _       -> t, Tvar s.node, true
		with Not_found -> raise (Error (
					("Undefined identifier '" ^ s.node ^ "'"),
					s.start_pos,
					s.end_pos
				))
		)
	| QIdent _ -> raise TODO
	| Dot         (e, s) -> raise TODO
	| Arrow       (e, s) -> raise TODO
	| Eq          (e1, e2) ->
		let t1, e1, g1 = type_expr env e1 in
		let t2, e2, g2 = type_expr env e2 in
		let err m = raise (Error (m, e.start_pos, e.end_pos)) in
			     if not g1
				then err "Invalid left member"
			else if not (est_sous_type t2 t1)
				then err ("'" ^ (string_of_ty t2) ^ "' is not a valid subtype of '" ^ (string_of_ty t2) ^ "'")
			else if not (num t1)
				then err ("'" ^ (string_of_ty t1) ^ "' can't be assigned (not a numeric type)")
			else t1, Tassign (e1, e2), false
	| Application (e, args) ->
		let err m = raise (Error (m, e.start_pos, e.end_pos)) in
		let t, f, _ = type_expr env e in
		let fun_t, r, fun_args, fun_env = match t with
			| TyFun (t, r, a, aenv) -> t, r, a, aenv
			| _ -> err "A function was expected"
		in
		let typed_args = List.map (type_expr env) args in
		let args_type = List.map (fun (t,_,g) -> (t,g)) typed_args in
		let expr_args = List.map (fun (_,e,_) -> e) typed_args in
		let check =
			let rec aux a b = match a, b with
				| []      , []       -> true
				| v1 :: q1, (t2, g) :: q2 ->
					let t1, r = Env.Local.find v1 fun_env in
						(g || (not r)) && t1 = t2 && aux q1 q2
				| _                  -> false
			in aux fun_args args_type
		in
		if not check
		then err "Given arguments doesn't match expected ones" (*TODO : expliciter le message d'erreur*)
		else
			fun_t, Tcall (f, r, expr_args), r
	| New         (s, args) -> raise TODO
	| Unop        (op, e') ->
		let err m = raise (Error (m, e.start_pos, e.end_pos)) in
		let t, e', g = type_expr env e' in
		let rt, rg =
		match op with
			| Not | Minus | Plus ->
				if t != TyInt
				then err ("'" ^ (string_of_ty t) ^ "' is not integer")
				else TyInt, false
			| IncrLeft | DecrLeft | IncrRight | DecrRight ->
				     if t != TyInt
					then err ("'" ^ (string_of_ty t) ^ "' is not integer")
				else if not g
					then err "A left value was expected"
				else TyInt, false
			| Star ->
				(match t with
				| TyPointer t' -> t', true
				| _           -> err ("'" ^ (string_of_ty t) ^ "' is not a pointer type")
				)
			| Amp ->
				if not g then err "A left value was expected"
				else TyPointer t, false
		in rt, t_unop_of_unop op e', rg
	| Binop       (op, e1, e2) ->
		let t1, e1, g1 = type_expr env e1 in
		let t2, e2, g2 = type_expr env e2 in
		let err m = raise (Error (m, e.start_pos, e.end_pos)) in
		let comp = op = Dbleq || op = Neq in
			     if comp && t1 <> t2
				then err ("'" ^ (string_of_ty t2) ^ "' can not be compared to '" ^ (string_of_ty t1) ^ "'")
			else if comp && not (num t1)
				then err ("'" ^ (string_of_ty t1) ^ "' can not be compared (not a numeric types)")
			else if t1 <> TyInt
				then err ("The first operand was expected to be an integer but is '" ^ (string_of_ty t1) ^ "'")
			else if t2 <> TyInt
				then err ("The second operand was expected to be an integer but is '" ^ (string_of_ty t2) ^ "'")
			else TyInt, Tbinop (t_binop_of_binop op, e1, e2), false


let type_cout env instr = function
	| Expression_in_flow e ->
		let t, te, _ = type_expr env e in
			if t <> TyInt
			then raise (
					Error (("Only integers and strings can be printed ('" ^ (string_of_ty t) ^ "' given)"), e.start_pos, e.end_pos)
				)
			else
				Tcout_expr te :: instr
	| String_in_flow     s ->
		Tcout_str s :: instr


let rec type_instr parent_fun (env, instr) = function
	| Empty                                  -> env, instr
	| Expression e                           ->
		let _, e, _ = type_expr env e in
			env, Texpr e :: instr
	| Var_init   (t, var, var_val)           ->
		let t, s, r = decl_var_extract_name t var in
		let env' = decl_var env t s r in
		env',
		let instr = Tdecl s.node :: instr in
			(match var_val with
				| None ->
					if r
					then raise (Error (
						("Uninitialized reference : " ^ s.node),
						s.start_pos,
						s.end_pos
						))
					else instr
				| Some (Value    e) ->
					let t', e', g' = type_expr env e in
					let err m = raise (Error (m, e.start_pos, e.end_pos)) in
						if not (est_sous_type t' t)
						then err ("'" ^ (string_of_ty t') ^ "' is not a valid subtype of '" ^ (string_of_ty t) ^ "'")
						else
							if r
							then
								if not g'
								then err "A left value was expected"
								else (Texpr (Trefinit (s.node, e'))) :: instr
							else (Texpr (Tassign (Tvar s.node, e'))) :: instr
				| Some (Returned _) -> raise TODO
			)
	| If_else    (test, instr1, instr2)      ->
		let t, e, _ = type_expr env test in
		if t <> TyInt
		then raise ( Error (
				"Condition must be an integer", test.start_pos, test.end_pos
			))
		else
		let _, i1 = type_instr parent_fun (env, []) (Bloc [instr1]) in
		let _, i2 = type_instr parent_fun (env, []) (Bloc [instr2]) in
			env, (Tifelse (e, List.rev i1, List.rev i2)) :: instr
	| For        (before, test, iter, corps) ->
		let t, t_test, _ = type_expr env test in
		if t <> TyInt
		then raise ( Error (
				"Condition must be an integer", test.start_pos, test.end_pos
			))
		else
		let f e = let _, e, _ = type_expr env e in e in
		let t_before = List.map f before in
		let t_iter = List.map f iter in
		let _, t_corps = type_instr parent_fun (env, []) (Bloc [corps]) in
			env, (Tfor (t_before, t_test, t_iter, List.rev t_corps)) :: instr
	| Bloc       bloc                        ->
		let env', instr' =
			List.fold_left (type_instr parent_fun) (Env.push_empty env, []) bloc
		in
		let local_env, _ = Env.pop env' in
			env,
			if Env.Local.is_empty local_env
			then instr' @ instr
			else Tfree :: instr' @ ((Tmalloc local_env) :: instr)
	| Cout       expr_list                   ->
		env, List.fold_left (type_cout env) instr expr_list
	| Return     some_expr                   ->
		let fr, ft = match fst (Env.find parent_fun env) with
			| TyFun (t, r, _, _) -> r, t
			| _ -> assert false
		in
		let s = match some_expr with
			| None -> None
			| Some expr ->
				let t, e, _ = type_expr env expr in
					if t <> ft
					then raise (Error (
						("Returned type is '" ^ (string_of_ty t) ^ "' instead of '" ^ (string_of_ty ft) ^ "'"),
						expr.start_pos,
						expr.end_pos
					))
					else Some (e, fr)
		in
			env, (Treturn s) :: instr

let type_decl (env, decls) = function
	| Decl_vars  (t, vars) ->
		List.fold_left
			(fun env v -> decl_glob_var env t v)
			env
			vars,
		decls
	
	| Proto_bloc (proto, bloc) ->
		let env, local_env, args, t, s = type_proto env proto in
		let env' = Env.push local_env env in
		let _, instr = type_instr s.node (env', []) (Bloc bloc) in
			env, Tdeclfun (s.node, args, local_env, List.rev instr) :: decls
	| _ -> raise TODO


let typing pAst =
	let env, decls = List.fold_left
		type_decl
		(Env.empty, [])
		pAst.decls
	in {
		declarations = List.rev decls;
		globals = Env.get_globals env
	}




