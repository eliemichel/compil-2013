open Ast
open Lexing

exception Error of string * position * position

let (globals : (string, ty) Hashtbl.t) = Hashtbl.create 17


let rec string_of_ty = function
	| TyTypeNull       -> "TyTypeNull"
	| TyVoid           -> "TyVoid"
	| TyInt            -> "TyInt"
	| TyClass s        -> "TyClass " ^ s
	| TyPointer t      -> "TyPointer " ^ (string_of_ty t)
	| TyFun (rt, argt) ->
		"TyFun : " ^
		(List.fold_left (fun s t -> s ^ ", " ^ (string_of_ty t)) "" argt) ^
		" -> " ^
		(string_of_ty rt)


let rec convert_type = function
	| Void -> TyVoid
	| Int  -> TyInt
	| TIdent s ->
		if Hashtbl.mem tidentTbl s
		then TyClass s
		else assert false (*raise (Error ("Undefined type : " ^ s))*)



let rec decl_var_extract_name t = function
	| Ident  s          -> convert_type t, s
	| Pointer_value v   ->
		let t', s = decl_var_extract_name t v in
			TyPointer t', s
	| Address (Ident s) -> raise TODO (* Autoriser plus qu'un ident *)
	| Address v         ->
		let _, s = decl_var_extract_name t v in
			raise (Error (
				("Unexpected `&` in declaration of " ^ s.node),
				s.start_pos,
				s.end_pos
				))


let convert_qident  = function
	| Simple_qident     s      -> s
	| Namespace_qident (ns, s) -> ignore (ns, s) ; raise TODO


let rec decl_function_extract_name t = function
	| QIdent_in_qvar qi            -> convert_type t, convert_qident qi
	| QPointer_value v             ->
		let t', s = decl_function_extract_name t v in
			TyPointer t', s
	| QAddress (QIdent_in_qvar qi) -> raise TODO
	| QAddress v                   ->
		let _, s = decl_function_extract_name t v in
			raise (Error (
				("Unexpected `&` in declaration of " ^ s.node),
				s.start_pos,
				s.end_pos
				))



let decl_var env t s =
	if Env.mem s.node env
	then raise (Error (
		("This variable has already been declared : " ^ s.node),
		s.start_pos,
		s.end_pos
		))
	else (
		Env.add s.node t env
	)

let type_decl_var glob env t v =
	let t, s = decl_var_extract_name t v in
		Hashtbl.add globals s.node t;
		decl_var env t s

let type_proto env proto =
	let args, argst, env' = List.fold_left (* /!\ List.rev *)
		(fun (l, lt, c) (t, v) ->
			let t, s = decl_var_extract_name t v in
				(t, s) :: l,
				t :: lt,
				Env.add s.node t c
		)
		([], [], env)
		proto.args
	in
	match proto.start with
		| Function (t, qvar) ->
			let t, s = decl_function_extract_name t qvar in
			let t = TyFun (t, argst) in
				let env = decl_var env t s in
				let env' = decl_var env' t s in
					env, env', t, s
		| Constructor s   -> raise (Error (
			("Constructor of " ^ s.node ^ " out of class declaration"),
			s.start_pos,
			s.end_pos
			))
		| Method (s1, s2) -> ignore (s1, s2) ; raise TODO

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


let t_op_of_op = function
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

let rec type_expr env e = match e.node with
	| This -> (
		try Env.find "this" env, Tthis, true
		with Not_found -> raise (
				Error ("Undefined identifier 'this'", e.start_pos, e.end_pos)
			)
		)
	| Null -> TyTypeNull, Tnull, false
	| Integer     s -> TyInt, Tint s, false
	| QIdent (Simple_qident s) ->
		let n =
			if Hashtbl.mem globals s.node
			then Tglobal s.node
			else Tlocal  s.node
		in
		(try Env.find s.node env, n, true
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
			     if not g1 then err "Invalid left member"
			else if not (est_sous_type t2 t1) then err "Wrong type" (* TODO : message d'erreur plus étoffé *)
			else if not (num t1) then err "Only numerical types can be assigned"
			else t1, Tassign (e1, e2), false
	| Application (e, args) -> raise TODO
	| New         (s, args) -> raise TODO
	| Unop        (op, e) -> raise TODO
	| Binop       (op, e1, e2) ->
		let t1, e1, g1 = type_expr env e1 in
		let t2, e2, g2 = type_expr env e2 in
		let err m = raise (Error (m, e.start_pos, e.end_pos)) in
		let comp = op = Dbleq || op = Neq in
			     if comp && t1 <> t2 then err "Compared values must have the same type"
			else if comp && not (num t1) then err "Only numerical types can be compared"
			else if t1 <> TyInt || t2 <> TyInt then err "Int expected"
			else TyInt, Tbinop (t_op_of_op op, e1, e2), false


let type_cout (env, instr) = function
	| Expression_in_flow e ->
		let t, te, _ = type_expr env e in
			if t <> TyInt
			then raise (
					Error ("Only integers and strings can be printed", e.start_pos, e.end_pos)
				)
			else
				env, Tcout_expr te :: instr
	| String_in_flow     s -> env, Tcout_str s :: instr


let rec type_instr (env, instr) = function
	| Empty                                  -> env, instr
	| Expression e                           ->
		let _, e, _ = type_expr env e in
			env, Texpr e :: instr
	| Var_init   (t, var, var_val)           ->
		let t, s = decl_var_extract_name t var in
		let env' = decl_var env t s in
		env',
		(match var_val with
			| None -> instr
			| Some (Value    e) ->
				let _, e', _ = type_expr env e in
					(Texpr (Tassign (Tlocal s.node, e'))) :: instr
			| Some (Returned _) -> raise TODO
		)
	| If_else    (test, instr1, instr2)      ->
		let t, e, _ = type_expr env test in
		if t <> TyInt
		then raise ( Error (
				"Condition must be an integer", test.start_pos, test.end_pos
			))
		else
		let _, i1 = type_instr (env, []) (Bloc [instr1]) in
		let _, i2 = type_instr (env, []) (Bloc [instr2]) in
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
		let _, t_corps = type_instr (env, []) (Bloc [corps]) in
			env, (Tfor (t_before, t_test, t_iter, List.rev t_corps)) :: instr
	| Bloc       bloc                        ->
		let env', instr' = List.fold_left type_instr (env, []) bloc in
		let local_env = Env.filter (fun v _ -> not (Env.mem v env)) env' in
			if Env.is_empty local_env
			then env, instr' @ instr
			else env, Tfree :: instr' @ ((Tmalloc local_env) :: instr)
	| Cout       expr_list                   ->
		List.fold_left type_cout (env, instr) expr_list
	| Return     e                           ->
		env, (Treturn 0) :: instr

let type_decl (env, decls) = function
	| Decl_vars  (t, vars) ->
		List.fold_left
			(fun env v -> type_decl_var true env t v)
			env
			vars,
		decls
	
	| Proto_bloc (proto, bloc) ->
		let env, env', t, s = type_proto env proto in
		let _, instr = type_instr (env', []) (Bloc bloc) in
			env, Tfun (t, s.node, List.rev instr) :: decls
	| _ -> raise TODO


let typing pAst =
	let _, decls = List.fold_left
		type_decl
		(Env.empty, [])
		pAst.decls
	in {
		declarations = List.rev decls;
		globals = globals
	}




