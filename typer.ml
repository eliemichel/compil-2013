open Ast
open Lexing
open Format

exception Error of string * position * position

let err n m = raise (Error (m, n.start_pos, n.end_pos))


let rec string_of_ty = function
	| TyTypeNull       -> "NULL"
	| TyVoid           -> "void"
	| TyInt            -> "int"
	| TyClass s        -> s
	| TyPointer t      -> (string_of_ty t) ^ "*"
	| TyFun (rt, r, init, args, argenv) ->
		let aux s v =
			let t, r = Env.Local.find v argenv in
				sprintf "%s%s%s%s"
					s
					(if s = "" then "" else ", ")
					(string_of_ty t)
					(if r then "&" else "")
		in sprintf "%s%s (%s)%s"
			(string_of_ty rt)
			(if r then "&" else "")
			(List.fold_left aux "" args )
			(if init then "" else " prototype")


let rec convert_type = function
	| Void -> TyVoid
	| Int  -> TyInt
	| TIdent s ->
		if Hashtbl.mem tidentTbl s
		then TyClass s
		else assert false (*raise (Error ("Undefined type : " ^ s))*)



let cut_at_point k =
	let n = String.length k in
	let i = String.index k '.' in
		String.sub k 0 i,
		String.sub k (i + 1) (n - i - 1)



let rec decl_var_extract_name t = function
	| Ident  s          -> convert_type t, s, false
	| Pointer_value v   ->
		let t', s, r = decl_var_extract_name t v in
			if r
			then err s ("Pointer to reference is not possible (in declaration of " ^ s.node ^ ")")
			else
				TyPointer t', s, false
	| Address v         ->
		let t', s, r = decl_var_extract_name t v in
			if r
			then err s ("Reference to reference is not possible (in declaration of " ^ s.node ^ ")")
			else t', s, true

let convert_qident class_name = function
	| Simple_qident     s      -> s
	| Namespace_qident (ns, s) ->
		let n = {
			node = sprintf "%s.%s" ns.node s.node;
			start_pos = ns.start_pos;
			end_pos = s.end_pos
		} in
			if class_name <> ""
			then err n (sprintf "Unexpected qualification '%s.'" ns.node)
			else n


let rec decl_function_extract_name class_name t = function (* Recopiage… *)
	| QIdent_in_qvar qi -> convert_type t, convert_qident class_name qi, false
	| QPointer_value v  ->
		let t', s, r = decl_function_extract_name class_name t v in
		if r
			then err s ("Pointer to reference is not possible (in declaration of " ^ s.node ^ ")")
			else TyPointer t', s, false
	| QAddress v       ->
		let t', s, r = decl_function_extract_name class_name t v in
			if r
			then err s ("Reference to reference is not possible (in declaration of " ^ s.node ^ ")")
			else t', s, true


(* fonction d'erreur, environnement, type, nom, si c'est une référence *)
let decl_var err env t s r =
	try
		let t', r' = Env.find_local s env in
		match t, t' with
			| TyFun (rt , r , true , args , argenv ),
			  TyFun (rt', r', false, args', argenv') ->
				let rec aux l l' = match l, l' with
					| [], [] -> true
					| a :: q, a' :: q' ->
						(Env.Local.find a argenv) = (Env.Local.find a' argenv')
						&& (aux q q')
					| _ -> false
				in
				if rt = rt' && r = r' && aux args args'
				then Env.add s (t, r) env
				else err (sprintf "'%s' declaration does not match its prototype" s)
			| _ -> err (sprintf "This variable has already been declared : %s" s)
	with Not_found ->
		try
			match t with
			| TyFun (_, _, true ,_ ,_) ->
				let c, f = cut_at_point s in
				err (sprintf "No method '%s' declared in class '%s'" f c)
			| _ -> raise Not_found
		with Not_found -> Env.add s (t, r) env

let decl_glob_var env t v =
	let t, s, r = decl_var_extract_name t v in
		if r
		then err s ("Uninitialized reference : " ^ s.node)
		else decl_var (err s) env t s.node r


let rec num = function
	| TyTypeNull | TyInt | TyPointer _ -> true
	| _                                -> false

let est_sous_classe c1 c2 =
	c1 = c2

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
	| And   -> Tlazy Tand
	| Or    -> Tlazy Tor

let t_unop_of_unop op expr = match op with
	| Not       -> Tnot expr
	| Minus     -> Tbinop (Tarith Tsub, Tint "0", expr)
	| Plus      -> expr
	| IncrLeft  -> Tincrleft  expr
	| DecrLeft  -> Tdecrleft  expr
	| IncrRight -> Tincrright expr
	| DecrRight -> Tdecrright expr
	| Star      -> Tderef     expr
	| Amp       -> Tgetaddr   expr


let parse_int i =
	let n = String.length i in
	Int32.to_string (Int32.of_string (
		if i.[0] = '0' && n > 1 && i.[1] <> 'x'
		then "0o" ^ (String.sub i 1 (n - 1))
		else i
	))


let rec type_fun pf members env erre t args =
	let fun_t, r, fun_args, fun_env =
			match t with
			| TyFun (t, r, _, a, aenv) -> t, r, a, aenv
			| _ -> erre "A function was expected here"
		in
		let n, m = List.length fun_args, List.length args in
		let rec aux exprs fargs args =
			match fargs, args with
				| [], [] -> exprs
				| [], _  ->
					ignore (erre (sprintf "Too many arguments (%d given, %d expected)" n m)); []
				| _ , [] ->
					ignore (erre (sprintf "Missing arguments (%d given, %d expected)" n m)); []
				| farg :: fq, arg :: q ->
					let t, e, g = type_expr pf members env arg in
					let ft, r = Env.Local.find farg fun_env in
							 if not (est_sous_type t ft)
							then err arg (sprintf
									"'%s' is not a valid subtype of '%s'"
									(string_of_ty t) (string_of_ty ft)
								)
						else if r && (not g)
							then err arg "A left value was expected"
						else aux ((e, r) :: exprs) fq q
		in aux [] fun_args args, (fun_t, r)
	


and type_expr pf members env e = match e.node with
	| This ->
		(match members with
		| None -> err e "'this' is defined inside methods only"
		| _ ->
			let c, _ = cut_at_point pf in
				TyPointer(TyClass c), Tthis, false
		)
	| Null -> TyTypeNull, Tnull, false
	| Integer     s -> TyInt, Tint (parse_int s), false
	| QIdent (Simple_qident s) ->
		(try let t, _ = Env.find s.node env in
			match t with
				| TyFun _ -> t, Tfun s.node, false
				| _       -> t, Tvar s.node, true
		 with Not_found ->
		 try
			match members with
				| None   -> raise Not_found
				| Some m ->
			 	let t, _ = Env.Local.find s.node m in
			 	let k = sprintf "%s.%s" (fst (cut_at_point pf)) s.node in
				match t with
					| TyFun _ -> t, Tlocalmeth k, false
					| _       -> t, Tlocalattr k, true
		 with Not_found -> err s ("Undefined identifier '" ^ s.node ^ "'")
		)
	| QIdent _ -> assert false ; raise TODO
	| Dot         (ec, s) ->
		let t, te, g = type_expr pf members env ec in
			(match t with
			| TyClass c ->
				let k = sprintf "%s.%s" c s in
				(try let t', r = Env.find k env in
					(match t' with
					| TyFun _ -> t', Tmeth (te, k), false
					| _       -> t', Tattr (te, k), true
					)
				 with Not_found -> err e (sprintf "'%s' has no member named '%s'" c s)
				)
			| _ -> err ec (sprintf "'%s' is not a class type" (string_of_ty t))
			)
	| Eq          (e1, e2) ->
		let t1, e1, g1 = type_expr pf members env e1 in
		let t2, e2, g2 = type_expr pf members env e2 in
		     if not g1
			then err e "Invalid left member"
		else if not (est_sous_type t2 t1)
			then err e ("'" ^ (string_of_ty t2) ^ "' is not a valid subtype of '" ^ (string_of_ty t2) ^ "'")
		else if not (num t1)
			then err e ("'" ^ (string_of_ty t1) ^ "' can't be assigned (not a numeric type)")
		else t1, Tassign (e1, e2), false
	| Application (e, args) ->
		let t, f, _ = type_expr pf members env e in
		let exprs, (fun_t, r) = type_fun pf members env (err e) t args in
			fun_t, Tcall (f, r, exprs), r
	| New         (s, args) ->
		let k = sprintf "%s.%s" s s in
		let t, _ = Env.find k env in
		let exprs, _ = type_fun pf members env (err e) t args in
			TyPointer(TyClass s), Tnew (s, exprs), false
	| Unop        (op, e') ->
		let t, e', g = type_expr pf members env e' in
		let rt, rg =
		match op with
			| Not | Minus | Plus ->
				if t != TyInt
				then err e ("'" ^ (string_of_ty t) ^ "' is not integer")
				else TyInt, false
			| IncrLeft | DecrLeft | IncrRight | DecrRight ->
				     if t != TyInt
					then err e ("'" ^ (string_of_ty t) ^ "' is not integer")
				else if not g
					then err e "A left value was expected"
				else TyInt, false
			| Star ->
				(match t with
				| TyPointer t' -> t', true
				| _           -> err e ("'" ^ (string_of_ty t) ^ "' is not a pointer type")
				)
			| Amp ->
				if not g then err e "A left value was expected"
				else TyPointer t, false
		in rt, t_unop_of_unop op e', rg
	| Binop       (op, e1i, e2i) ->
		let t1, e1, g1 = type_expr pf members env e1i in
		let t2, e2, g2 = type_expr pf members env e2i in
		let comp = op = Dbleq || op = Neq in
			     if comp && t1 <> t2
				then err e ("'" ^ (string_of_ty t2) ^ "' can not be compared to '" ^ (string_of_ty t1) ^ "'")
			else if comp && not (num t1)
				then err e ("'" ^ (string_of_ty t1) ^ "' can not be compared (not a numeric types)")
			else if t1 <> TyInt
				then err e1i ("'" ^ (string_of_ty t1) ^ "' is not integer")
			else if t2 <> TyInt
				then err e2i ("'" ^ (string_of_ty t2) ^ "' is not integer")
			else TyInt, Tbinop (t_binop_of_binop op, e1, e2), false


let type_cout pf members env instr = function
	| Expression_in_flow e ->
		let t, te, _ = type_expr pf members env e in
			if t <> TyInt
			then err e ("Only integers and strings can be printed ('" ^ (string_of_ty t) ^ "' given)")
			else
				Tcout_expr te :: instr
	| String_in_flow     s ->
		Tcout_str s :: instr


let rec type_instr pf members (env, instr) = function
	| Empty                                  -> env, instr
	| Expression e                           ->
		let _, e, _ = type_expr pf members env e in
			env, Texpr e :: instr
	| Var_init   (t, var, var_val)           ->
		let t, s, r = decl_var_extract_name t var in
		let env' = decl_var (err s) env t s.node r in
		env',
		let instr = Tdecl s.node :: instr in
			(match var_val with
				| None ->
					if r
					then err s ("Uninitialized reference : " ^ s.node)
					else instr
				| Some (Value    e) ->
					let t', e', g' = type_expr pf members env e in
						if not (est_sous_type t' t)
						then err e ("'" ^ (string_of_ty t') ^ "' is not a valid subtype of '" ^ (string_of_ty t) ^ "'")
						else
							if r
							then
								if not g'
								then err e "A left value was expected"
								else (Texpr (Trefinit (s.node, e'))) :: instr
							else (Texpr (Tassign (Tvar s.node, e'))) :: instr
				| Some (Returned r) ->
					let c, args = r.node in
					let k = sprintf "%s.%s" c c in
						let t, _ = Env.find k env in
						let exprs, _ = type_fun pf members env (err r) t args in
							(Tconstr (Tvar s.node, c, exprs)) :: instr
			)
	| If_else    (test, instr1, instr2)      ->
		let t, e, _ = type_expr pf members env test in
		if t <> TyInt
		then err test "Condition must be an integer"
		else
		let _, i1 = type_instr pf members (env, []) (Bloc [instr1]) in
		let _, i2 = type_instr pf members (env, []) (Bloc [instr2]) in
			env, (Tifelse (e, List.rev i1, List.rev i2)) :: instr
	| For        (before, test, iter, corps) ->
		let t, t_test, _ = type_expr pf members env test in
		if t <> TyInt
		then err test "Condition must be an integer"
		else
		let f e = let _, e, _ = type_expr pf members env e in e in
		let t_before = List.map f before in
		let t_iter = List.map f iter in
		let _, t_corps =
			type_instr pf members (env, []) (Bloc [corps])
		in
			env, (Tfor (t_before, t_test, t_iter, List.rev t_corps)) :: instr
	| Bloc       bloc                        ->
		let env', instr' =
			List.fold_left
				(type_instr pf members)
				(Env.push_empty env, [])
				bloc
		in
		let local_env, _ = Env.pop env' in
			env,
			if Env.Local.is_empty local_env
			then instr' @ instr
			else Tfree :: instr' @ ((Tmalloc local_env) :: instr)
	| Cout       expr_list                   ->
		env, List.fold_left (type_cout pf members env) instr expr_list
	| Return     some_expr                   ->
		let fr, ft = match fst (Env.find pf env) with
			| TyFun (t, r, _, _, _) -> r, t
			| t -> eprintf "%s : %s\n" pf (string_of_ty t) ; assert false
		in
		let s = match some_expr.node with
			| None ->
				if TyVoid <> ft
				then err some_expr ("Returned type is 'void' instead of '" ^ (string_of_ty ft) ^ "'")
				else None
			| Some expr ->
				let t, e, _ = type_expr pf members env expr in
					if t <> ft
					then err expr ("Returned type is '" ^ (string_of_ty t) ^ "' instead of '" ^ (string_of_ty ft) ^ "'")
					else Some (e, fr)
		in
			env, (Treturn s) :: instr


let type_proto env cl proto class_name bloc =
	let instr, is_instr = match bloc with
		| None   -> Bloc [], false
		| Some i -> Bloc i , true
	in
	let args, local_env = List.fold_right
		(fun (t, v) (l, loc) ->
			let t, s, r = decl_var_extract_name t v in
				if Env.Local.mem s.node loc
				then err s "There is another argument with the same identifier."
				else
					s.node :: l,
					Env.Local.add s.node (t, r) loc
		)
		proto.args
		([], Env.Local.empty)
	in
	let get_attr_env fn =
		let cn =
			if class_name = ""
			then
				try fst (cut_at_point fn)
				with Not_found -> ""
			else class_name
		in
		if cn = ""
		then None
		else Some (
			Env.fold
				(fun k v e ->
					try
						let c, id = cut_at_point k in
							if c = cn
							then Env.Local.add id v e
							else e
					with Not_found -> e
				)
				env
				Env.Local.empty
			)
	in
	match proto.start with
		| Function (t, qvar) ->
			let t, s, r = decl_function_extract_name class_name t qvar in
			let ty = TyFun (t, r, is_instr, args, local_env) in
			let k = sprintf "%s%s%s"
				class_name
				(if class_name = "" then "" else ".")
				s.node
			in
				eprintf "Déclaration de %s : %s\n" k (string_of_ty ty);
				let env' = decl_var (err s) env ty k r in
				let cl' = Env.Local.add k ty cl in
				let envl = Env.push local_env env' in
				let _, instrs =
					type_instr k (get_attr_env k) (envl, []) instr
				in
				let decl =
					Tdeclfun (k, t = TyVoid, args, local_env, List.rev instrs)
				in env', cl', [ decl ]
		| Constructor s   ->
			if class_name = ""
			then err s (sprintf "Constructor of '%s' out of class declaration" s.node)
			else if s.node <> class_name
			then err s (sprintf "'%s' does not inherit from '%s'" class_name s.node)
			else
				let k = sprintf "%s.%s" s.node s.node in
				let ty = TyFun (TyClass class_name, false, false, args, local_env) in
				let env' = decl_var (err s) env ty k false in
				let cl' = Env.Local.add k ty cl in
					eprintf "Déclaration de %s : %s\n" k (string_of_ty ty);
					env', cl', []
		| Qualified_constructor (s1, s2) ->
			let err m = raise (Error (m, s1.start_pos, s2.end_pos)) in
			if class_name <> ""
			then err (sprintf "Unexpected qualification '%s.'" s1.node)
			else if s1.node <> s2.node
			then err (sprintf "'%s' has no constructor for type '%s'" s1.node s2.node)
			else
				let k = sprintf "%s.%s" s1.node s1.node in
				let ty = TyFun (TyClass s1.node, false, true, args, local_env) in
				eprintf "Déclaration de %s : %s\n" k (string_of_ty ty);
				let env' = decl_var err env ty k false in
				let cl' = Env.Local.add k ty cl in
				let envl = Env.push local_env env' in
				let _, instrs =
					type_instr k (get_attr_env k) (envl, []) instr
				in
				let decl =
					Tdeclfun (k, true, args, local_env, List.rev instrs)
				in
					env', cl', [ decl ]



let rec type_class env cl name = function
	| [] -> env, cl
	| (Decl_attr (t, [])) :: q ->
		type_class env cl name q
	| (Decl_attr (t, v :: vars)) :: q ->
		let t', v', r' = decl_var_extract_name t v in
		let k = sprintf "%s.%s" name v'.node in
		eprintf "Déclaration de %s : %s\n" k (string_of_ty t');
		if Env.mem_local k env
		then err v' ("This member has already been declared : " ^ k)
		else
			let env' = Env.add k (t', r') env in
			let cl' = Env.Local.add k t' cl in
				type_class env' cl' name ((Decl_attr (t, vars)) :: q)
	| (Proto proto) :: q ->
		let env', cl', _ = type_proto env cl proto name None in
			type_class env' cl' name q
	| (Virtual_proto proto) :: q -> assert false ; raise TODO


let type_decl (env, classes, decls) = function
	| Decl_vars  (t, vars) ->
		List.fold_left
			(fun env v -> decl_glob_var env t v)
			env
			vars,
		classes,
		decls
	
	| Proto_bloc (proto, bloc) ->
		let env', _, decl = type_proto env (Env.Local.empty) proto "" (Some bloc) in
			env', classes, decl @ decls
	
	| Decl_class c ->
		eprintf "Déclaration de %s : Class\n" c.name;
		let env', cl = type_class env (Env.Local.empty) c.name c.members in
		let classes' = Env.Local.add c.name cl classes in
			env', classes', decls

let typing pAst =
	let env, classes, decls = List.fold_left
		type_decl
		(Env.empty, Env.Local.empty, [])
		pAst.decls
	in {
		declarations = List.rev decls;
		globals      = Env.get_globals env;
		classes      = classes
	}




