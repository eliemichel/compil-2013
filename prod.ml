open Ast
open Mips
open Format

let is_meth = ref false

let data_i = ref 0
let get_new_data_label () =
	let s = "data_" ^ (string_of_int !data_i) in
	incr data_i; s

let cont_i = ref 0
let get_new_control_label () =
	let s = "cont_" ^ (string_of_int !cont_i) in
	incr cont_i; s

let data = ref []
let bloc_count = ref 0
let global_env : (int * bool * bool) Env.t ref = ref Env.empty (* Une variable globale… pas beau, mais pratique *)
let class_env : int Env.Local.t ref = ref Env.Local.empty (* une autre, parce que la première marche bien finalement *)
let size_env : int Env.Local.t ref = ref Env.Local.empty

let push n = mips [ Arith (Mips.Sub, SP, SP, Oimm n) ]
let pop n = mips [ Arith (Mips.Add, SP, SP, Oimm n) ]

let push_r r = mips [ Sw (r, Areg (0, SP)) ] ++ push 4
let pop_r r = pop 4 ++ mips [ Lw (r, Areg(0, SP)) ]
let peek_r r = mips [ Lw (r, Areg(4, SP)) ]

let push_bloc = push_r FP ++ mips [ Move (FP, SP) ]
let pop_bloc = mips [ Move (SP, FP) ] ++ pop_r FP

let arith_of_binop = function
	| Tadd  -> Add
	| Tsub  -> Sub
	| Tmult -> Mul
	| Tdiv  -> Div
	| Tmod  -> Rem

let set_of_binop = function
	| Teq  -> Eq
	| Tneq -> Ne
	| Tlt  -> Lt
	| Tleq -> Le
	| Tgt  -> Gt
	| Tgeq -> Ge

(*Printf.eprintf "%a\n" (Env.print test_ib) !global_env;*)
let test_ib ff (k, (i,r,b)) =
	Printf.fprintf ff "(%s -> %d, %s, %s)" k i (string_of_bool r) (string_of_bool b)

(*Printf.eprintf "%a\n" (Env.print_local test_t) local_env;*)
let test_t ff (k, (t,r)) =
	Printf.fprintf ff "(%s -> %s, %s)" k (Typer.string_of_ty t) (string_of_bool r)


let decl_local s =
	let t, r, _ = Env.find_local s !global_env in
		global_env := Env.add s (t, r, true) !global_env

let make_addr_env env =
	let aux v (t, r) (addr_env, c) =
		let size = size_of_ty t in
			Env.Local.add v (c, r, false) addr_env,
			c + size
	in
		Env.Local.fold aux env (Env.Local.empty, 0)

let make_addr_env_ordered order env =
	let aux v (addr_env, c) =
		let t, r = Env.Local.find v env in
		let size = size_of_ty t in
			Env.Local.add v (c, r, false) addr_env,
			c + size
	in
		List.fold_right aux order (Env.Local.empty, 0)


let malloc env =
	let addr_env, n = make_addr_env env in
		incr bloc_count;
		global_env := Env.push addr_env !global_env;
		push_bloc ++
		push n

let malloc_ordered order env =
	let addr_env, n = make_addr_env_ordered order env in
		incr bloc_count;
		global_env := Env.push addr_env !global_env;
		push_bloc ++
		push n

let free () =
	decr bloc_count;
	global_env := snd (Env.pop !global_env);
	pop_bloc

let rec search_locals = function
	| 0 -> nop
	| i -> mips [ Lw (FP, Areg (4, FP)) ] ++ search_locals (i - 1)

let rec pop_n_bloc = function
	| 0 -> nop
	| i -> pop_bloc ++ (pop_n_bloc (i - 1))

let rec compile_expr_g = function
	| Tthis | Tnull | Tint _ | Tassign _ | Trefinit _ | Tbinop  _
	| Tfun _ | Tlocalmeth _ | Tnot _ | Tincrleft _ | Tdecrleft _ | Tincrright _
	| Tdecrright _ | Tgetaddr _ | Tcall (_,false,_) | Tnew _ | Tmeth _
		-> assert false
	| Tcall (f, true, args) ->
		let p1 =
			malloc Env.Local.empty
		in
		let p2 =
			compile_args args ++
			compile_expr f ++
			pop_r A0 ++
			mips [ Jalr A0 ] ++
			pop_r A0
		in
		let p3 =
			free () ++
			push_r A0
		in p1 ++ p2 ++ p3
	| Tvar  s ->
		let env = Env.of_bool_env !global_env in
		let (addr, r), iter = Env.find_and_localize s env in
			push_r FP ++
			search_locals iter ++
			mips [
				if r
				then Lw (A0, Areg (-addr, FP))
				else Arith (Mips.Sub, A0, FP, Oimm addr)
			] ++
			pop_r FP ++
			push_r A0
	| Tlocalattr s ->
		let off = Env.Local.find s !class_env in
			mips [ Arith (Mips.Add, A0, T0, Oimm off) ] ++
			push_r A0
	| Tattr (e, s) ->
		let off = Env.Local.find s !class_env in
			push_r T0 ++
			compile_expr e ++
			pop_r T0 ++
			mips [ Arith (Mips.Add, A0, T0, Oimm off) ] ++
			pop_r T0 ++
			push_r A0
	| Tderef e ->
		compile_expr e


and compile_args args =
	let aux (arg, r) =
		if r
		then compile_expr_g arg
		else compile_expr   arg
	in List.fold_right (++) (List.map aux args) nop

and compile_exprs i = List.fold_right (++) (List.map compile_expr i) nop

and compile_expr = function
	| Tthis  ->
		mips [
			Sw (T0, Alab("this"));
			La (A0, "this");
		] ++
		push_r A0
	| Tnull  -> mips [ Li (A0, "0") ] ++ push_r A0
	| Tint i -> mips [ Li (A0,  i ) ] ++ push_r A0
	| Tvar s ->
		let env = Env.of_bool_env !global_env in
		let (addr, r), iter = Env.find_and_localize s env in
			(*eprintf "%s addr %d iter %d@." s addr iter;
			eprintf "%a@." (Env.print test_ib) !global_env;*)
			push_r FP ++
			search_locals iter ++
			mips [ Lw (A0, Areg (-addr, FP)) ] ++
			(if r then mips [ Lw (A0, Areg (0, A0)) ] else nop) ++
			pop_r FP ++
			push_r A0
	| Tfun s ->
		mips [ La (A0, "fun_" ^ s) ] ++
		push_r A0
	| Tlocalattr s ->
		let off = Env.Local.find s !class_env in
			mips [ Lw (A0, Areg (off, T0)) ] ++
			push_r A0
	| Tlocalmeth s ->
		let off = Env.Local.find s !class_env in
			mips [ Lw (A0, Areg (0, T0)) ] ++
			mips [ Lw (A0, Areg (off, A0)) ] ++
			push_r A0
	| Tattr (e, s) ->
		let off = Env.Local.find s !class_env in
			push_r T0 ++
			compile_expr e ++
			pop_r T0 ++
			mips [ Lw (A0, Areg (off, T0)) ] ++
			pop_r T0 ++
			push_r A0
	| Tmeth (e, s) ->
		is_meth := true;
		let off = Env.Local.find s !class_env in
			push_r T0 ++
			compile_expr e ++
			pop_r T0 ++
			mips [
				Lw (A0, Areg (0, T0));
				Lw (A0, Areg (off, A0));
			] ++
			push_r A0
	| Tassign (e1, e2) ->
		compile_expr_g e1 ++
		compile_expr   e2 ++
		pop_r A0 ++
		pop_r A1 ++
		mips [ Sw (A0, Areg (0, A1)) ] ++
		push_r A0
	| Trefinit (s, e) ->
		let env = Env.of_bool_env !global_env in
		let (addr, r), iter = Env.find_and_localize s env in
			push_r FP ++
			search_locals iter ++
			mips [ Arith (Mips.Sub, A0, FP, Oimm addr) ] ++
			pop_r FP ++
			push_r A0 ++
			compile_expr_g e ++
			pop_r A0 ++
			pop_r A1 ++
			mips [ Sw (A0, Areg (0, A1)) ] ++
			push_r A0
	| Tcall (f, r, args) ->
		let p1 =
			malloc Env.Local.empty
		in
		let p2 =
			compile_args args ++
			compile_expr f ++
			pop_r A0 ++
			mips [ Jalr A0 ] ++
			pop_r A0 ++
			(if !is_meth then pop_r T0 else nop)
		in
		let p3 =
			free () ++
			(if r then mips [ Lw (A0, Areg (0, A0)) ] else nop) ++
			push_r A0
		in
			is_meth := false;
			p1 ++ p2 ++ p3 (* 'astuce' pour forcer l'ordre d'évaluation (nécessaire
		                     à cause de l'utilisation d'effets de bord -- d'un
		                     environnement global) *)
	| Tnew (s, args) ->
		let k = sprintf "%s.%s" s s in
		(* +4 : l'adresse en plus est la variable 'contenant' l'objet *)
		let size = string_of_int (4 + Env.Local.find s !size_env) in
		let p1 = 
			push_r T0 ++
			mips [
				Li (A0, size);
				Li (V0, "9");
				Syscall;
				Arith (Mips.Add, T0, V0, Oimm 4);
				Sw (T0, Areg(0, V0));
			] ++
			push_r V0 ++
			mips [
				La (A0, "descr_" ^ s);
				Sw (A0, Areg(0, T0))
			] ++
			malloc Env.Local.empty
		in
		let p2 =
			compile_args args ++
			mips [ Jal ("fun_" ^ k) ] ++
			pop_r A0
		in
		let p3 =
			free () ++
			pop_r A0 ++
			pop_r T0 ++
			push_r A0
		in p1 ++ p2 ++ p3
	| Tbinop (Tlazy op, e1, e2) ->
		let cont = get_new_control_label () in
			compile_expr e1 ++
			peek_r A0 ++
			mips [ if op = Tand
				then Beqz (A0, cont)
				else Bnez (A0, cont)
			] ++
			pop 4 ++
			compile_expr e2 ++
			mips [ Label cont ]
	| Tbinop (op, e1, e2) ->
		compile_expr e1 ++
		compile_expr e2 ++
		pop_r A1 ++
		pop_r A0 ++
		mips [ match op with
			| Tarith o -> Arith (arith_of_binop o, A0, A0, Oreg A1)
			| Tset   o -> Set   (set_of_binop   o, A0, A0, Oreg A1)
			| _ -> assert false
		] ++
		push_r A0
	| Tnot e ->
		compile_expr e ++
		pop_r A0 ++
		mips [ Not (A0, A0)] ++
		push_r A0
	| Tincrleft e ->
		compile_expr_g e ++
		compile_expr   e ++
		pop_r A0 ++
		mips [ Arith (Add, A0, A0, Oimm 1) ] ++
		pop_r A1 ++
		mips [ Sw (A0, Areg (0, A1)) ] ++
		push_r A0
	| Tdecrleft e -> (* Recopiage de code… Pas beau… *)
		compile_expr_g e ++
		compile_expr   e ++
		pop_r A0 ++
		mips [ Arith (Sub, A0, A0, Oimm 1) ] ++
		pop_r A1 ++
		mips [ Sw (A0, Areg (0, A1)) ] ++
		push_r A0
	| Tincrright e ->
		compile_expr_g e ++
		compile_expr   e ++
		pop_r A0 ++
		mips [ Arith (Add, A2, A0, Oimm 1) ] ++
		pop_r A1 ++
		mips [ Sw (A2, Areg (0, A1)) ] ++
		push_r A0
	| Tdecrright e ->
		compile_expr_g e ++
		compile_expr   e ++
		pop_r A0 ++
		mips [ Arith (Sub, A2, A0, Oimm 1) ] ++
		pop_r A1 ++
		mips [ Sw (A2, Areg (0, A1)) ] ++
		push_r A0
	| Tgetaddr e ->
		compile_expr_g e
	| Tderef e ->
		compile_expr e ++
		pop_r A0 ++
		mips [ Lw (A0, Areg (0, A0)) ] ++
		push_r A0


let rec compile_instrs i = List.fold_right (++) (List.map compile_instr i) nop

and compile_instr = function
	| Texpr e -> compile_expr e ++ pop 4
	| Tdecl s -> decl_local s ; nop
	| Tifelse (test, i1, i2) ->
		let cont1 = get_new_control_label () in
		let cont2 = get_new_control_label () in
			compile_expr test ++
			pop_r A0 ++
			mips [ Beqz (A0, cont1) ] ++
			compile_instrs i1 ++
			mips [
				J cont2;
				Label cont1
			] ++
			compile_instrs i2 ++
			mips [ Label cont2 ]
	| Tfor (before, test, iter, instr) ->
		let cont1 = get_new_control_label () in
		let cont2 = get_new_control_label () in
			compile_exprs before ++
			mips [ Label cont1 ] ++
			compile_expr test ++
			pop_r A0 ++
			mips [ Beqz (A0, cont2) ] ++
			compile_instrs instr ++
			compile_exprs iter ++
			mips [
				J cont1;
				Label cont2
			]
	| Tcout_str  s ->
		let label = get_new_data_label () in
			data := (Asciiz (label, s)) :: !data;
			mips [
				La (A0, label);
				Li (V0, "4");
				Syscall
			]
	| Tcout_expr e ->
		compile_expr e ++
		pop_r A0 ++
		mips [ Li (V0, "1") ; Syscall ]
	| Treturn None ->
		pop_n_bloc (!bloc_count - 2) ++
		mips [ Jr RA ]
	| Treturn (Some (e, r)) ->
		(if r
			then compile_expr_g e
			else compile_expr   e
		) ++
		pop_r A0 ++
		pop_n_bloc (!bloc_count - 2) ++
		pop_r RA ++
		push_r A0 ++
		mips [ Jr RA ]
	| Tconstr (e, s, args) ->
		let k = sprintf "%s.%s" s s in
		let size = string_of_int (Env.Local.find s !size_env) in
		let p0 =
			push_r T0 ++
			compile_expr_g e
		in
		let p1 =
			mips [
				Li (A0, size);
				Li (V0, "9");
				Syscall;
				Move (T0, V0);
				La (A0, "descr_" ^ s);
				Sw (A0, Areg(0, T0))
			] ++
			malloc Env.Local.empty
		in
		let p2 =
			compile_args args ++
			mips [ Jal ("fun_" ^ k) ] ++
			pop_r A0
		in
		let p3 =
			free () ++
			pop_r A1 ++
			mips [ Sw (T0, Areg(0, A1)) ] ++
			pop_r T0
		in p0 ++ p1 ++ p2 ++ p3		
	| Tmalloc env  -> malloc env
	| Tfree        -> free ()


let compile_decl = function
	| Tdeclfun (s, v, args, local_env, instrs) ->
		ignore (malloc_ordered args local_env);
		global_env := Env.decl !global_env;
		let body =
			compile_instrs instrs ++
			pop_n_bloc (!bloc_count - 2) ++
			pop_r RA ++
			(if not v then push_r A0 else nop) ++
			mips [ Jr RA ]
		in
		ignore (free ());
			mips [ Label ("fun_" ^ s) ] ++
			push_r RA ++
			body

let make_descr cl env (senv, cenv, l) =
	let count_attr = ref 0 in
	let count_meth = ref (-4) in
	
	let aux k t (e, l) = match t with
		| TyFun _ ->
			count_meth := !count_meth + 4;
			Env.Local.add k !count_meth e,
			(Waddr ("fun_" ^ k)) :: l
		| _       ->
			count_attr := !count_attr + 4;
			Env.Local.add k !count_attr e,
			l
	in
	let cenv', descr = Env.Local.fold aux env (cenv, []) in
	let senv' = Env.Local.add cl (!count_attr + 4) senv in
		senv', cenv', Word ("descr_" ^ cl, List.rev descr) :: l
(*eprintf "%a\n" (Env.print_local (fun ff (k,i) -> fprintf ff "(%s -> %d)" k i)) cenv;*)

let compile tAst =
	let this = Word ("this", [ Wint 0 ]) in
	(*  ^^^^ petit hack pas beau pour avoir une variable vers laquelle pointer *)
	let senv, cenv, descr = Env.Local.fold
		make_descr
		tAst.classes
		(Env.Local.empty,Env.Local.empty, [ this ])
	in
	class_env := cenv;
	size_env := senv;
	let pre =
		malloc tAst.globals ++
		compile_expr (Tcall (Tfun "main", false, []))
	in
	global_env := Env.decl !global_env;
	let code = List.map compile_decl tAst.declarations in
	let fonctions = List.fold_right (++) code nop in
	let post = free () ++ mips [ Li (V0, "10") ; Syscall ] in
	
	
	{
		text = pre ++ post ++ fonctions;
		data = descr @ !data
	}







