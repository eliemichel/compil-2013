open Ast
open Mips

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
	| Tfun _ | Tnot _ | Tincrleft _ | Tdecrleft _ | Tincrright _ | Tdecrright _
	| Tgetaddr _ | Tcall (_,false,_)
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
	| Tdereference e ->
		compile_expr e


and compile_args args =
	let aux (arg, r) =
		if r
		then compile_expr_g arg
		else compile_expr   arg
	in List.fold_right (++) (List.map aux args) nop

and compile_exprs i = List.fold_right (++) (List.map compile_expr i) nop

and compile_expr = function
	| Tthis  -> raise TODO
	| Tnull  -> mips [ Li (A0, "0") ] ++ push_r A0
	| Tint i -> mips [ Li (A0,  i ) ] ++ push_r A0
	| Tvar s ->
		let env = Env.of_bool_env !global_env in
		let (addr, r), iter = Env.find_and_localize s env in
			(*Printf.eprintf "%s addr %d iter %d\n" s addr iter;
			Printf.eprintf "%a\n" (Env.print test_ib) !global_env;*)
			push_r FP ++
			search_locals iter ++
			mips [ Lw (A0, Areg (-addr, FP)) ] ++
			(if r then mips [ Lw (A0, Areg (0, A0)) ] else nop) ++
			pop_r FP ++
			push_r A0
	| Tfun s ->
		mips [ La (A0, "fun_" ^ s) ] ++
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
			pop_r A0
		in
		let p3 =
			free () ++
			(if r then mips [ Lw (A0, Areg (0, A0)) ] else nop) ++
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
	| Tdereference e ->
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

let compile tAst =
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
		data = !data
	}







