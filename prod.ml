open Ast
open Mips

let data_i = ref 0
let get_new_data_label () =
	let s = "data_" ^ (string_of_int !data_i) in
	incr data_i; s

let fun_labels : (string, unit) Hashtbl.t = Hashtbl.create 17

let cont_i = ref 0
let rec get_new_control_label () =
	let s = "cont_" ^ (string_of_int !cont_i) in
	incr cont_i;
	if Hashtbl.mem fun_labels s then get_new_control_label () else s

let data = ref []
let bloc_count = ref 0
let global_env = ref Env.empty (* Une variable globale… pas beau, mais pratique *)

let push n = mips [ Arith (Mips.Sub, SP, SP, Oimm n) ]
let pop n = mips [ Arith (Mips.Add, SP, SP, Oimm n) ]

let push_r r = mips [ Sw (r, Areg (0, SP)) ] ++ push 4
let pop_r r = pop 4 ++ mips [ Lw (r, Areg(0, SP)) ]

let push_bloc = push_r FP ++ mips [ Move (FP, SP) ]
let pop_bloc = mips [ Move (SP, FP) ] ++ pop_r FP

let arith_of_binop = function
	| Tadd  -> Add
	| Tsub  -> Sub
	| Tmult -> Mul
	| Tdiv  -> Div
	| Tmod  -> Rem
	| Tand  -> And
	| Tor   -> Or

let set_of_binop = function
	| Teq  -> Eq
	| Tneq -> Ne
	| Tlt  -> Lt
	| Tleq -> Le
	| Tgt  -> Gt
	| Tgeq -> Ge

(*Printf.eprintf "%a\n" (Env.print test_ib) !global_env;*)
let test_ib ff (k, (i,b)) =
	Printf.fprintf ff "(%s -> %d, %s)" k i (string_of_bool b)

(*Printf.eprintf "%a\n" (Env.print_local test_t) local_env;*)
let test_t ff (k, t) =
	Printf.fprintf ff "(%s -> %s)" k (Typer.string_of_ty t)


let decl_local s =
	let t, _ = Env.find_local s !global_env in
		global_env := Env.add s (t, true) !global_env

let make_addr_env env =
	let aux k t (addr_env, c) =
		let size = size_of_ty t in
			Env.Local.add k (c, false) addr_env,
			c + size
	in
		Env.Local.fold aux env (Env.Local.empty, 0)

let make_addr_env_ordered order env =
	let aux k (addr_env, c) =
		let t = Env.Local.find k env in
		let size = size_of_ty t in
			Env.Local.add k (c, false) addr_env,
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
	| Tthis | Tnull | Tint _ | Tassign _ | Tcall _ | Tbinop  _ | Tfun _
		-> assert false
	| Tvar  s ->
		let env = Env.of_bool_env !global_env in
		let addr, iter = Env.find_and_localize s env in
			(*Printf.eprintf "ref %s addr %d iter %d\n" s addr iter;
			Printf.eprintf "%a\n" (Env.print test_ib) !global_env;*)
			push_r FP ++
			search_locals iter ++
			mips [ Arith (Mips.Sub, A0, FP, Oimm addr) ] ++
			pop_r FP ++
			push_r A0



let rec compile_exprs i = List.fold_right (++) (List.map compile_expr i) nop

and compile_expr = function
	| Tthis  -> raise TODO
	| Tnull  -> mips [ Li (A0, "0") ] ++ push_r A0
	| Tint i -> mips [ Li (A0,  i ) ] ++ push_r A0
	| Tvar s ->
		let env = Env.of_bool_env !global_env in
		let addr, iter = Env.find_and_localize s env in
			(*Printf.eprintf "%s addr %d iter %d\n" s addr iter;
			Printf.eprintf "%a\n" (Env.print test_ib) !global_env;*)
			push_r FP ++
			search_locals iter ++
			mips [ Lw (A0, Areg (-addr, FP)) ] ++
			pop_r FP ++
			push_r A0
	| Tfun s ->
		mips [ La (A0, s) ] ++
		push_r A0
	| Tassign (e1, e2) ->
		compile_expr_g e1 ++
		compile_expr   e2 ++
		pop_r A0 ++
		pop_r A1 ++
		mips [ Sw (A0, Areg (0, A1)) ] ++
		push_r A0
	| Tcall (f, args) ->
		let p1 =
			malloc Env.Local.empty
		in
		let p2 =
			compile_exprs args ++
			compile_expr f ++
			pop_r A0 ++
			mips [ Jalr A0 ] ++
			pop_r A0
		in
		let p3 =
			free () ++
			push_r A0
		in p1 ++ p2 ++ p3
	| Tbinop (op, e1, e2) ->
		compile_expr e1 ++
		compile_expr e2 ++
		pop_r A1 ++
		pop_r A0 ++
		mips [ match op with
			| Tarith o -> Arith (arith_of_binop o, A0, A0, Oreg A1)
			| Tset   o -> Set   (set_of_binop   o, A0, A0, Oreg A1)
		] ++
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
	| Treturn (Some e) ->
		compile_expr e ++
		pop_r A0 ++
		pop_n_bloc (!bloc_count - 2) ++
		pop_r RA ++
		push_r A0 ++
		mips [ Jr RA ]
	| Tmalloc env  -> malloc env
	| Tfree        -> free ()


let compile_decl = function
	| Tdeclfun (s, args, local_env, instrs) ->
		ignore (malloc_ordered args local_env);
		global_env := Env.decl !global_env;
		let body = compile_instrs instrs in
		ignore (free ());
			mips [ Label s ] ++
			push_r RA ++
			body

let compile tAst =
	Env.Local.iter
		(fun key _ -> Hashtbl.replace fun_labels key ())
		tAst.globals;
	let pre =
		malloc tAst.globals ++
		compile_expr (Tcall (Tfun "main", []))
	in
	global_env := Env.decl !global_env;
	let code = List.map compile_decl tAst.declarations in
	let fonctions = List.fold_right (++) code nop in
	let post = free () ++ mips [ Li (V0, "10") ; Syscall ] in
	
	{
		text = pre ++ post ++ fonctions;
		data = !data
	}







