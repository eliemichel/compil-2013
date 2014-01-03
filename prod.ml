open Ast
open Mips

let globals = ref (Hashtbl.create 0)

let data_i = ref 0
let rec get_new_label () =
	let s = "data_" ^ (string_of_int !data_i) in
	incr data_i;
	if Hashtbl.mem !globals s then get_new_label () else s

let cont_i = ref 0
let get_new_control () =
	let s = "cont_" ^ (string_of_int !cont_i) in
	incr cont_i; s

let data = ref []
let env = ref Env.empty

let push n = mips [ Arith (Mips.Sub, SP, SP, Oimm n) ]
let pop n = mips [ Arith (Mips.Add, SP, SP, Oimm n) ]

let push_r r = mips [ Sw (r, Areg (0, SP)) ] ++ push 4
let pop_r r = pop 4 ++ mips [ Lw (r, Areg(0, SP)) ]

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



let make_addr_env env =
	let c = ref 0 in
	let aux t =
		let c' = !c in
			c := !c + size_of_ty t;
			c'
	in
		Env.Local.map aux env, !c

let rec search_locals = function
	| 0 -> nop
	| i -> mips [ Lw (FP, Areg (0, FP)) ] ++ search_locals (i - 1)

let rec compile_expr_g = function
	| Tthis | Tnull | Tint _ | Tassign _ | Tbinop  _ -> assert false
	| Tglobal s -> mips [ La (A0, s) ] ++ push_r A0
	| Tlocal  s ->
		let addr, iter = Env.find_and_localize s !env in
			push_r FP ++
			search_locals iter ++
			mips [ Arith (Mips.Sub, A0, FP, Oimm addr) ] ++
			pop_r FP ++
			push_r A0

let rec compile_expr = function
	| Tthis     -> raise TODO
	| Tnull     -> mips [ Li (A0, "0") ] ++ push_r A0
	| Tint i    -> mips [ Li (A0,  i ) ] ++ push_r A0
	| Tglobal s -> mips [ Lw (A0, Alab s) ] ++ push_r A0
	| Tlocal  s ->
		let addr, iter = Env.find_and_localize s !env in
			push_r FP ++
			search_locals iter ++
			mips [ Lw (A0, Areg (-addr, FP)) ] ++
			pop_r FP ++
			push_r A0
	| Tassign (e1, e2) ->
		compile_expr_g e1 ++
		compile_expr   e2 ++
		pop_r A0 ++
		pop_r A1 ++
		mips [ Sw (A0, Areg (0, A1)) ] ++
		push_r A0
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


let compile_exprs i = List.fold_right (++) (List.map compile_expr i) nop

let rec compile_instrs i = List.fold_right (++) (List.map compile_instr i) nop

and compile_instr = function
	| Texpr e -> compile_expr e ++ pop 4
	| Tifelse (test, i1, i2) ->
		let cont1 = get_new_control () in
		let cont2 = get_new_control () in
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
		let cont1 = get_new_control () in
		let cont2 = get_new_control () in
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
		let label = get_new_label () in
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
	| Treturn    i ->
		mips [ Li (V0, "10") ; Syscall ]
	| Tmalloc  local_env ->
		let addr_env, n = make_addr_env local_env in
			env := Env.push addr_env !env;
			push_r FP ++
			mips [ Move (FP, SP) ] ++
			push n
	| Tfree ->
		env := snd (Env.pop !env);
		mips [ Move (SP, FP) ] ++
		pop_r FP


let compile_decl = function
	| Tvar (t, s) -> raise TODO
	| Tfun (t, s, instrs) ->
		mips [ Label s ] ++
		compile_instrs instrs

let compile tAst =
	globals := tAst.globals;
	let code = List.map compile_decl tAst.declarations in
	let code = List.fold_right (++) code nop in
	
	let data =
		Hashtbl.fold (fun x _ l -> Word (x, [Wint 1]) :: l) !globals !data
	in
	
	{
		text = code;
		data = data
	}







