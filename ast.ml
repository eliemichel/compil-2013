
(* ------ Parsed AST ------ *)

type 'a pos_node = {
	node : 'a;
	start_pos : Lexing.position;
	end_pos : Lexing.position
}

type pAst = {
		decls    : decl list;
		iostream : bool
	}


and decl =
	| Decl_vars   of (type_ * var list)
	| Decl_class  of decl_class
	| Proto_bloc  of proto * bloc


and var =
	| Ident          of string pos_node
	| Pointer_value  of var
	| Address        of var


and qvar =
	| QIdent_in_qvar of qident
	| QPointer_value of qvar
	| QAddress       of qvar


and type_ =
	| Void
	| Int
	| TIdent of string


and decl_class = {
		name    : string;
		supers  : string list;
		members : member list
	}


and member =
	| Decl_vars_in_member of (type_ * var list)
	| Proto               of proto
	| Virtual_proto       of proto


and proto = {
		start : start_proto;
		args  : argument list
	}


and argument = type_ * var


and start_proto =
	| Function    of type_ * qvar
	| Constructor of string pos_node
	| Method      of string * string


and bloc = instruction list


and instruction =
	| Empty
	| Expression of expression
	| Var_init   of type_ * var * var_val option
	| If_else    of expression * instruction * instruction
	| For        of expression list * expression * expression list * instruction
	| Bloc       of bloc
	| Cout       of expr_flow list
	| Return     of expression option

and expr_flow =
	| Expression_in_flow of expression
	| String_in_flow     of string


and var_val =
	| Value    of expression
	| Returned of string * expression list


and expression = expression_node pos_node


and expression_node =
	| This
	| Null
	| Integer     of string
	| QIdent      of qident
	| Dot         of expression * string
	| Arrow       of expression * string
	| Eq          of expression * expression
	| Application of expression * expression list
	| New         of string * expression list
	| Unop        of unop * expression
	| Binop       of binop * expression * expression


and unop =
	| Star
	| IncrLeft
	| DecrLeft
	| IncrRight
	| DecrRight
	| Amp
	| Not
	| Minus
	| Plus


and qident =
	| Simple_qident    of string pos_node
	| Namespace_qident of string pos_node * string pos_node


and binop =
	| Dbleq
	| Neq
	| Lt
	| Leq
	| Gt
	| Geq
	| Add
	| Sub
	| Mult
	| Div
	| Mod
	| And
	| Or


(* ------ Function AST ------ *)


type tAst = {
	declarations : t_decl list;
	globals      : ty Env.Local.t
}


and ty =
	| TyTypeNull
	| TyVoid
	| TyInt
	| TyClass of string
	| TyPointer of ty
	| TyFun of ty * string list * ty Env.Local.t (* bonne idée ou pas ? *)


and t_decl =
	| Tdeclfun of string * string list * ty Env.Local.t * t_instr list

and t_instr =
	| Texpr of t_expr
	| Tdecl of string
	| Tifelse of t_expr * t_instr list * t_instr list
	| Tfor of t_expr list * t_expr * t_expr list * t_instr list
	| Tcout_expr of t_expr
	| Tcout_str of string
	| Treturn of t_expr option
	| Tmalloc of ty Env.Local.t
	| Tfree

and t_expr =
	| Tthis
	| Tnull
	| Tint of string
	| Tvar  of string
	| Tfun of string
	| Tassign of t_expr * t_expr
	| Tcall of t_expr * t_expr list
	(*| Tunop of unop * t_expr*)
	| Tbinop of t_binop * t_expr * t_expr

and t_binop =
	| Tarith of t_binop_arith
	| Tset   of t_binop_set

and t_binop_arith =
	| Tadd
	| Tsub
	| Tmult
	| Tdiv
	| Tmod
	| Tand
	| Tor

and t_binop_set =
	| Teq
	| Tneq
	| Tlt
	| Tleq
	| Tgt
	| Tgeq

(* Lexer hack *)
let tidentTbl : (string, unit) Hashtbl.t = Hashtbl.create 17


exception TODO

let size_of_ty = function
	| TyTypeNull  -> 0
	| TyVoid      -> 0
	| TyInt       -> 4
	| TyClass s   -> raise TODO
	| TyPointer _ -> 4
	| TyFun _     -> 0



