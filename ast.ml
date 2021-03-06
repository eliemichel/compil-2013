
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
	| Decl_attr     of (type_ * var list)
	| Proto         of proto
	| Virtual_proto of proto


and proto = {
		start : start_proto;
		args  : argument list
	}


and argument = type_ * var


and start_proto =
	| Function    of type_ * qvar
	| Constructor of string pos_node
	| Qualified_constructor of string pos_node * string pos_node


and bloc = instruction list


and instruction =
	| Empty
	| Expression of expression
	| Var_init   of type_ * var * var_val option
	| If_else    of expression * instruction * instruction
	| For        of expression list * expression * expression list * instruction
	| Bloc       of bloc
	| Cout       of expr_flow list
	| Return     of expression option pos_node

and expr_flow =
	| Expression_in_flow of expression
	| String_in_flow     of string


and var_val =
	| Value    of expression
	| Returned of (string * expression list) pos_node


and expression = expression_node pos_node


and expression_node =
	| This
	| Null
	| Integer     of string
	| QIdent      of qident
	| Dot         of expression * string
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
	globals      : (ty * bool) Env.Local.t;
	classes      : ty Env.Local.t Env.Local.t
}


and ty =
	| TyTypeNull
	| TyVoid
	| TyInt
	| TyClass   of string
	| TyPointer of ty
	| TyFun     of tfun

and tfun =
	ty *      (* type de retour           *)
	bool *    (* si le retour est une ref *)
	bool *    (* si le corps de la fonction est connu (ou si seul le prototype a été lu) *)
	string list *           (* noms des arguments ordonnés       *)
	(ty * bool) Env.Local.t (* environnement local à la fonction *)

and t_decl =
	(* nom, si la fonction est de type void, arguments, environnement local,
	liste des instructions du corps de la fonction *)
	| Tdeclfun of string * bool * string list * (ty * bool) Env.Local.t * t_instr list

and t_instr =
	| Texpr of t_expr
	| Tdecl of string
	| Tifelse of t_expr * t_instr list * t_instr list
	| Tfor of t_expr list * t_expr * t_expr list * t_instr list
	| Tcout_expr of t_expr
	| Tcout_str of string
	| Treturn of (t_expr * bool) option
	| Tmalloc of (ty * bool) Env.Local.t
	| Tconstr of t_expr * string * (t_expr * bool) list
	| Tfree

and t_expr =
	| Tthis
	| Tnull
	| Tint       of string
	| Tvar       of string
	| Tfun       of string
	| Tlocalattr of string
	| Tlocalmeth of string
	| Tattr      of t_expr * string
	| Tmeth      of t_expr * string
	| Tassign    of t_expr * t_expr
	| Trefinit   of string * t_expr
	| Tcall      of t_expr * bool * (t_expr * bool) list (* bool : si le passage se fait par référence *)
	| Tnew       of string * (t_expr * bool) list
	| Tbinop     of t_binop * t_expr * t_expr
	| Tnot       of t_expr
	| Tincrleft  of t_expr
	| Tdecrleft  of t_expr
	| Tincrright of t_expr
	| Tdecrright of t_expr
	| Tgetaddr   of t_expr
	| Tderef     of t_expr

and t_binop =
	| Tlazy  of t_binop_lazy
	| Tarith of t_binop_arith
	| Tset   of t_binop_set

and t_binop_lazy =
	| Tand
	| Tor

and t_binop_arith =
	| Tadd
	| Tsub
	| Tmult
	| Tdiv
	| Tmod

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

let rec size_of_ty = function
	| TyTypeNull      -> 0
	| TyVoid          -> 0
	| TyInt           -> 4
	| TyClass s       -> 4 (* les objets sont tous sur le tas *)
	| TyPointer _     -> 4
	| TyFun _         -> 0



