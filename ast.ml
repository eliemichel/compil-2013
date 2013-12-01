

type ast = {
		decls    : decl list;
		iostream : bool
	}


and decl =
	| Decl_vars   of (type_ * var list)
	| Decl_class  of decl_class
	| Proto_bloc  of proto * bloc


and var =
	| Ident          of string
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
	| Typed       of type_ * qvar
	| Constructor of string
	| Method      of string * string


and bloc = instruction list


and instruction =
	| Empty
	| Expression of expression
	| Var_init   of type_ * var * var_val option
	| If         of expression * instruction
	| If_else    of expression * instruction * instruction
	| While      of expression * instruction
	| For        of expression list * expression * expression list * instruction
	| Bloc       of bloc
	| Cout       of expr_flow list
	| Return     of expression option

and expr_flow =
	| Expression_in_flow of expression
	| String_in_flow     of string
	| Endl


and var_val =
	| Value    of expression
	| Returned of string * expression list


and expression =
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
	| Simple_qident    of string
	| Namespace_qident of string * string


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


(* ------ *)

let tidentTbl : (string, unit) Hashtbl.t = Hashtbl.create 17




